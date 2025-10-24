# ==========================
# Ingestão de exercícios (.qmd) -> D1 (via Worker) — MCQ-only (estável)
# - Processa UMA pasta por vez (defina QMD_DIR abaixo, ex.: "module1/mcq")
# - Upserta templates (MCQ) e publica answer-keys (MCQ)
# - NÃO usa instances, NÃO usa seeds, NÃO processa TF/NUM/Long
# ==========================

# ======== CONFIGURAÇÃO MÍNIMA ========
QMD_DIR <- "module9/mcq"  # <<< ajuste a pasta com seus .qmd de MCQ

ENDPOINT_BASE       <- "https://course-chat.hcmrtns.workers.dev"
ENDPOINT_ANSWER_KEY <- paste0(ENDPOINT_BASE, "/exercises/answer-key")
#ENDPOINT_TEMPL      <- paste0(ENDPOINT_BASE, "/exercises/templates")
PING_URL            <- paste0(ENDPOINT_BASE, "/admin/ping")
ENDPOINT_BANK       <- paste0(ENDPOINT_BASE, "/selfquiz/bank")

ADMIN_TOKEN_ENV     <- "ADMIN_TOKEN"     # defina antes: Sys.setenv(ADMIN_TOKEN = "seu_token")
 
# Política de lotes/pausas
BANK_BATCH_SIZE   <- 10L   # nº de questões por POST /selfquiz/bank
BANK_PAUSE_MS     <- 400L   # pausa entre lotes do bank

AK_FILES_BATCH    <- 20L    # nº de arquivos .qmd por “leva” de answer-keys
AK_PAUSE_MS       <- 250L   # pausa entre levas de answer-keys


# ======== PACOTES ========
ensure_pkg <- function(pkgs) for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
ensure_pkg(c("fs","xml2","stringr","jsonlite","httr2","digest"))

library(fs); library(xml2); library(stringr)
library(jsonlite); library(httr2); library(digest)

`%||%` <- function(a,b) if (is.null(a) || is.na(a)) b else a

# ======== SANITY: TOKEN & HOST ========
admin_token <- Sys.getenv(ADMIN_TOKEN_ENV, unset = NA)
if (is.na(admin_token) || admin_token == "") stop(sprintf("Variável de ambiente %s não definida.", ADMIN_TOKEN_ENV))
try({
  ping <- request(PING_URL) |>
    req_headers("x-admin-token" = admin_token) |>
    req_options(ipresolve = 1L) |>
    req_timeout(20) |>
    req_perform() |>
    resp_body_json()
  cat("[admin/ping] ok — host:", ping$host, "\n")
}, silent = TRUE)

# ======== LISTAGEM DE ARQUIVOS ========
if (!dir_exists(QMD_DIR)) stop("Pasta não encontrada: ", QMD_DIR)
FILES <- fs::dir_ls(QMD_DIR, recurse = TRUE, glob = "*.qmd", type = "file")
if (!length(FILES)) stop("Nenhum .qmd encontrado em: ", QMD_DIR)
cat("Arquivos encontrados em", QMD_DIR, "→", length(FILES), "arquivos .qmd\n")

# ======== HELPERS ========
norm_path <- function(p, mustWork = TRUE) normalizePath(p, winslash = "/", mustWork = mustWork)

chunk_list <- function(vec, size = 10L){
  n <- length(vec); if (!n) return(list())
  size <- max(1L, as.integer(size))
  idx <- split(seq_len(n), ceiling(seq_len(n)/size))
  lapply(idx, function(i) vec[i])
}

derive_question_id <- function(qmd_path){
  tolower(fs::path_ext_remove(fs::path_file(qmd_path)))
}

derive_module_from_qid <- function(qid){
  m <- stringr::str_match(qid, "^(module\\d+)")[,2]
  if (!is.na(m) && nzchar(m)) return(m)
  NA_character_
}

derive_module_fallback_from_path <- function(p){
  m <- stringr::str_match(tolower(p), "(module\\d+)")[,2]
  if (!is.na(m) && nzchar(m)) return(m)
  cand <- basename(dirname(dirname(p)))
  if (grepl("^module\\d+$", tolower(cand))) return(tolower(cand))
  ""
}

version_hash_of_qmd <- function(qmd_path) {
  txt <- readLines(qmd_path, warn = FALSE)
  norm <- gsub("\\s+", " ", trimws(txt))
  digest(paste(norm, collapse = "\n"), algo = "sha256")
}

render_qmd_to_html <- function(qmd_path, out_dir) {
  fs::dir_create(out_dir, recurse = TRUE)
  qmd_norm <- norm_path(qmd_path, mustWork = TRUE)
  out_norm <- norm_path(out_dir, mustWork = FALSE)
  cmd  <- "quarto"
  args <- c("render", qmd_norm, "--to", "html", "--output-dir", out_norm)
  status <- tryCatch(system2(cmd, args, stdout = TRUE, stderr = TRUE),
                     error = function(e) as.character(e))
  html_name <- fs::path_ext_set(fs::path_file(qmd_path), "html")
  candidate <- fs::path(out_norm, html_name)
  if (fs::file_exists(candidate)) return(candidate)
  htmls <- fs::dir_ls(out_norm, recurse = TRUE, type = "file", glob = "*.html")
  htmls <- htmls[fs::path_file(htmls) == html_name]
  if (length(htmls) == 1) return(htmls[[1]])
  stop("Falha ao renderizar HTML (verifique Quarto/arquivo): ", qmd_path,
       "\nSaída:\n", paste(status, collapse = "\n"))
}

# ======== PARSER DE KEYS — MCQ-only ========
extract_items_from_html <- function(html_path) {
  doc <- read_html(html_path)
  items <- list()
  
  # MCQ via <form data-correct-answer="A"> com inputs type=radio
  forms_with_correct <- xml_find_all(doc, ".//form[@data-correct-answer]")
  if (length(forms_with_correct) > 0) {
    for (frm in forms_with_correct) {
      radios <- xml_find_all(frm, ".//input[@type='radio']")
      if (length(radios) == 0) next
      
      corr_letter <- toupper(trimws(xml_attr(frm, "data-correct-answer") %||% ""))
      if (!nzchar(corr_letter)) next
      if (!corr_letter %in% c("A","B","C","D","E")) next
      
      form_id <- xml_attr(frm, "id") %||% ""
      r_ids   <- xml_attr(radios, "id")
      r_ids   <- r_ids[!is.na(r_ids) & nzchar(r_ids)]
      qid_from_radio_id <- if (length(r_ids) > 0) sub("_[A-Ea-e]$", "", r_ids[1]) else ""
      r_name <- xml_attr(radios[[1]], "name") %||% ""
      
      qid <- if      (nzchar(form_id))           form_id
      else if (nzchar(qid_from_radio_id)) qid_from_radio_id
      else if (nzchar(r_name))            r_name
      else paste0("MCQ_", digest::digest(xml_path(frm)))
      
      items[[length(items)+1]] <- list(
        question_id   = qid,
        qtype         = "mcq",
        correct_value = corr_letter,
        tolerance     = NA_real_
      )
    }
  }
  
  if (length(items) == 0) {
    return(data.frame(question_id=character(), qtype=character(),
                      correct_value=character(), tolerance=double(),
                      stringsAsFactors = FALSE))
  }
  
  df <- do.call(rbind.data.frame, lapply(items, as.data.frame))
  df <- df[!duplicated(df$question_id), ]
  rownames(df) <- NULL
  df$qtype <- "mcq"
  df$correct_value <- as.character(df$correct_value)
  df$tolerance[] <- NA_real_
  df
}
# Extrai enunciado e alternativas A..E do mesmo <form data-correct-answer="...">
extract_bank_from_html <- function(html_path) {
  doc <- read_html(html_path)
  out <- list()
  
  forms <- xml_find_all(doc, ".//form[@data-correct-answer]")
  if (length(forms) == 0) return(out)
  
  get_text <- function(node) {
    txt <- xml_text(node, trim = TRUE)
    gsub("\\s+", " ", txt)
  }
  
  for (frm in forms) {
    # Heurística para QID (mesma do extract_items_from_html)
    radios <- xml_find_all(frm, ".//input[@type='radio']")
    if (length(radios) == 0) next
    form_id <- xml_attr(frm, "id") %||% ""
    r_ids   <- xml_attr(radios, "id")
    r_ids   <- r_ids[!is.na(r_ids) & nzchar(r_ids)]
    qid_from_radio_id <- if (length(r_ids) > 0) sub("_[A-Ea-e]$", "", r_ids[1]) else ""
    r_name <- xml_attr(radios[[1]], "name") %||% ""
    qid <- if      (nzchar(form_id))           form_id
    else if (nzchar(qid_from_radio_id)) qid_from_radio_id
    else if (nzchar(r_name))            r_name
    else paste0("MCQ_", digest::digest(xml_path(frm)))
    
    # Enunciado: tenta legend/h3/h4/p (primeiro que existir)
    stem_node <- xml_find_first(
      frm,
      ".//*[self::legend or self::h1 or self::h2 or self::h3 or self::h4 or self::strong or self::p][1]"
    )
    stem <- get_text(stem_node) %||% ""
    
    # Alternativas: tenta pegar texto do <label> associado a cada input
    get_opt_text <- function(inp) {
      id <- xml_attr(inp, "id") %||% ""
      # 1) label[@for=id]
      if (nzchar(id)) {
        lab <- xml_find_first(frm, sprintf(".//label[@for='%s']", id))
        if (!is.na(lab)) {
          t <- get_text(lab)
          if (nzchar(t)) return(t)
        }
      }
      # 2) pai é <label>
      lab2 <- xml_find_first(inp, "ancestor::label[1]")
      if (!is.na(lab2)) {
        t <- get_text(lab2)
        if (nzchar(t)) return(t)
      }
      # 3) irmão próximo <label>
      sib <- xml_find_first(inp, "following-sibling::label[1]")
      if (!is.na(sib)) {
        t <- get_text(sib)
        if (nzchar(t)) return(t)
      }
      ""
    }
    
    # Mapeia A..E na ordem dos radios
    opts <- sapply(radios, get_opt_text)
    names(opts) <- c("A","B","C","D","E")[seq_along(opts)]
    optA <- opts[["A"]] %||% ""; optB <- opts[["B"]] %||% ""
    optC <- opts[["C"]] %||% ""; optD <- opts[["D"]] %||% ""
    optE <- opts[["E"]] %||% ""
    
    out[[length(out)+1]] <- list(
      question_id = qid, stem = stem,
      opt_a = optA, opt_b = optB, opt_c = optC, opt_d = optD, opt_e = optE
    )
  }
  out
}

# ======== HTTP HELPERS ========
req_hardening <- function(req) {
  req |>
    req_options(ipresolve = 1L) |>
    req_timeout(90) |>
    req_retry(max_tries = 5, backoff = ~ min(60, 2^.x))
}

post_answer_key <- function(payload, endpoint = ENDPOINT_ANSWER_KEY, token = admin_token) {
  req <- request(endpoint) |>
    req_method("POST") |>
    req_headers("Content-Type"="application/json", "Authorization"=paste("Bearer", trimws(token))) |>
    req_body_json(payload)
  resp <- req_hardening(req) |> req_perform()
  list(status = resp_status(resp),
       body   = tryCatch(resp_body_string(resp), error = function(e) ""))
}




post_bank <- function(items, endpoint = ENDPOINT_BANK, token = admin_token) {
  req <- request(endpoint) |>
    req_method("POST") |>
    req_headers("Authorization"=paste("Bearer", trimws(token)),
                "Content-Type"="application/json") |>
    req_body_json(list(items = unname(items))) |>
    req_error(is_error = function(resp) FALSE)
  resp <- req_hardening(req) |> req_perform()
  list(status = resp_status(resp),
       body   = tryCatch(resp_body_string(resp), error = function(e) ""),
       json   = tryCatch(resp_body_json(resp),  error = function(e) NULL))
}

# ======== MONTAGEM DOS PAYLOADS ========

build_bank_payload <- function(files){
  items <- list()
  for (qmd in files) {
    qid    <- derive_question_id(qmd)
    module <- derive_module_from_qid(qid) %||% derive_module_fallback_from_path(qmd)
    base_dir <- fs::path_dir(qmd)
    out_dir  <- fs::path(base_dir, ".render_bank")
    html_out <- render_qmd_to_html(qmd, out_dir)
    qs <- extract_bank_from_html(html_out)
    
    if (length(qs)) {
      for (q in qs) {
        items[[length(items)+1]] <- list(
          question_id = tolower(q$question_id),
          module      = tolower(module %||% ""),
          stem        = q$stem %||% "",
          opt_a       = q$opt_a %||% "",
          opt_b       = q$opt_b %||% "",
          opt_c       = q$opt_c %||% "",
          opt_d       = q$opt_d %||% "",
          opt_e       = q$opt_e %||% "",
          active      = 1
        )
      }
    } else {
      message("  [BANK] Sem MCQ detectado — pulando: ", qid)
    }
    try(fs::dir_delete(out_dir), silent = TRUE)
  }
  items
}

run_ingest_bank <- function(files){
  items <- build_bank_payload(files)
  if (!length(items)) {
    message("[BANK] Nenhum item para enviar.")
    return(list(status = NA, sent = 0, batches = 0))
  }
  chunks <- chunk_list(seq_along(items), size = BANK_BATCH_SIZE)
  sent_total <- 0L
  last_status <- NA_integer_
  for (i in seq_along(chunks)) {
    batch <- items[chunks[[i]]]
    res <- post_bank(batch)
    last_status <- res$status
    sent_total <- sent_total + length(batch)
    message(sprintf("[BANK] Lote %d/%d — HTTP %s — enviados: %d (acum: %d)",
                    i, length(chunks), res$status, length(batch), sent_total))
    if (i < length(chunks)) Sys.sleep(BANK_PAUSE_MS/1000)
  }
  list(status = last_status, sent = sent_total, batches = length(chunks))
}


# ======== ANSWER-KEY (um arquivo por vez) ========
run_ingest_answer_keys <- function(files){
  ok <- 0L; fail <- 0L
  for (qmd in files) {
    qid    <- derive_question_id(qmd)
    module <- derive_module_from_qid(qid) %||% derive_module_fallback_from_path(qmd)
    slideid<- qid
    
    # Render para extrair as keys MCQ
    base_dir <- fs::path_dir(qmd)
    out_dir  <- fs::path(base_dir, ".render_key")
    html_out <- render_qmd_to_html(qmd, out_dir)
    df <- extract_items_from_html(html_out)
    
    # Segurança: só MCQ
    df <- subset(df, tolower(df$qtype) == "mcq")
    
    if (nrow(df) == 0) {
      message("  [KEY] Sem MCQ com 'data-correct-answer' — pulando: ", qid)
      try(fs::dir_delete(out_dir), silent = TRUE)
      next
    }
    
    items <- lapply(seq_len(nrow(df)), function(i) {
      list(
        question_id   = df$question_id[i],
        qtype         = "mcq",
        correct_value = df$correct_value[i],
        tolerance     = NULL,
        module        = tolower(module %||% ""),
        slideid       = slideid
      )
    })
    
    payload <- list(slide_id = slideid, items = items)
    res <- post_answer_key(payload)
    if (res$status >= 200 && res$status < 300) { ok <- ok+1L } else { fail <- fail+1L }
    message(sprintf("[KEY] %s  → HTTP %s", qid, res$status))
    try(fs::dir_delete(out_dir), silent = TRUE)
  }
  list(ok = ok, fail = fail)
}

# ======== TEMPLATES (lotes de 10) ========


# ======== PIPELINE (UMA PASTA POR VEZ) ========
cat("\n=== [1/2] ANSWER-KEY por arquivo (MCQ) ===\n")
ak_chunks <- chunk_list(FILES, size = AK_FILES_BATCH)
ak_ok <- 0L; ak_fail <- 0L
for (i in seq_along(ak_chunks)) {
  r1 <- run_ingest_answer_keys(ak_chunks[[i]])
  ak_ok  <- ak_ok  + r1$ok
  ak_fail<- ak_fail+ r1$fail
  message(sprintf("[KEY] Lote %d/%d — ok:%d fail:%d (acum ok:%d fail:%d)",
                  i, length(ak_chunks), r1$ok, r1$fail, ak_ok, ak_fail))
  if (i < length(ak_chunks)) Sys.sleep(AK_PAUSE_MS/1000)
}
cat(sprintf("Resumo keys: %d OK, %d falhas\n", ak_ok, ak_fail))


cat("\n=== [2/2] BANCO (selfquiz_bank) ===\n")
rbank <- run_ingest_bank(FILES)
cat(sprintf("Resumo bank: enviados %d em %d lotes (último HTTP %s)\n",
            rbank$sent, rbank$batches, rbank$status))


cat("\n=== CHECKPOINT FINAL ===\n")
cat(sprintf("- Arquivos .qmd na pasta: %d\n", length(FILES)))
cat(sprintf("- Itens enviados ao banco: %s\n", rbank$sent))
cat("=== Finished (MCQ-only) ===\n")

