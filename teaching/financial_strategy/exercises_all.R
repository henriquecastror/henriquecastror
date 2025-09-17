# ==========================
# Ingestão de exercícios (.qmd) -> D1 (via Worker) — MCQ-only (estável)
# - Processa UMA pasta por vez (defina QMD_DIR abaixo, ex.: "module1/mcq")
# - Upserta templates (MCQ) e publica answer-keys (MCQ)
# - NÃO usa instances, NÃO usa seeds, NÃO processa TF/NUM/Long
# ==========================

# ======== CONFIGURAÇÃO MÍNIMA ========
QMD_DIR <- "module1/mcq"  # <<< ajuste a pasta com seus .qmd de MCQ

ENDPOINT_BASE       <- "https://course-chat.hcmrtns.workers.dev"
ENDPOINT_ANSWER_KEY <- paste0(ENDPOINT_BASE, "/exercises/answer-key")
ENDPOINT_TEMPL      <- paste0(ENDPOINT_BASE, "/exercises/templates")
PING_URL            <- paste0(ENDPOINT_BASE, "/admin/ping")

ADMIN_TOKEN_ENV     <- "ADMIN_TOKEN"     # defina antes: Sys.setenv(ADMIN_TOKEN = "seu_token")

# Política de lotes/pausas
BATCH_SIZE          <- 10L               # fixo: 10
PAUSE_MS_BETWEEN    <- 250L              # ~0.25s entre lotes

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

post_templates <- function(items, endpoint = ENDPOINT_TEMPL, token = admin_token) {
  req <- request(endpoint) |>
    req_method("POST") |>
    req_headers("Authorization"=paste("Bearer", trimws(token)), "Content-Type"="application/json") |>
    req_body_json(list(items = unname(items))) |>
    req_error(is_error = function(resp) FALSE)
  resp <- req_hardening(req) |> req_perform()
  list(status = resp_status(resp),
       body   = tryCatch(resp_body_string(resp), error = function(e) ""),
       json   = tryCatch(resp_body_json(resp),  error = function(e) NULL))
}

# ======== MONTAGEM DOS PAYLOADS ========
build_templates_payload <- function(files) {
  lapply(files, function(qmd){
    qid    <- derive_question_id(qmd)                 # ex: module5_mcq_q101
    module <- derive_module_from_qid(qid) %||% derive_module_fallback_from_path(qmd)
    list(
      question_id    = qid,
      module         = tolower(module %||% ""),
      qtype          = "mcq",          # força MCQ
      tags           = "",
      source_file    = norm_path(qmd, mustWork = TRUE),
      source_url     = "",
      version_hash   = version_hash_of_qmd(qmd),
      active         = 1
    )
  })
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
    df <- subset(df, tolower(qtype) == "mcq")
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
run_ingest_templates <- function(files){
  items   <- build_templates_payload(files)
  chunks  <- chunk_list(items, BATCH_SIZE)
  results <- list()
  for (i in seq_along(chunks)) {
    res <- post_templates(chunks[[i]])
    ok  <- 0L
    if (!is.null(res$json) && !is.null(res$json$results)) {
      ok <- sum(vapply(res$json$results, function(z) {
        identical(z$action, "upsert") || identical(z$action, "insert")
      }, logical(1)))
    }
    msg <- sprintf("[TPL] Lote %d/%d — HTTP %s — upserts: %d/%d",
                   i, length(chunks), res$status, ok, length(chunks[[i]]))
    message(msg)
    results[[i]] <- list(status = res$status, ok = ok, total = length(chunks[[i]]))
    Sys.sleep(PAUSE_MS_BETWEEN/1000)
  }
  results
}

# ======== PIPELINE (UMA PASTA POR VEZ) ========
cat("\n=== [1/2] ANSWER-KEY por arquivo (MCQ) ===\n")
r1 <- run_ingest_answer_keys(FILES)
cat(sprintf("Resumo: %d OK, %d falhas (sem key ou erro HTTP)\n", r1$ok, r1$fail))

cat("\n=== [2/2] TEMPLATES em lotes de ", BATCH_SIZE, " (MCQ) ===\n", sep = "")
r2 <- run_ingest_templates(FILES)
tpl_ok <- sum(vapply(r2, function(x) x$ok, numeric(1)))
tpl_tot<- sum(vapply(r2, function(x) x$total, numeric(1)))
cat(sprintf("Resumo: %d/%d templates upsertados\n", tpl_ok, tpl_tot))

# ======== CHECKPOINT FINAL ========
cat("\n=== CHECKPOINT FINAL ===\n")
cat(sprintf("- Arquivos .qmd na pasta:        %d\n", length(FILES)))
cat(sprintf("- Templates upsertados (passo 2): %d/%d\n", tpl_ok, tpl_tot))
cat("=== Finished (MCQ-only, no instances) ===\n")
