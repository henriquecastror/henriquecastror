# ==========================
# Ingestão de exercícios (.qmd) -> D1 (via Worker) — Versão “estável 10x10”
# - Processa UMA pasta por vez (defina QMD_DIR abaixo)
# - Lotes de 10 para /templates e /instances, com pausas e checkpoints
# - Deriva module do question_id (prefixo "moduleX_"), com fallback por caminho
# - Usa apenas fragmento de .question-block (evita payload gigante)
# - Sem qualquer regra de pontos no cliente (pontuação é decidida no Worker)
# ==========================

# ======== CONFIGURAÇÃO MÍNIMA ========
QMD_DIR <- "module4/num"  # <- <<< ALINHE AQUI a pasta a importar (ex.: "module1/tf", "module2/num", etc.)

ENDPOINT_BASE       <- "https://course-chat.hcmrtns.workers.dev"
ENDPOINT_ANSWER_KEY <- paste0(ENDPOINT_BASE, "/exercises/answer-key")
ENDPOINT_TEMPL      <- paste0(ENDPOINT_BASE, "/exercises/templates")
ENDPOINT_INST       <- paste0(ENDPOINT_BASE, "/exercises/instances")
PING_URL            <- paste0(ENDPOINT_BASE, "/admin/ping")

ADMIN_TOKEN_ENV     <- "ADMIN_TOKEN"     # defina: Sys.setenv(ADMIN_TOKEN = "seu_token")

# === Multi-seed para NUM ===
MULTISEED_NUM    <- TRUE
NUM_SEEDS_GLOBAL <- c(101L, 202L, 303L, 404L, 505L)   # ajuste como quiser

# (opcional) seeds por arquivo: chaves = caminho completo do .qmd
SEEDS_BY_FILE <- list(
  # "module5/num/module5_num_q1.qmd" = c(111L, 222L)
)


# Política de lotes/pausas
BATCH_SIZE          <- 10L               # fixo: 10
PAUSE_MS_BETWEEN    <- 250L              # ~0.25s entre lotes

# Política de fragmento HTML
REQUIRE_QUESTION_BLOCK <- TRUE           # TRUE = exige .question-block (recomendado)
ALLOW_FALLBACK_FORM    <- TRUE           # se não achar .question-block, tenta <form> (curto)
ALLOW_FALLBACK_FULLDOC <- FALSE          # NUNCA mandar doc inteiro (evita payload gigante)

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

infer_qtype_by_path <- function(p) {
  p <- tolower(p)
  if (grepl("/num/|_num_", p)) return("num")
  if (grepl("/tf/|_tf_",   p)) return("tf")
  if (grepl("/mcq/|_mcq_", p)) return("mcq")
  "long"
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

extract_seed_from_qmd <- function(qmd_path) {
  txt <- readLines(qmd_path, warn = FALSE)
  m <- regexpr("\\bset\\.seed\\s*\\(\\s*(\\d+)\\s*\\)", txt, perl = TRUE)
  if (any(m > 0)) {
    line <- txt[which(m > 0)[1]]
    as.integer(sub(".*set\\.seed\\s*\\(\\s*(\\d+)\\s*\\).*", "\\1", line, perl = TRUE))
  } else NA_integer_
}

make_local_qmd_with_seed <- function(qmd_path, seed) {
  base_dir <- fs::path_dir(qmd_path)
  base_nm  <- fs::path_ext_remove(fs::path_file(qmd_path))
  tmp_path <- fs::path(base_dir, paste0(base_nm, "_seed", seed, "_.qmd"))
  txt <- readLines(qmd_path, warn = FALSE)
  
  pattern <- "\\bset\\.seed\\s*\\([^\\)]*\\)"
  replacement <- sprintf("set.seed(%s)", as.character(seed))
  
  if (any(grepl(pattern, txt))) {
    txt2 <- gsub(pattern, replacement, txt)
  } else {
    open_idx <- which(grepl("^```\\{r", txt, perl = TRUE))[1]
    if (is.na(open_idx)) txt2 <- c("```{r}", replacement, "```", txt)
    else txt2 <- append(txt, values = c(replacement), after = open_idx)
  }
  writeLines(txt2, tmp_path, useBytes = TRUE)
  tmp_path
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

extract_items_from_html <- function(html_path) {
  doc <- read_html(html_path)
  items <- list()
  # MCQ via <form data-correct-answer="A">
  forms_with_correct <- xml_find_all(doc, ".//form[@data-correct-answer]")
  if (length(forms_with_correct) > 0) {
    for (frm in forms_with_correct) {
      corr_letter <- toupper(trimws(xml_attr(frm, "data-correct-answer") %||% ""))
      if (!nzchar(corr_letter)) next
      radios <- xml_find_all(frm, ".//input[@type='radio']")
      if (length(radios) == 0) next
      form_id <- xml_attr(frm, "id") %||% ""
      r_ids   <- xml_attr(radios, "id")
      r_ids   <- r_ids[!is.na(r_ids) & nzchar(r_ids)]
      qid_from_radio_id <- if (length(r_ids) > 0) sub("_[A-Ea-e]$", "", r_ids[1]) else ""
      r_name <- xml_attr(radios[[1]], "name") %||% ""
      qid <- if      (nzchar(form_id))        form_id
      else if (nzchar(qid_from_radio_id)) qid_from_radio_id
      else if (nzchar(r_name))           r_name
      else paste0("MCQ_", digest::digest(paste0(xml_path(frm), corr_letter)))
      items[[length(items)+1]] <- list(
        question_id   = qid, qtype = "mcq",
        correct_value = corr_letter, tolerance = NA_real_
      )
    }
  }
  # Qualquer nó com data-correct-answer (TF/NUM/etc)
  nodes <- xml_find_all(doc, ".//*[@data-correct-answer]")
  for (nd in nodes) {
    if (xml_name(nd) == "form") next
    id  <- xml_attr(nd, "id") %||% ""
    if (!nzchar(id)) next
    qtype <- {
      type <- tolower(xml_attr(nd, "type") %||% "")
      idl  <- tolower(id)
      corr <- tolower(xml_attr(nd, "data-correct-answer") %||% "")
      if (type == "number" || grepl("_num_", idl)) "num"
      else if (grepl("_tf_", idl) || corr %in% c("t","f","true","false")) "tf"
      else if (grepl("_mcq_", idl)) "mcq" else "long"
    }
    corr  <- xml_attr(nd, "data-correct-answer") %||% ""
    tol   <- suppressWarnings(as.numeric(xml_attr(nd, "data-tolerance") %||% NA))
    if (qtype == "mcq") {
      val <- xml_attr(nd, "value") %||% ""
      if (nzchar(val)) corr <- toupper(val)
      else {
        m <- regexpr("([A-Ea-e])$", id); if (m > 0) corr <- toupper(substr(id, m, m+attr(m,"match.length")-1))
      }
    }
    items[[length(items)+1]] <- list(
      question_id   = id, qtype = tolower(qtype),
      correct_value = as.character(corr),
      tolerance     = if (!is.na(tol)) tol else NA_real_
    )
  }
  if (length(items) == 0) {
    return(data.frame(question_id=character(), qtype=character(),
                      correct_value=character(), tolerance=double(),
                      stringsAsFactors = FALSE))
  }
  df <- do.call(rbind.data.frame, lapply(items, as.data.frame))
  df <- df[!duplicated(df$question_id), ]
  rownames(df) <- NULL
  df$qtype <- tolower(df$qtype)
  df$correct_value <- as.character(df$correct_value)
  df$tolerance[!(df$qtype %in% c("num"))] <- NA_real_
  df
}





html_fragment_from <- function(html_path) {
  doc <- xml2::read_html(html_path)
  
  # 1) Preferir o wrapper completo da questão (enunciado + bloco)
  wrap <- xml2::xml_find_first(
    doc,
    ".//*[contains(concat(' ', normalize-space(@class), ' '), ' question ')][.//*[contains(concat(' ', normalize-space(@class), ' '), ' question-block ')]]"
  )
  if (!is.na(wrap) && length(wrap)) return(as.character(wrap))
  
  # 2) Fallback: apenas o bloco respondível
  blk <- xml2::xml_find_first(
    doc,
    ".//*[contains(concat(' ', normalize-space(@class), ' '), ' question-block ')]"
  )
  if (!is.na(blk) && length(blk)) return(as.character(blk))
  
  # 3) Fallbacks antigos (mantidos como segurança)
  if (REQUIRE_QUESTION_BLOCK) stop("HTML sem .question/.question-block: ", html_path)
  if (ALLOW_FALLBACK_FORM) {
    frm <- xml2::xml_find_first(doc, ".//form")
    if (!is.na(frm) && length(frm)) return(as.character(frm))
  }
  if (ALLOW_FALLBACK_FULLDOC) return(as.character(doc))
  stop("Sem fragmento utilizável (nem .question/.question-block nem <form>): ", html_path)
}







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

post_instances <- function(items, endpoint = ENDPOINT_INST, token = admin_token) {
  req <- request(endpoint) |>
    req_method("POST") |>
    req_headers("Authorization"=paste("Bearer", trimws(token)), "Content-Type"="application/json") |>
    req_body_json(list(items = items)) |>
    req_error(is_error = function(resp) FALSE)
  resp <- req_hardening(req) |> req_perform()
  list(status = resp_status(resp),
       body   = tryCatch(resp_body_string(resp), error = function(e) ""),
       json   = tryCatch(resp_body_json(resp),  error = function(e) NULL))
}

# ======== MONTAGEM DOS PAYLOADS ========
build_templates_payload <- function(files) {
  lapply(files, function(qmd){
    qid    <- derive_question_id(qmd)                 # ex: module5_tf_q101
    qtype  <- infer_qtype_by_path(qmd)
    module <- derive_module_from_qid(qid) %||% derive_module_fallback_from_path(qmd)
    list(
      question_id    = qid,
      module         = tolower(module %||% ""),
      qtype          = qtype,
      tags           = "",
      source_file    = norm_path(qmd, mustWork = TRUE),
      source_url     = "",
      version_hash   = version_hash_of_qmd(qmd),
      active         = 1
    )
  })
}

build_instances_payload <- function(files) {
  out <- list()
  for (qmd in files) {
    qid     <- derive_question_id(qmd)
    qtype   <- infer_qtype_by_path(qmd)
    base_dir<- fs::path_dir(qmd)
    
    # --- escolher seeds ---
    if (qtype == "num" && isTRUE(MULTISEED_NUM)) {
      seeds <- SEEDS_BY_FILE[[qmd]]
      if (is.null(seeds)) seeds <- NUM_SEEDS_GLOBAL
      seeds <- unique(as.integer(seeds))
      seeds <- seeds[is.finite(seeds)]
      if (!length(seeds)) {
        # fallback para seed do próprio .qmd (ou nosd)
        s <- extract_seed_from_qmd(qmd); seeds <- if (is.na(s)) NA_integer_ else s
      }
    } else if (qtype == "num") {
      s <- extract_seed_from_qmd(qmd); seeds <- if (is.na(s)) NA_integer_ else s
    } else {
      seeds <- NA_integer_
    }
    
    # --- gerar instâncias (uma por seed) ---
    for (sd in seeds) {
      use_copy <- (qtype == "num" && !is.na(sd))
      tmp_qmd  <- if (use_copy) make_local_qmd_with_seed(qmd, sd) else qmd
      out_dir  <- fs::path(base_dir, paste0(".render_inst_", ifelse(is.na(sd), "nosd", sd)))
      fs::dir_create(out_dir, recurse = TRUE)
      
      html_out <- render_qmd_to_html(tmp_qmd, out_dir)
      df <- extract_items_from_html(html_out)
      
      keys <- if (nrow(df) == 0) list() else lapply(seq_len(nrow(df)), function(i) {
        list(
          part_id       = df$question_id[i],
          qtype_part    = df$qtype[i],
          correct_value = as.character(df$correct_value[i]),
          tolerance     = if (is.na(df$tolerance[i])) NULL else unname(df$tolerance[i])
        )
      })
      
      out[[length(out)+1]] <- list(
        question_id   = qid,
        seed          = if (qtype == "num") (if (!is.na(sd)) sd else { s <- extract_seed_from_qmd(qmd); if (is.na(s)) NULL else s }) else NULL,
        version_hash  = version_hash_of_qmd(qmd),
        html_fragment = html_fragment_from(html_out),  # <- já prioriza .question
        active        = 1,
        keys          = keys
      )
      
      if (use_copy) try(fs::file_delete(tmp_qmd), silent = TRUE)
      try(fs::dir_delete(out_dir), silent = TRUE)
    }
  }
  out
}


# ======== ANSWER-KEY (um arquivo por vez) ========
run_ingest_answer_keys <- function(files){
  ok <- 0L; fail <- 0L
  for (qmd in files) {
    qid    <- derive_question_id(qmd)
    qtype  <- infer_qtype_by_path(qmd)
    module <- derive_module_from_qid(qid) %||% derive_module_fallback_from_path(qmd)
    slideid<- qid
    
    # Render para extrair as partes/keys daquele arquivo
    base_dir <- fs::path_dir(qmd)
    out_dir  <- fs::path(base_dir, ".render_key")
    html_out <- render_qmd_to_html(qmd, out_dir)
    df <- extract_items_from_html(html_out)
    
    if (nrow(df) == 0) {
      message("  [KEY] Sem 'data-correct-answer' — pulando: ", qid)
      try(fs::dir_delete(out_dir), silent = TRUE)
      next
    }
    
    items <- lapply(seq_len(nrow(df)), function(i) {
      list(
        question_id   = df$question_id[i],
        qtype         = df$qtype[i],
        correct_value = df$correct_value[i],
        tolerance     = if (is.na(df$tolerance[i])) NULL else unname(df$tolerance[i]),
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

# ======== INSTANCES (lotes de 10) ========
run_ingest_instances <- function(files){
  items   <- build_instances_payload(files)
  chunks  <- chunk_list(items, BATCH_SIZE)
  results <- list()
  for (i in seq_along(chunks)) {
    res <- post_instances(chunks[[i]])
    ok  <- length(chunks[[i]]) # assume 2xx como sucesso do lote
    if (!(res$status >= 200 && res$status < 300)) ok <- 0L
    msg <- sprintf("[INS] Lote %d/%d — HTTP %s — enviados: %d",
                   i, length(chunks), res$status, length(chunks[[i]]))
    message(msg)
    results[[i]] <- list(status = res$status, ok = ok, total = length(chunks[[i]]))
    Sys.sleep(PAUSE_MS_BETWEEN/1000)
  }
  results
}

# ======== PIPELINE (UMA PASTA POR VEZ) ========
cat("\n=== [1/3] ANSWER-KEY por arquivo ===\n")
r1 <- run_ingest_answer_keys(FILES)
cat(sprintf("Resumo: %d OK, %d falhas (sem key ou erro HTTP)\n", r1$ok, r1$fail))

cat("\n=== [2/3] TEMPLATES em lotes de ", BATCH_SIZE, " ===\n", sep = "")
r2 <- run_ingest_templates(FILES)
tpl_ok <- sum(vapply(r2, function(x) x$ok, numeric(1)))
tpl_tot<- sum(vapply(r2, function(x) x$total, numeric(1)))
cat(sprintf("Resumo: %d/%d templates upsertados\n", tpl_ok, tpl_tot))

cat("\n=== [3/3] INSTANCES em lotes de ", BATCH_SIZE, " ===\n", sep = "")
r3 <- run_ingest_instances(FILES)
ins_ok <- sum(vapply(r3, function(x) x$ok, numeric(1)))
ins_tot<- sum(vapply(r3, function(x) x$total, numeric(1)))
cat(sprintf("Resumo: %d/%d instâncias enviadas\n", ins_ok, ins_tot))

# ======== CHECKPOINT FINAL ========
cat("\n=== CHECKPOINT FINAL ===\n")
cat(sprintf("- Arquivos .qmd na pasta:        %d\n", length(FILES)))
cat(sprintf("- Templates upsertados (passo 2): %d/%d\n", tpl_ok, tpl_tot))
cat(sprintf("- Instâncias enviadas (passo 3):  %d/%d\n", ins_ok, ins_tot))
if (tpl_ok < tpl_tot) cat("Atenção: templates incompletos — revise lotes com HTTP != 200.\n")
if (ins_ok < ins_tot) cat("Atenção: instâncias incompletas — revise lotes com HTTP != 200.\n")
cat("=== Finished ===\n")
