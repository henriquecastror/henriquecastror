# ==========================
# Ingestão de exercícios T/F (.qmd) -> D1 (via Worker)
# - Processa UMA pasta por vez (QMD_DIR)
# - Publica answer-keys (qtype="tf") e adiciona itens no banco (A=True, B=False)
# - NÃO processa MCQ/NUM/Long
# ==========================

# ======== CONFIGURAÇÃO ========
QMD_DIR <- "module6/tf"  # pasta com seus .qmd de T/F

ENDPOINT_BASE       <- "https://course-chat.hcmrtns.workers.dev"
ENDPOINT_ANSWER_KEY <- paste0(ENDPOINT_BASE, "/exercises/answer-key")
PING_URL            <- paste0(ENDPOINT_BASE, "/admin/ping")
ENDPOINT_BANK       <- paste0(ENDPOINT_BASE, "/selfquiz/bank")

ADMIN_TOKEN_ENV     <- "ADMIN_TOKEN"  # defina antes: Sys.setenv(ADMIN_TOKEN = "seu_token")

# Política de lotes/pausas
BANK_BATCH_SIZE   <- 10L   # nº de questões por POST /selfquiz/bank
BANK_PAUSE_MS     <- 400L  # pausa entre lotes do bank

AK_FILES_BATCH    <- 20L   # nº de arquivos .qmd por leva de keys
AK_PAUSE_MS       <- 250L  # pausa entre levas de keys

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
cat("Arquivos encontrados em", QMD_DIR, "→", length(FILES), "arquivos .qmd TF\n")

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

# ======== PARSERS — T/F ========

# Lê o HTML e pega T/F via <input type="text" data-correct-answer="T|F">
extract_tf_keys_from_html <- function(html_path) {
  doc <- read_html(html_path)
  items <- list()
  tf_inputs <- xml_find_all(doc, ".//input[@type='text' and @data-correct-answer]")
  if (length(tf_inputs) > 0) {
    for (inp in tf_inputs) {
      corr <- toupper(trimws(xml_attr(inp, "data-correct-answer") %||% ""))
      if (!corr %in% c("T","F")) next
      qid <- xml_attr(inp, "id") %||% xml_attr(inp, "name") %||%
        paste0("TF_", digest::digest(xml_path(inp)))
      items[[length(items)+1]] <- list(
        question_id   = qid,
        qtype         = "tf",
        correct_value = corr,
        tolerance     = NA_real_
      )
    }
  }
  if (!length(items)) {
    return(data.frame(question_id=character(), qtype=character(),
                      correct_value=character(), tolerance=double(),
                      stringsAsFactors = FALSE))
  }
  df <- do.call(rbind.data.frame, lapply(items, as.data.frame))
  df <- df[!duplicated(df$question_id), ]
  rownames(df) <- NULL
  df$correct_value <- as.character(df$correct_value)
  df$tolerance[] <- NA_real_
  df
}

# Monta itens para o banco (A=True, B=False) + tenta extrair enunciado
extract_tf_bank_from_html <- function(html_path) {
  doc <- read_html(html_path)
  out <- list()
  tf_inputs <- xml_find_all(doc, ".//input[@type='text' and @data-correct-answer]")
  if (length(tf_inputs) == 0) return(out)
  get_text <- function(node) {
    if (is.na(node) || length(node) == 0) return("")
    txt <- xml_text(node, trim = TRUE)
    gsub("\\s+"," ", txt)
  }
  for (inp in tf_inputs) {
    qid <- xml_attr(inp, "id") %||% xml_attr(inp, "name") %||%
      paste0("TF_", digest::digest(xml_path(inp)))
    # Enunciado: pega algo próximo ao input dentro do form
    stem_node <- xml_find_first(
      inp,
      "ancestor::form[1]//*[self::legend or self::h1 or self::h2 or self::h3 or self::h4 or self::strong or self::label or self::p][1]"
    )
    stem <- get_text(stem_node)
    out[[length(out)+1]] <- list(
      question_id = tolower(qid),
      stem = stem %||% "",
      opt_a = "True",
      opt_b = "False",
      opt_c = "",
      opt_d = "",
      opt_e = ""
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

# ======== MONTAGEM & ENVIO (TF) ========

build_tf_bank_payload <- function(files){
  items <- list()
  for (qmd in files) {
    qid_file <- derive_question_id(qmd)
    module   <- derive_module_from_qid(qid_file) %||% derive_module_fallback_from_path(qmd)
    base_dir <- fs::path_dir(qmd)
    out_dir  <- fs::path(base_dir, ".render_bank")
    html_out <- render_qmd_to_html(qmd, out_dir)
    qs <- extract_tf_bank_from_html(html_out)
    if (length(qs)) {
      for (q in qs) {
        items[[length(items)+1]] <- list(
          question_id = tolower(q$question_id),
          module      = tolower(module %||% ""),
          stem        = q$stem %||% "",
          opt_a       = q$opt_a,
          opt_b       = q$opt_b,
          opt_c       = q$opt_c,
          opt_d       = q$opt_d,
          opt_e       = q$opt_e,
          active      = 1
        )
      }
    } else {
      message("  [BANK] Sem T/F detectado — pulando: ", qid_file)
    }
    try(fs::dir_delete(out_dir), silent = TRUE)
  }
  items
}

run_ingest_bank_tf <- function(files){
  items <- build_tf_bank_payload(files)
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

run_ingest_answer_keys_tf <- function(files){
  ok <- 0L; fail <- 0L
  for (qmd in files) {
    qid_file <- derive_question_id(qmd)
    module   <- derive_module_from_qid(qid_file) %||% derive_module_fallback_from_path(qmd)
    slideid  <- qid_file
    
    base_dir <- fs::path_dir(qmd)
    out_dir  <- fs::path(base_dir, ".render_key")
    html_out <- render_qmd_to_html(qmd, out_dir)
    
    df <- extract_tf_keys_from_html(html_out)  # só T/F
    if (nrow(df) == 0) {
      message("  [KEY] Sem T/F com 'data-correct-answer' — pulando: ", qid_file)
      try(fs::dir_delete(out_dir), silent = TRUE)
      next
    }
    
    items <- lapply(seq_len(nrow(df)), function(i) {
      list(
        question_id   = df$question_id[i],
        qtype         = "tf",
        correct_value = df$correct_value[i],
        tolerance     = NULL,
        module        = tolower(module %||% ""),
        slideid       = slideid
      )
    })
    
    payload <- list(slide_id = slideid, items = items)
    res <- post_answer_key(payload)
    if (res$status >= 200 && res$status < 300) { ok <- ok+1L } else { fail <- fail+1L }
    message(sprintf("[KEY] %s  → HTTP %s", qid_file, res$status))
    try(fs::dir_delete(out_dir), silent = TRUE)
  }
  list(ok = ok, fail = fail)
}

# ======== PIPELINE (TF-only) ========
cat("\n=== [1/2] ANSWER-KEY (T/F) ===\n")
ak_chunks <- chunk_list(FILES, size = AK_FILES_BATCH)
ak_ok <- 0L; ak_fail <- 0L
for (i in seq_along(ak_chunks)) {
  r1 <- run_ingest_answer_keys_tf(ak_chunks[[i]])
  ak_ok  <- ak_ok  + r1$ok
  ak_fail<- ak_fail+ r1$fail
  message(sprintf("[KEY] Lote %d/%d — ok:%d fail:%d (acum ok:%d fail:%d)",
                  i, length(ak_chunks), r1$ok, r1$fail, ak_ok, ak_fail))
  if (i < length(ak_chunks)) Sys.sleep(AK_PAUSE_MS/1000)
}
cat(sprintf("Resumo keys TF: %d OK, %d falhas\n", ak_ok, ak_fail))

cat("\n=== [2/2] BANCO (selfquiz_bank — T/F) ===\n")
rbank <- run_ingest_bank_tf(FILES)
cat(sprintf("Resumo bank TF: enviados %d em %d lotes (último HTTP %s)\n",
            rbank$sent, rbank$batches, rbank$status))

cat("\n=== CHECKPOINT FINAL ===\n")
cat(sprintf("- Arquivos .qmd na pasta: %d\n", length(FILES)))
cat(sprintf("- Itens enviados ao banco: %s\n", rbank$sent))
cat("=== Finished (TF-only) ===\n")
