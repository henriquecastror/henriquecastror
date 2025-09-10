# ==========================
# Ingestão de exercícios (.qmd) -> D1 (via Worker)
# - Renderiza QMD ao lado do arquivo (sem misturar drives)
# - Usa seed SOMENTE para questões numéricas (e somente se você fornecer)
# - Extrai gabaritos do HTML (id, data-correct-answer, data-tolerance)
# - Publica:
#     (1) /exercises/answer-key  (chave canônica por question_id/part_id)
#     (2) /exercises/templates   (catálogo por question_id)
#     (3) /exercises/instances   (instâncias concretas com html + keys)
# ==========================

# -------- Configurações principais --------
ENDPOINT_ANSWER_KEY <- "https://course-chat.hcmrtns.workers.dev/exercises/answer-key"
ENDPOINT_TEMPL      <- "https://course-chat.hcmrtns.workers.dev/exercises/templates"
ENDPOINT_INST       <- "https://course-chat.hcmrtns.workers.dev/exercises/instances"

ADMIN_TOKEN_ENV     <- "ADMIN_TOKEN"   # defina no seu ambiente
USE_POINTS_LOCAL    <- TRUE            # se FALSE, envia points = NULL (centralize em D1)
POINTS_BY_TYPE      <- list(tf = 1L, mcq = 1L, num = 2L, long = 5L)

# Liste aqui os arquivos do lote (caminhos relativos ao repo)
FILES <- c(
  "module4/num/module4_num_q16.qmd",
  "module4/num/module4_num_q17.qmd",
  "module4/num/module4_num_q18.qmd",
  "module4/tf/module4_tf_q101.qmd",
  "module4/tf/module4_tf_q102.qmd",
  "module4/tf/module4_tf_q103.qmd"
)

# Metadados por arquivo — AS CHAVES DEVEM SER IGUAIS AOS PATHS EM FILES
# seeds = NA  -> sem override (usa como está no QMD)
FILE_META <- list(
  "module4/num/module4_num_q16.qmd"   = list(module = "mod4", slideid = "module4_num_qu3", seeds = NA),  # seed canônica já no QMD
  "module4/num/module4_num_q17.qmd"   = list(module = "mod4", slideid = "module4_num_qu3", seeds = NA),  # seed canônica já no QMD
  "module4/num/module4_num_q18.qmd"   = list(module = "mod4", slideid = "module4_num_qu3", seeds = NA),  # seed canônica já no QMD
  "module4/tf/module4_tf_q101.qmd"   = list(module = "mod4", slideid = "module4_num_qu3", seeds = NA),  # seed canônica já no QMD
  "module4/tf/module4_tf_q102.qmd"   = list(module = "mod4", slideid = "module4_num_qu3", seeds = NA),  # seed canônica já no QMD
  "module4/tf/module4_tf_q103.qmd"   = list(module = "mod4", slideid = "module4_num_qu3", seeds = NA)  # seed canônica já no QMD
)

# -------- Pacotes --------
ensure_pkg <- function(pkgs) for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
ensure_pkg(c("fs", "xml2", "stringr", "jsonlite", "httr2", "digest"))

library(fs)
library(xml2)
library(stringr)
library(jsonlite)
library(httr2)
library(digest)

`%||%` <- function(a,b) if (is.null(a) || is.na(a)) b else a

# -------- Utilitários --------
admin_token <- Sys.getenv(ADMIN_TOKEN_ENV, unset = NA)
if (is.na(admin_token) || admin_token == "") stop(sprintf("Variável de ambiente %s não definida.", ADMIN_TOKEN_ENV))

norm_path <- function(p, mustWork = TRUE) normalizePath(p, winslash = "/", mustWork = mustWork)

# Renderiza para HTML usando --output-dir e localiza o .html (o Quarto às vezes cria subpastas)
render_qmd_to_html <- function(qmd_path, out_dir) {
  if (!fs::file_exists(qmd_path)) stop("Arquivo não encontrado: ", qmd_path)
  fs::dir_create(out_dir, recurse = TRUE)
  qmd_norm <- normalizePath(qmd_path, winslash = "/", mustWork = TRUE)
  out_norm <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
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
  stop("Falha ao renderizar HTML (verifique quarto e o arquivo): ", qmd_path,
       "\nSaída:\n", paste(status, collapse = "\n"))
}

# Cria cópia temporária do QMD (no MESMO diretório) substituindo set.seed(<seed>)
make_local_qmd_with_seed <- function(qmd_path, seed) {
  base_dir <- fs::path_dir(qmd_path)
  base_nm  <- fs::path_ext_remove(fs::path_file(qmd_path))
  tmp_path <- fs::path(base_dir, paste0(base_nm, "_seed", seed, "_.qmd"))
  txt <- readLines(qmd_path, warn = FALSE)
  pattern <- "\\bset\\.seed\\s*\\([^\\)]*\\)"
  replacement <- sprintf("set.seed(%s)", as.character(seed))
  txt2 <- if (any(grepl(pattern, txt))) {
    gsub(pattern, replacement, txt)
  } else {
    open_idx <- which(grepl("^```\\{r", txt, perl = TRUE))[1]
    if (is.na(open_idx)) c("```{r}", replacement, "```", txt) else append(txt, values = c(replacement), after = open_idx)
  }
  writeLines(txt2, tmp_path, useBytes = TRUE)
  return(tmp_path)
}

# Heurística do tipo de questão (por nó HTML)
infer_qtype <- function(node) {
  type <- tolower(xml_attr(node, "type") %||% "")
  id   <- tolower(xml_attr(node, "id") %||% "")
  if (type == "number") return("num")
  if (grepl("_num_", id)) return("num")
  if (grepl("_tf_",  id)) return("tf")
  if (type %in% c("radio","checkbox")) return("mcq")
  if (grepl("_mcq_", id)) return("mcq")
  corr <- tolower(xml_attr(node, "data-correct-answer") %||% "")
  if (corr %in% c("t","f","true","false")) return("tf")
  return("long")
}

# Heurística do tipo de questão (por caminho do arquivo)
infer_qtype_by_path <- function(p) {
  p <- tolower(p)
  if (grepl("/num/|_num_", p)) return("num")
  if (grepl("/tf/|_tf_",   p)) return("tf")
  if (grepl("/mcq/|_mcq_", p)) return("mcq")
  "long"
}

# Extrai gabaritos do HTML
extract_items_from_html <- function(html_path) {
  doc <- read_html(html_path)
  items <- list()
  # 1) MCQ via <form data-correct-answer="A"> (um item por form)
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
      else if (nzchar(r_name))         r_name
      else paste0("MCQ_", digest::digest(paste0(xml_path(frm), corr_letter)))
      items[[length(items)+1]] <- list(
        question_id   = qid,        # preserva o casing do QMD
        qtype         = "mcq",
        correct_value = corr_letter,
        tolerance     = NA_real_
      )
    }
  }
  # 2) Qualquer nó com data-correct-answer (num/tf/mcq marcados por input)
  nodes <- xml_find_all(doc, ".//*[@data-correct-answer]")
  for (nd in nodes) {
    if (xml_name(nd) == "form") next
    id  <- xml_attr(nd, "id") %||% ""
    if (!nzchar(id)) next
    qtype <- infer_qtype(nd)
    corr  <- xml_attr(nd, "data-correct-answer") %||% ""
    tol   <- xml_attr(nd, "data-tolerance") %||% NA
    tol   <- suppressWarnings(as.numeric(tol))
    if (qtype == "mcq") {
      val <- xml_attr(nd, "value") %||% ""
      if (nzchar(val)) {
        corr <- toupper(val)
      } else {
        m <- regexpr("([A-Ea-e])$", id)
        if (m > 0) corr <- toupper(substr(id, m, m+attr(m, "match.length")-1))
      }
    }
    items[[length(items)+1]] <- list(
      question_id   = id,                      # preserva casing original
      qtype         = qtype,
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

# (Opcional) hash do QMD
version_hash_of_qmd <- function(qmd_path) {
  txt <- readLines(qmd_path, warn = FALSE)
  norm <- gsub("\\s+", " ", trimws(txt))
  digest(paste(norm, collapse = "\n"), algo = "sha256")
}

# Extrai seed canônica do QMD, se houver
extract_seed_from_qmd <- function(qmd_path) {
  txt <- readLines(qmd_path, warn = FALSE)
  m <- regexpr("\\bset\\.seed\\s*\\(\\s*(\\d+)\\s*\\)", txt, perl = TRUE)
  if (any(m > 0)) {
    line <- txt[which(m > 0)[1]]
    as.integer(sub(".*set\\.seed\\s*\\(\\s*(\\d+)\\s*\\).*", "\\1", line, perl = TRUE))
  } else NA_integer_
}

# Extrai o outerHTML do bloco .question-block; fallback: <form>; fallback 2: HTML inteiro
html_fragment_from <- function(html_path) {
  doc <- xml2::read_html(html_path)
  blk <- xml2::xml_find_first(doc, ".//*[contains(@class,'question-block')]")
  if (!is.na(blk) && length(blk)) return(as.character(blk))
  frm <- xml2::xml_find_first(doc, ".//form")
  if (!is.na(frm) && length(frm)) return(as.character(frm))
  as.character(doc)
}

# POST para /exercises/answer-key (Authorization: Bearer)
post_answer_key <- function(payload, endpoint = ENDPOINT_ANSWER_KEY, token = admin_token) {
  httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type"  = "application/json",
      "Authorization" = paste("Bearer", trimws(token))
    ) |>
    httr2::req_body_json(payload) |>
    httr2::req_perform() |>
    (\(resp) list(
      status = httr2::resp_status(resp),
      body   = tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    ))()
}

# POST para /exercises/templates
post_templates <- function(items, endpoint = ENDPOINT_TEMPL, token = admin_token) {
  httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", trimws(token)),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(items = items)) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

post_instances <- function(items, endpoint = ENDPOINT_INST, token = admin_token) {
  httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", trimws(token)),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(items = items)) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}



points_for <- function(qtype) {
  if (!USE_POINTS_LOCAL) return(NULL)
  POINTS_BY_TYPE[[tolower(qtype)]] %||% NULL
}

# -------- Payloads templates/instâncias --------

build_templates_payload <- function(files = FILES, file_meta = FILE_META) {
  items <- lapply(files, function(qmd){
    meta <- file_meta[[qmd]]
    list(
      question_id    = tolower(fs::path_ext_remove(fs::path_file(qmd))),   # ex: module1_mcq_q1
      module         = meta$module %||% "",
      qtype          = infer_qtype_by_path(qmd),
      points_default = POINTS_BY_TYPE[[infer_qtype_by_path(qmd)]] %||% NULL,
      tags           = "",
      source_file    = qmd,
      source_url     = "",
      version_hash   = version_hash_of_qmd(qmd),
      active         = 1
    )
  })
  items
}

# Gera instâncias:
# - Para TF/MCQ/Long: 1 instância (seed = NULL), renderizando o QMD como está
# - Para NUM:
#    * se FILE_META$seeds vier vetor numérico => cria uma instância por seed (override copiando QMD)
#    * se NA/ausente => 1 instância conforme o QMD (seed extraída, se existir)
build_instances_payload <- function(files = FILES, file_meta = FILE_META) {
  out <- list()
  for (qmd in files) {
    meta    <- file_meta[[qmd]]
    qtype   <- tolower(infer_qtype_by_path(qmd))
    base_dir <- fs::path_dir(qmd)
    
    # decidir seeds a usar
    seeds_meta <- meta$seeds %||% NA
    use_override_seeds <- (qtype == "num" && length(seeds_meta) > 0 && !all(is.na(seeds_meta)))
    
    # lista de seeds a iterar (para num com override) OU um único NA (um render simples)
    seeds_iter <- if (use_override_seeds) seeds_meta else NA
    
    for (sd in seeds_iter) {
      # caminho de renderização
      use_copy <- (qtype == "num" && !is.na(sd))
      tmp_qmd  <- if (use_copy) make_local_qmd_with_seed(qmd, sd) else qmd
      
      out_dir  <- fs::path(base_dir, paste0(".render_inst_", ifelse(is.na(sd), "nosd", sd)))
      fs::dir_create(out_dir, recurse = TRUE)
      
      html_out <- render_qmd_to_html(tmp_qmd, out_dir)
      df <- extract_items_from_html(html_out)
      
      # monta keys (um registro por input/parte)
      keys <- lapply(seq_len(nrow(df)), function(i) {
        list(
          part_id       = df$question_id[i],
          qtype_part    = df$qtype[i],
          correct_value = as.character(df$correct_value[i]),
          tolerance     = if (is.na(df$tolerance[i])) NULL else unname(df$tolerance[i])
        )
      })
      
      # seed a enviar:
      seed_val <- if (qtype == "num") {
        if (!is.na(sd)) sd else {
          s <- extract_seed_from_qmd(qmd); if (is.na(s)) NULL else s
        }
      } else NULL
      
      out[[length(out) + 1]] <- list(
        question_id   = tolower(fs::path_ext_remove(fs::path_file(qmd))),
        seed          = seed_val,
        version_hash  = version_hash_of_qmd(qmd),
        html_fragment = html_fragment_from(html_out),
        active        = 1,
        keys          = keys
      )
      
      # limpeza
      if (use_copy) try(fs::file_delete(tmp_qmd), silent = TRUE)
      try(fs::dir_delete(out_dir), silent = TRUE)
    }
  }
  out
}

# -------- Pipeline principal (lote) --------
run_ingest_batch <- function(files = FILES, file_meta = FILE_META) {
  if (!setequal(files, names(file_meta))) stop("FILES e names(FILE_META) precisam casar 1:1.")
  summary <- list()
  for (qmd in files) {
    meta    <- file_meta[[qmd]]
    module  <- meta$module
    slideid <- meta$slideid
    # seeds: se ausente, vazio ou NA -> uma passada sem override de seed
    seeds   <- meta$seeds %||% NA
    if (length(seeds) == 0 || all(is.na(seeds))) seeds <- NA
    
    message("\n=== Arquivo: ", qmd, " | seeds: ", paste(seeds, collapse = ","), " ===")
    is_numeric_q <- grepl("/num/|_num_", qmd, ignore.case = TRUE)
    base_dir <- fs::path_dir(qmd)
    seed_results <- list()
    
    for (sd in seeds) {
      use_copy <- is_numeric_q && !is.na(sd)
      tmp_qmd  <- if (use_copy) make_local_qmd_with_seed(qmd, sd) else qmd
      out_dir  <- fs::path(base_dir, paste0(".render_", ifelse(is.na(sd), "nosd", sd)))
      out_dir  <- norm_path(out_dir, mustWork = FALSE)
      fs::dir_create(out_dir, recurse = TRUE)
      html_out <- render_qmd_to_html(tmp_qmd, out_dir)
      df <- extract_items_from_html(html_out)
      
      if (nrow(df) == 0) {
        message("  [seed ", sd, "] Nenhum input com data-correct-answer encontrado. Pulando.")
        if (use_copy) try(fs::file_delete(tmp_qmd), silent = TRUE)
        try(fs::dir_delete(out_dir), silent = TRUE)
        next
      }
      
      items <- lapply(seq_len(nrow(df)), function(i) {
        list(
          question_id   = df$question_id[i],                       # casing preservado
          qtype         = df$qtype[i],
          correct_value = df$correct_value[i],
          tolerance     = if (is.na(df$tolerance[i])) NULL else unname(df$tolerance[i]),
          points        = points_for(df$qtype[i]),
          module        = module,
          slideid       = slideid
        )
      })
      
      payload <- list(slide_id = slideid, items = items)
      res <- post_answer_key(payload)
      message("  [seed ", sd, "] HTTP ", res$status, " | ", substr(res$body, 1, 240))
      message("  preview items: ", nrow(df), " | ex: ", paste(head(df$question_id, 3), collapse=", "))
      
      seed_results[[as.character(sd)]] <- list(status = res$status, body = res$body)
      if (use_copy) try(fs::file_delete(tmp_qmd), silent = TRUE)
      try(fs::dir_delete(out_dir), silent = TRUE)
    }
    summary[[qmd]] <- seed_results
  }
  invisible(summary)
}

# -------- Execução --------
if (!setequal(FILES, names(FILE_META))) {
  cat("ATENÇÃO: FILES e names(FILE_META) não casam 1:1.\n")
  print(FILES); print(names(FILE_META))
  stop("Ajuste FILE_META para ter as mesmas chaves de FILES.")
}

# 1) Answer key (canônica)
resumo <- run_ingest_batch(FILES, FILE_META)
cat("\n=== FIM | Resumo curto (answer_key) ===\n")
for (nm in names(resumo)) {
  seeds <- resumo[[nm]]
  ok <- sum(vapply(seeds, function(x) isTRUE(x$status >= 200 && x$status < 300), logical(1)))
  cat(sprintf("- %s: %d/%d POSTs OK\n", nm, ok, length(seeds)))
}

# 2) Templates (catálogo)
tpl_items  <- build_templates_payload(FILES, FILE_META)
cat("\n=== Publicando templates ===\n")
cat(post_templates(tpl_items), "\n")

# 3) Instances (html + chaves por parte)
inst_items <- build_instances_payload(FILES, FILE_META)
cat("\n=== Publicando instances ===\n")
cat(substr(post_instances(inst_items), 1, 800), "\n")

