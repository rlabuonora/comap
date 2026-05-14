#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

input_dir <- Sys.getenv("COMAP_DATA_DIR", "data")
output_dir <- Sys.getenv("COMAP_OUTPUT_DIR", file.path(input_dir, "processed"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("dplyr", "lubridate", "readr", "readxl", "stringr", "tidyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

normalize_name <- function(x) {
  x |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA else x[[1]]
}

column_aliases <- list(
  rut = c("rut", "r_u_t", "nro_rut", "numero_rut", "documento"),
  codigo_comap = c(
    "codigo_comap", "cod_comap", "codigo", "nro_comap", "numero_comap",
    "expediente", "nro_expediente", "numero_expediente", "archivo", "carpeta"
  ),
  empresa = c("empresa", "razon_social", "nombre_empresa", "denominacion", "titular"),
  ministerio_evaluador = c(
    "ministerio_evaluador", "ministerio", "evaluador", "organismo", "inciso",
    "ministerio_eval"
  ),
  fecha_presentado = c(
    "fecha_presentado", "fecha_presentacion", "fecha_de_presentacion",
    "presentado", "fecha_ingreso", "fecha_registro", "fecha"
  ),
  fecha_recomendado = c(
    "fecha_recomendado", "fecha_recomendacion", "fecha_de_recomendacion",
    "recomendado", "fecha_aprobacion", "fecha_resolucion", "fecha_comap"
  ),
  regimen = c("regimen", "decreto", "normativa", "marco_normativo"),
  tramo = c("tramo", "ampliacion", "tipo_tramo", "etapa"),
  monto_ui = c(
    "monto_ui", "monto_en_ui", "monto_ui_total", "inversion_ui",
    "inversion_total_ui", "monto_inversion_ui", "ui"
  )
)

find_alias <- function(names, aliases) {
  exact <- intersect(aliases, names)
  if (length(exact) > 0) return(exact[[1]])

  partial <- names[vapply(names, function(name) {
    any(str_detect(name, fixed(aliases)))
  }, logical(1))]

  if (length(partial) > 0) partial[[1]] else NA_character_
}

score_header_row <- function(values) {
  names <- normalize_name(as.character(values))
  sum(vapply(column_aliases, function(aliases) {
    !is.na(find_alias(names, aliases))
  }, logical(1)))
}

detect_header_row <- function(path, sheet) {
  preview <- read_excel(path, sheet = sheet, col_names = FALSE, n_max = 25, .name_repair = "minimal")
  scores <- apply(as.data.frame(preview), 1, score_header_row)
  best <- which.max(scores)
  if (length(best) == 0 || scores[[best]] < 2) return(1L)
  as.integer(best)
}

read_best_sheet <- function(path, dataset) {
  sheets <- excel_sheets(path)
  candidates <- lapply(sheets, function(sheet) {
    header_row <- detect_header_row(path, sheet)
    data <- read_excel(path, sheet = sheet, skip = header_row - 1, .name_repair = "unique_quiet")
    names(data) <- normalize_name(names(data))

    expected <- names(column_aliases)
    if (dataset == "presentados") expected <- setdiff(expected, "fecha_recomendado")
    if (dataset == "recomendados") expected <- setdiff(expected, "fecha_presentado")

    score <- sum(vapply(expected, function(field) {
      !is.na(find_alias(names(data), column_aliases[[field]]))
    }, logical(1)))

    list(sheet = sheet, header_row = header_row, data = data, score = score)
  })

  candidates[[which.max(vapply(candidates, `[[`, numeric(1), "score"))]]
}

as_text <- function(x) {
  x <- as.character(x)
  str_squish(x)
}

parse_monto <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  text <- as.character(x)
  comma <- parse_number(text, locale = locale(decimal_mark = ",", grouping_mark = "."))
  dot <- parse_number(text, locale = locale(decimal_mark = ".", grouping_mark = ","))
  if (sum(!is.na(dot)) > sum(!is.na(comma))) dot else comma
}

parse_excel_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  text <- str_squish(as.character(x))
  text[text == ""] <- NA_character_
  parsed <- suppressWarnings(coalesce(
    dmy(text),
    ymd(text),
    mdy(text),
    as.Date(parse_date_time(text, orders = c("dmy", "ymd", "mdy", "dmY", "Ymd")))
  ))
  as.Date(parsed)
}

normalize_rut <- function(x) {
  rut <- str_replace_all(as.character(x), "[^0-9]", "")
  rut[rut == ""] <- NA_character_
  rut
}

normalize_codigo <- function(x) {
  codigo <- as_text(x)
  codigo <- str_replace(codigo, "\\.0$", "")
  codigo[codigo == ""] <- NA_character_
  codigo
}

normalize_ministerio <- function(x) {
  text <- normalize_name(as_text(x))
  case_when(
    str_detect(text, "mef") ~ "MEF",
    str_detect(text, "miem") ~ "MIEM",
    str_detect(text, "mintur|turismo") ~ "MINTUR",
    str_detect(text, "mgap|ganaderia|agricultura|pesca") ~ "MGAP",
    TRUE ~ str_to_upper(as_text(x))
  )
}

standardize_macrobase <- function(path, dataset) {
  best <- read_best_sheet(path, dataset)
  data <- best$data
  aliases <- vapply(column_aliases, function(alias_set) find_alias(names(data), alias_set), character(1))

  value <- function(field) {
    col <- aliases[[field]]
    if (is.na(col)) rep(NA, nrow(data)) else data[[col]]
  }

  tibble(
    source_file = basename(path),
    source_path = path,
    source_sheet = best$sheet,
    source_header_row = best$header_row,
    source_row = seq_len(nrow(data)) + best$header_row,
    rut = normalize_rut(value("rut")),
    codigo_comap = normalize_codigo(value("codigo_comap")),
    empresa = as_text(value("empresa")),
    ministerio_evaluador = normalize_ministerio(value("ministerio_evaluador")),
    fecha_presentado = parse_excel_date(value("fecha_presentado")),
    fecha_recomendado = parse_excel_date(value("fecha_recomendado")),
    regimen = as_text(value("regimen")),
    tramo = as_text(value("tramo")),
    monto_ui = parse_monto(value("monto_ui"))
  ) |>
    mutate(
      empresa = na_if(empresa, ""),
      ministerio_evaluador = na_if(ministerio_evaluador, ""),
      regimen = na_if(regimen, ""),
      tramo = na_if(tramo, ""),
      tramo_clean = normalize_name(coalesce(tramo, "")),
      es_ampliacion = str_detect(tramo_clean, "ampli|ampl"),
      project_key = if_else(!is.na(rut) & !is.na(codigo_comap), paste(rut, codigo_comap, sep = "::"), NA_character_)
    ) |>
    filter(if_any(c(rut, codigo_comap, empresa, ministerio_evaluador, fecha_presentado, fecha_recomendado, monto_ui), ~ !is.na(.x)))
}

find_input_files <- function(pattern) {
  files <- list.files(input_dir, pattern = "\\.xls[xm]?$", full.names = TRUE, recursive = TRUE)
  files <- files[!str_detect(files, fixed(output_dir))]
  files[str_detect(normalize_name(basename(files)), pattern)]
}

presentados_files <- find_input_files("presentad")
recomendados_files <- find_input_files("recomend")

if (length(presentados_files) == 0 || length(recomendados_files) == 0) {
  stop(
    "Could not find both Macrobase workbooks under ", input_dir, ". ",
    "Expected filenames containing 'presentad' and 'recomend'.",
    call. = FALSE
  )
}

presentados <- bind_rows(lapply(presentados_files, standardize_macrobase, dataset = "presentados")) |>
  mutate(presentado_row_id = row_number())

recomendados <- bind_rows(lapply(recomendados_files, standardize_macrobase, dataset = "recomendados")) |>
  mutate(recomendado_row_id = row_number())

recommended_exact <- recomendados |>
  filter(!is.na(project_key), tramo_clean != "") |>
  group_by(project_key, tramo_clean) |>
  summarise(
    fecha_recomendado_exact = suppressWarnings(min(fecha_recomendado, na.rm = TRUE)),
    regimen_recomendado_exact = first_non_missing(regimen),
    monto_ui_recomendado_exact = first_non_missing(monto_ui),
    ministerio_recomendado_exact = first_non_missing(ministerio_evaluador),
    recommended_rows_exact = n(),
    .groups = "drop"
  ) |>
  mutate(fecha_recomendado_exact = if_else(is.infinite(fecha_recomendado_exact), as.Date(NA), fecha_recomendado_exact))

recommended_project <- recomendados |>
  filter(!is.na(project_key)) |>
  group_by(project_key) |>
  summarise(
    fecha_recomendado_project = suppressWarnings(min(fecha_recomendado, na.rm = TRUE)),
    regimen_recomendado_project = first_non_missing(regimen),
    monto_ui_recomendado_project = first_non_missing(monto_ui),
    ministerio_recomendado_project = first_non_missing(ministerio_evaluador),
    recommended_rows_project = n(),
    recommended_tramos = n_distinct(tramo_clean[tramo_clean != ""]),
    .groups = "drop"
  ) |>
  mutate(fecha_recomendado_project = if_else(is.infinite(fecha_recomendado_project), as.Date(NA), fecha_recomendado_project))

authoritative_from_presentados <- presentados |>
  left_join(recommended_exact, by = c("project_key", "tramo_clean")) |>
  left_join(recommended_project, by = "project_key") |>
  mutate(
    can_fallback_to_project = is.na(fecha_recomendado_exact) & recommended_rows_project == 1,
    fecha_recomendado = coalesce(
      fecha_recomendado_exact,
      if_else(can_fallback_to_project, fecha_recomendado_project, as.Date(NA))
    ),
    regimen_recomendado = coalesce(
      regimen_recomendado_exact,
      if_else(can_fallback_to_project, regimen_recomendado_project, NA_character_)
    ),
    monto_ui_recomendado = coalesce(
      monto_ui_recomendado_exact,
      if_else(can_fallback_to_project, as.numeric(monto_ui_recomendado_project), NA_real_)
    ),
    ministerio_recomendado = coalesce(
      ministerio_recomendado_exact,
      if_else(can_fallback_to_project, ministerio_recomendado_project, NA_character_)
    ),
    match_method = case_when(
      !is.na(fecha_recomendado_exact) ~ "rut_codigo_tramo",
      can_fallback_to_project ~ "rut_codigo_unique",
      !is.na(recommended_rows_project) ~ "rut_codigo_ambiguous",
      TRUE ~ "presentado_only"
    ),
    source = "presentado"
  )

presented_keys <- presentados |> filter(!is.na(project_key)) |> distinct(project_key)

recommended_only <- recomendados |>
  anti_join(presented_keys, by = "project_key") |>
  transmute(
    source_file,
    source_path,
    source_sheet,
    source_header_row,
    source_row,
    rut,
    codigo_comap,
    empresa,
    ministerio_evaluador,
    fecha_presentado = as.Date(NA),
    regimen,
    tramo,
    monto_ui,
    tramo_clean,
    es_ampliacion,
    project_key,
    presentado_row_id = NA_integer_,
    fecha_recomendado_exact = fecha_recomendado,
    regimen_recomendado_exact = regimen,
    monto_ui_recomendado_exact = monto_ui,
    ministerio_recomendado_exact = ministerio_evaluador,
    recommended_rows_exact = 1L,
    fecha_recomendado_project = fecha_recomendado,
    regimen_recomendado_project = regimen,
    monto_ui_recomendado_project = monto_ui,
    ministerio_recomendado_project = ministerio_evaluador,
    recommended_rows_project = 1L,
    recommended_tramos = as.integer(tramo_clean != ""),
    can_fallback_to_project = TRUE,
    fecha_recomendado,
    regimen_recomendado = regimen,
    monto_ui_recomendado = monto_ui,
    ministerio_recomendado = ministerio_evaluador,
    match_method = "recomendado_only",
    source = "recomendado_only"
  )

authoritative <- bind_rows(authoritative_from_presentados, recommended_only) |>
  transmute(
    project_row_id = row_number(),
    source,
    match_method,
    rut,
    codigo_comap,
    project_key,
    empresa,
    ministerio_evaluador = coalesce(ministerio_evaluador, ministerio_recomendado),
    fecha_presentado,
    fecha_recomendado,
    regimen = coalesce(regimen_recomendado, regimen),
    tramo,
    es_ampliacion,
    monto_ui_presentado = monto_ui,
    monto_ui_recomendado,
    monto_ui_beneficios = if_else(!is.na(fecha_recomendado), coalesce(monto_ui_recomendado, monto_ui), NA_real_),
    has_benefits = !is.na(fecha_recomendado),
    recommended_rows_for_key = coalesce(recommended_rows_exact, recommended_rows_project),
    source_file,
    source_sheet,
    source_row
  ) |>
  arrange(rut, codigo_comap, fecha_presentado, tramo)

summary_by_ministerio <- authoritative |>
  mutate(ministerio_evaluador = coalesce(ministerio_evaluador, "SIN_DATO")) |>
  group_by(ministerio_evaluador) |>
  summarise(
    filas = n(),
    proyectos = n_distinct(project_key, na.rm = TRUE),
    ampliaciones = sum(es_ampliacion, na.rm = TRUE),
    proyectos_con_beneficios = n_distinct(project_key[has_benefits], na.rm = TRUE),
    filas_con_beneficios = sum(has_benefits, na.rm = TRUE),
    monto_ui_beneficios = sum(monto_ui_beneficios, na.rm = TRUE),
    monto_ui_presentado = sum(monto_ui_presentado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(monto_ui_beneficios), ministerio_evaluador)

linkage_audit <- authoritative |>
  count(match_method, source, name = "rows") |>
  arrange(match_method, source)

write_csv(presentados, file.path(output_dir, "macrobase_presentados_normalized.csv"), na = "")
write_csv(recomendados, file.path(output_dir, "macrobase_recomendados_normalized.csv"), na = "")
write_csv(authoritative, file.path(output_dir, "comap_projects_authoritative.csv"), na = "")
write_csv(summary_by_ministerio, file.path(output_dir, "summary_by_ministerio.csv"), na = "")
write_csv(linkage_audit, file.path(output_dir, "linkage_audit.csv"), na = "")

message("Wrote processed COMAP datasets to ", output_dir)
message("Rows in authoritative set: ", nrow(authoritative))
