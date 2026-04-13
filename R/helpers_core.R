backup_path_same_dir <- function(target_path) {
  ts_int <- as.integer(Sys.time())
  dir <- dirname(target_path)
  base <- tools::file_path_sans_ext(basename(target_path))
  file.path(dir, sprintf("%s_backup_%s.xlsx", base, ts_int))
}

reset_form <- function(session, ids, defaults = list()) {
  for (id in ids) {
    def <- defaults[[id]]
    try(updateTextInput(session, id, value = ifelse(is.null(def), "", def)), silent = TRUE)
    try(updateNumericInput(session, id, value = ifelse(is.null(def), NA, def)), silent = TRUE)
    try(updateDateInput(session, id, value = if (is.null(def) || all(is.na(def))) Sys.Date() else def), silent = TRUE)
    try(updateSelectInput(session, id, selected = if (is.null(def)) NULL else def), silent = TRUE)
    try(updateRadioButtons(session, id, selected = if (is.null(def)) NULL else def), silent = TRUE)
    try(updateCheckboxInput(session, id, value = ifelse(is.null(def), FALSE, isTRUE(def))), silent = TRUE)
    try(updateCheckboxGroupInput(session, id, selected = if (is.null(def)) character(0) else def), silent = TRUE)
  }
}

create_startup_backup <- function(target_path, wb_for_fallback = NULL) {
  bpath <- backup_path_same_dir(target_path)

  ok_copy <- FALSE
  if (file.exists(target_path)) {
    ok_copy <- suppressWarnings(file.copy(target_path, bpath, overwrite = FALSE))
  }

  if (!isTRUE(ok_copy) && !is.null(wb_for_fallback)) {
    try({
      writexl::write_xlsx(wb_for_fallback, path = bpath)
      ok_copy <- TRUE
    }, silent = TRUE)
  }

  list(ok = isTRUE(ok_copy), path = bpath)
}

sanitize_nulls_df <- function(df) {
  if (is.null(df)) return(data.frame(info = character(0), stringsAsFactors = FALSE))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (ncol(df) == 0) return(df)
  for (j in seq_len(ncol(df))) {
    if (is.null(df[[j]])) df[[j]] <- rep(NA_character_, nrow(df))
  }
  df
}

ensure_cols <- function(df, cols, default = NA_character_) {
  df <- sanitize_nulls_df(df)
  for (c in cols) if (!c %in% names(df)) df[[c]] <- rep(default, nrow(df))
  df
}

safe_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))

  if (is.numeric(x)) {
    if (all(x > 1e12, na.rm = TRUE)) {
      return(as.Date(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC")))
    }
    if (all(x > 20000 & x < 60000, na.rm = TRUE)) {
      return(as.Date(x, origin = "1899-12-30"))
    }
    return(as.Date(x, origin = "1970-01-01"))
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  out <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
  miss <- is.na(out) & !is.na(x_chr)
  if (any(miss)) out[miss] <- suppressWarnings(lubridate::dmy(x_chr[miss], quiet = TRUE))
  miss <- is.na(out) & !is.na(x_chr)
  if (any(miss)) out[miss] <- suppressWarnings(lubridate::mdy(x_chr[miss], quiet = TRUE))

  as.Date(out)
}

coerce_dates <- function(df, cols) {
  df <- sanitize_nulls_df(df)
  for (c in intersect(cols, names(df))) df[[c]] <- safe_as_date(df[[c]])
  df
}

coerce_char_cols <- function(df, cols) {
  for (col in intersect(cols, names(df))) df[[col]] <- as.character(df[[col]])
  df
}

safe_as_numeric <- function(x, col_name = NULL) {
  result <- suppressWarnings(as.numeric(x))
  bad <- !is.na(x) & !is.na(as.character(x)) & nzchar(as.character(x)) & is.na(result)
  if (any(bad)) {
    label <- if (!is.null(col_name)) paste0(" i kolumn '", col_name, "'") else ""
    bad_vals <- unique(as.character(x[bad]))
    warning(
      paste0(
        "safe_as_numeric: ", sum(bad), " värde(n)", label,
        " kunde inte konverteras till tal och sätts till NA: ",
        paste(bad_vals, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  result
}

read_wb_clean <- function(path) {
  sheets <- excel_sheets(path)
  wb <- lapply(sheets, function(sh) sanitize_nulls_df(read_excel(path, sheet = sh) |> clean_names()))
  names(wb) <- sheets
  wb
}

normalize_wb_dates <- function(wb) {
  if (is.null(wb)) return(wb)

  date_cols_master <- DATE_COLS_MASTER

  for (nm in intersect(names(date_cols_master), names(wb))) {
    wb[[nm]] <- sanitize_nulls_df(wb[[nm]])
    wb[[nm]] <- coerce_dates(wb[[nm]], date_cols_master[[nm]])
  }
  wb
}
