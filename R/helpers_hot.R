hot_with_date_cols <- function(df,
                               date_cols = c("startdatum", "slutdatum", "created_at", "date"),
                               read_only = FALSE) {
  df2 <- df
  for (dc in intersect(date_cols, names(df2))) {
    d <- safe_as_date(df2[[dc]])
    df2[[dc]] <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
  }

  rownames(df2) <- NULL
  h <- rhandsontable::rhandsontable(df2, stretchH = "all", readOnly = read_only)

  for (dc in intersect(date_cols, names(df2))) {
    h <- rhandsontable::hot_col(h, dc, type = "date", dateFormat = "YYYY-MM-DD", correctFormat = TRUE)
  }
  h
}

ensure_uppgift_timpris <- function(df) {
  if (!"timpris" %in% names(df)) df$timpris <- NA_real_
  df
}

order_tid_cols_for_view <- function(df) {
  df <- sanitize_nulls_df(df)
  wanted <- c(
    "tidrapport_id",
    "consultant_name",
    "customer_name",
    "uppdrag_label",
    "uppgift_name",
    "timmar",
    "startdatum",
    "slutdatum",
    "created_at",
    "uppgift_id"
  )
  keep <- intersect(wanted, names(df))
  rest <- setdiff(names(df), wanted)
  df[, c(keep, rest), drop = FALSE]
}
