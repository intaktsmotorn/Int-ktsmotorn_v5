next_id <- function(df, prefix) {
  n <- if (!is.null(df) && nrow(df) > 0) nrow(df) + 1 else 1
  sprintf("%s-%03d", prefix, n)
}

next_ids <- function(df, prefix, n) {
  start <- if (!is.null(df) && nrow(df) > 0) nrow(df) + 1 else 1
  sprintf("%s-%03d", prefix, start:(start + n - 1))
}

next_seq_id <- function(existing_ids, prefix, width = 3) {
  ids <- toupper(trimws(as.character(existing_ids)))
  ids <- ids[!is.na(ids) & nzchar(ids)]

  pat <- paste0("^", toupper(prefix), "-(\\d{", width, "})$")
  m <- stringr::str_match(ids, pat)

  nums <- suppressWarnings(as.integer(m[, 2]))
  nums <- nums[!is.na(nums)]

  n <- if (length(nums) == 0) 1L else max(nums) + 1L
  sprintf("%s-%0*d", toupper(prefix), width, n)
}

norm_one_per_id <- function(df, id_col) {
  df <- sanitize_nulls_df(df)
  df[[id_col]] <- trimws(as.character(df[[id_col]]))
  df <- df[!is.na(df[[id_col]]) & df[[id_col]] != "", , drop = FALSE]
  df |>
    group_by(.data[[id_col]]) |>
    slice(1) |>
    ungroup()
}

changed_num <- function(old_vec, new_vec) {
  old_vec <- suppressWarnings(as.numeric(old_vec))
  new_vec <- suppressWarnings(as.numeric(new_vec))
  out <- mapply(function(a, b) {
    if (is.na(a) && is.na(b)) return(FALSE)
    !isTRUE(all.equal(a, b))
  }, old_vec, new_vec)
  out[is.na(out)] <- FALSE
  out
}

make_customer_id <- function(name, existing_ids = character(0)) {
  existing_ids <- toupper(trimws(as.character(existing_ids)))
  existing_ids <- existing_ids[!is.na(existing_ids) & nzchar(existing_ids)]

  nm <- toupper(gsub("[^A-Za-zÅÄÖåäö]", "", name))
  base <- sprintf("CUST-%s", substr(nm, 1, 6))
  base <- toupper(trimws(base))

  if (!(base %in% existing_ids)) return(base)

  i <- 2L
  repeat {
    cand <- sprintf("%s-%d", base, i)
    if (!(cand %in% existing_ids)) return(cand)
    i <- i + 1L
  }
}

make_broker_id <- function(name, existing_ids = character(0)) {
  nm <- toupper(gsub("[^A-Za-zÅÄÖåäö]", "", name))
  base <- sprintf("MAKL-%s", substr(nm, 1, 6))
  if (!(base %in% existing_ids)) return(base)
  i <- 2L
  repeat {
    cand <- sprintf("%s-%d", base, i)
    if (!(cand %in% existing_ids)) return(cand)
    i <- i + 1L
  }
}

generate_tidrapport_id <- function(existing_tid, consultant_id, year, month) {
  existing_ids <- as.character(existing_tid[["tidrapport_id"]])
  tr_nums <- suppressWarnings(as.integer(
    sub("^TR-0*", "", grep("^TR-[0-9]{3,}$", existing_ids, value = TRUE))
  ))
  next_num <- if (length(tr_nums) == 0) 1L else max(tr_nums, na.rm = TRUE) + 1L
  sprintf("TR-%03d", next_num)
}

month_bounds <- function(year, month) {
  y <- as.integer(year)
  m <- as.integer(month)
  first <- as.Date(sprintf("%04d-%02d-01", y, m))
  last <- (first %m+% months(1)) - days(1)
  list(start = as.Date(first), end = as.Date(last))
}

make_labels <- function(wb) {
  kons <- sanitize_nulls_df(wb[["Konsulter"]]) |>
    mutate(consultant_name = str_squish(paste(fornamn, efternamn)))

  kunder <- sanitize_nulls_df(wb[["Kunder"]]) |>
    mutate(customer_name = ifelse(is.na(customer_namn) | customer_namn == "", customer_id, customer_namn))

  makl <- sanitize_nulls_df(wb[["Maklare"]]) |>
    mutate(maklare_name = ifelse(is.na(maklare_namn) | maklare_namn == "", maklare_id, maklare_namn))

  upp <- sanitize_nulls_df(wb[["Uppdrag"]]) |>
    mutate(uppdrag_label = ifelse(is.na(uppdrag_name) | uppdrag_name == "", uppdrag_id, uppdrag_name))

  list(
    kons = kons[, c("consultant_id", "consultant_name", "fornamn", "efternamn")],
    kunder = kunder[, c("customer_id", "customer_name")],
    makl = makl[, c("maklare_id", "maklare_name")],
    upp = upp[, c("uppdrag_id", "uppdrag_label")]
  )
}

id_to_name <- function(df, id_col, name_col, id_vec) {
  map <- setNames(df[[name_col]], df[[id_col]])
  nm <- unname(map[as.character(id_vec)])
  nm[is.na(nm)] <- as.character(id_vec[is.na(nm)])
  nm
}

name_to_id <- function(df, id_col, name_col, name_vec) {
  map <- setNames(df[[id_col]], df[[name_col]])
  id <- unname(map[as.character(name_vec)])
  id[id %in% c(NA, "NA")] <- NA
  id[is.na(id)] <- as.character(name_vec[is.na(id)])
  id
}
