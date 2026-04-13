diff_grundlon <- function(old, new) {
  old <- sanitize_nulls_df(old)
  new <- sanitize_nulls_df(new)
  if (!all(c("consultant_id", "grundlon") %in% names(old))) return(data.frame())
  if (!all(c("consultant_id", "grundlon") %in% names(new))) return(data.frame())

  common <- intersect(old$consultant_id, new$consultant_id)
  out <- data.frame(consultant_id = character(0), old_grundlon = numeric(0), new_grundlon = numeric(0), stringsAsFactors = FALSE)

  for (cid in common) {
    ov <- suppressWarnings(as.numeric(old$grundlon[old$consultant_id == cid][1]))
    nv <- suppressWarnings(as.numeric(new$grundlon[new$consultant_id == cid][1]))
    if (is.na(ov) && is.na(nv)) next
    if (!isTRUE(all.equal(ov, nv))) out <- bind_rows(out, data.frame(consultant_id = cid, old_grundlon = ov, new_grundlon = nv))
  }
  out
}

diff_bonus_any <- function(old, new, cols = c("bonus_grund", "group_bonus", "sales_bonus")) {
  old <- sanitize_nulls_df(old)
  new <- sanitize_nulls_df(new)

  if (!("consultant_id" %in% names(old) && "consultant_id" %in% names(new))) return(data.frame())

  cols <- intersect(cols, intersect(names(old), names(new)))
  if (length(cols) == 0) return(data.frame())

  old_k <- old |>
    mutate(consultant_id = as.character(consultant_id)) |>
    select(consultant_id, all_of(cols)) |>
    filter(!is.na(consultant_id), consultant_id != "") |>
    group_by(consultant_id) |>
    slice(1) |>
    ungroup()

  new_k <- new |>
    mutate(consultant_id = as.character(consultant_id)) |>
    select(consultant_id, all_of(cols)) |>
    filter(!is.na(consultant_id), consultant_id != "") |>
    group_by(consultant_id) |>
    slice(1) |>
    ungroup()

  for (cc in cols) {
    old_k[[cc]] <- suppressWarnings(as.numeric(old_k[[cc]]))
    new_k[[cc]] <- suppressWarnings(as.numeric(new_k[[cc]]))
  }

  m <- full_join(old_k, new_k, by = "consultant_id", suffix = c(".old", ".new"))

  out <- data.frame(
    consultant_id = character(0),
    bonus_col = character(0),
    new_value = numeric(0),
    stringsAsFactors = FALSE
  )

  for (cc in cols) {
    a <- m[[paste0(cc, ".old")]]
    b <- m[[paste0(cc, ".new")]]
    changed <- mapply(\(x, y) !isTRUE(all.equal(x, y)), a, b)
    changed[is.na(changed)] <- FALSE

    if (any(changed)) {
      out <- bind_rows(
        out,
        data.frame(
          consultant_id = as.character(m$consultant_id[changed]),
          bonus_col = cc,
          new_value = suppressWarnings(as.numeric(b[changed])),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  out |> distinct()
}

diff_uppgift_timpris <- function(old, new) {
  old <- sanitize_nulls_df(old)
  new <- sanitize_nulls_df(new)

  key <- c("consultant_id", "uppdrag_id", "uppgift_id")
  if (!all(key %in% names(old)) || !all(key %in% names(new))) return(data.frame())

  old <- ensure_uppgift_timpris(old)
  new <- ensure_uppgift_timpris(new)

  old_keys <- old |>
    mutate(across(all_of(key), as.character)) |>
    distinct(across(all_of(key))) |>
    mutate(key_str = paste(consultant_id, uppdrag_id, uppgift_id, sep = "||")) |>
    pull(key_str)

  m <- merge(
    new, old,
    by = key,
    all.x = TRUE,
    suffixes = c(".new", ".old")
  )

  if (!"timpris.new" %in% names(m)) m$timpris.new <- NA_real_
  if (!"timpris.old" %in% names(m)) m$timpris.old <- NA_real_

  m <- m |>
    mutate(
      across(all_of(key), as.character),
      key_str = paste(consultant_id, uppdrag_id, uppgift_id, sep = "||"),
      existed_before = key_str %in% old_keys,
      timpris.new = suppressWarnings(as.numeric(timpris.new)),
      timpris.old = suppressWarnings(as.numeric(timpris.old))
    )

  m$changed <- mapply(\(a, b) !isTRUE(all.equal(a, b)), m$timpris.new, m$timpris.old)
  m$changed[is.na(m$changed)] <- FALSE

  out <- m[m$existed_before == TRUE & m$changed == TRUE, c(key, "timpris.old", "timpris.new")]
  out
}

sync_grundlon_from_history <- function(wb) {
  kons <- sanitize_nulls_df(wb[["Konsulter"]])
  gl <- sanitize_nulls_df(wb[["GrundlonHistory"]]) |> coerce_dates(c("created_at"))

  if (nrow(kons) == 0 || nrow(gl) == 0) return(wb)
  if (!("consultant_id" %in% names(kons) && "consultant_id" %in% names(gl))) return(wb)
  if (!("grundlon" %in% names(gl))) return(wb)

  gl <- gl |>
    mutate(
      consultant_id = as.character(consultant_id),
      grundlon = suppressWarnings(as.numeric(grundlon)),
      created_at = safe_as_date(created_at)
    ) |>
    filter(!is.na(consultant_id)) |>
    arrange(consultant_id, desc(created_at))

  gl_latest <- gl |>
    group_by(consultant_id) |>
    slice(1) |>
    ungroup() |>
    select(consultant_id, grundlon)

  kons <- kons |>
    mutate(consultant_id = as.character(consultant_id)) |>
    left_join(gl_latest, by = "consultant_id", suffix = c("", ".hist")) |>
    mutate(grundlon = ifelse(!is.na(grundlon.hist), grundlon.hist, suppressWarnings(as.numeric(grundlon)))) |>
    select(-grundlon.hist)

  wb[["Konsulter"]] <- kons
  wb
}
