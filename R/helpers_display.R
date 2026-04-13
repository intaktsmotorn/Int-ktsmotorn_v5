fm_map_for_customers <- function(wb) {
  fm <- sanitize_nulls_df(wb[["FaktureringInformation"]]) |>
    ensure_cols(c("customer_id", "mottagare_typ", "faktura_mottagare_id"), default = NA_character_) |>
    mutate(
      customer_id = as.character(customer_id),
      mottagare_typ = as.character(mottagare_typ),
      faktura_mottagare_id = as.character(faktura_mottagare_id)
    ) |>
    filter(!is.na(customer_id), customer_id != "")

  fm |>
    group_by(customer_id) |>
    slice(1) |>
    ungroup() |>
    select(customer_id, mottagare_typ, faktura_mottagare_id)
}

add_customer_name_to_uppdrag <- function(df, wb, labels) {
  df <- sanitize_nulls_df(df)

  if (!"customer_id" %in% names(df)) {
    df$customer_name <- NA_character_
    df$faktura_mottagare_typ <- NA_character_
    return(df)
  }

  df$customer_name <- id_to_name(labels$kunder, "customer_id", "customer_name", df$customer_id)

  fm_map <- fm_map_for_customers(wb)

  df <- df |>
    mutate(customer_id = as.character(customer_id)) |>
    left_join(fm_map |> select(customer_id, mottagare_typ), by = "customer_id") |>
    mutate(faktura_mottagare_typ = mottagare_typ) |>
    select(-mottagare_typ)

  df <- df |>
    relocate(customer_name, .after = customer_id) |>
    relocate(faktura_mottagare_typ, .after = customer_name)

  df
}

add_display_cols_to_uppgift <- function(df, wb, labels) {
  df <- sanitize_nulls_df(df) |> ensure_uppgift_timpris()
  if ("consultant_id" %in% names(df)) {
    df$consultant_name <- id_to_name(labels$kons, "consultant_id", "consultant_name", df$consultant_id)
    df <- df |> relocate(consultant_name, .after = consultant_id)
  } else df$consultant_name <- NA_character_

  upp <- sanitize_nulls_df(wb[["Uppdrag"]]) |>
    mutate(uppdrag_label = ifelse(is.na(uppdrag_name) | uppdrag_name == "", uppdrag_id, uppdrag_name)) |>
    mutate(uppdrag_id = as.character(uppdrag_id)) |>
    distinct(uppdrag_id, .keep_all = TRUE)

  if ("uppdrag_id" %in% names(df)) {
    df <- df |>
      mutate(uppdrag_id = as.character(uppdrag_id)) |>
      left_join(upp[, c("uppdrag_id", "uppdrag_label")], by = "uppdrag_id")
    df <- df |> relocate(uppdrag_label, .after = uppdrag_id)
  } else df$uppdrag_label <- NA_character_

  if ("customer_id" %in% names(df)) {
    df$customer_name <- id_to_name(labels$kunder, "customer_id", "customer_name", df$customer_id)
    df <- df |> relocate(customer_name, .after = customer_id)
  } else df$customer_name <- NA_character_

  df
}

add_display_cols_to_tidrapport <- function(df, wb, labels) {
  df <- sanitize_nulls_df(df)

  if ("consultant_id" %in% names(df)) {
    df$consultant_name <- id_to_name(labels$kons, "consultant_id", "consultant_name", df$consultant_id)
    df <- df |> relocate(consultant_name, .after = consultant_id)
  } else df$consultant_name <- NA_character_

  upp <- sanitize_nulls_df(wb[["Uppdrag"]]) |>
    mutate(uppdrag_label = ifelse(is.na(uppdrag_name) | uppdrag_name == "", uppdrag_id, uppdrag_name)) |>
    mutate(uppdrag_id = as.character(uppdrag_id)) |>
    distinct(uppdrag_id, .keep_all = TRUE)

  if ("uppdrag_id" %in% names(df)) {
    df <- df |>
      mutate(uppdrag_id = as.character(uppdrag_id)) |>
      left_join(upp[, c("uppdrag_id", "uppdrag_label")], by = "uppdrag_id")
    df <- df |> relocate(uppdrag_label, .after = uppdrag_id)
  } else df$uppdrag_label <- NA_character_

  if ("customer_id" %in% names(df)) {
    df$customer_name <- id_to_name(labels$kunder, "customer_id", "customer_name", df$customer_id)
    df <- df |> relocate(customer_name, .after = customer_id)
  } else df$customer_name <- NA_character_

  tasks <- sanitize_nulls_df(wb[["Uppgift"]]) |>
    ensure_cols(c("uppgift_id", "uppgift_name")) |>
    mutate(
      uppgift_id = as.character(uppgift_id),
      uppgift_name = ifelse(is.na(uppgift_name) | trimws(uppgift_name) == "", uppgift_id, uppgift_name)
    ) |>
    distinct(uppgift_id, .keep_all = TRUE)

  if ("uppgift_id" %in% names(df)) {
    df <- df |>
      mutate(uppgift_id = as.character(uppgift_id)) |>
      left_join(tasks[, c("uppgift_id", "uppgift_name")], by = "uppgift_id")
  } else df$uppgift_name <- NA_character_

  df
}

drop_display_cols_uppdrag <- function(df, wb, labels) {
  df <- sanitize_nulls_df(df)

  if (!"customer_id" %in% names(df) && "customer_name" %in% names(df)) {
    df$customer_id <- name_to_id(labels$kunder, "customer_id", "customer_name", df$customer_name)
  }

  if (!"faktura_mottagare_id" %in% names(df) && "customer_id" %in% names(df)) {
    fm_map <- fm_map_for_customers(wb)
    df <- df |>
      mutate(customer_id = as.character(customer_id)) |>
      left_join(fm_map |> select(customer_id, faktura_mottagare_id), by = "customer_id")
  }

  drop_cols <- intersect(c("customer_name", "faktura_mottagare_typ"), names(df))
  if (length(drop_cols) > 0) df[drop_cols] <- NULL

  df
}

drop_display_cols_uppgift <- function(df, wb, labels) {
  df <- sanitize_nulls_df(df) |> ensure_uppgift_timpris()

  if (!"consultant_id" %in% names(df) && "consultant_name" %in% names(df)) {
    df$consultant_id <- name_to_id(labels$kons, "consultant_id", "consultant_name", df$consultant_name)
  }
  if (!"uppdrag_id" %in% names(df) && "uppdrag_label" %in% names(df)) {
    df$uppdrag_id <- name_to_id(labels$upp, "uppdrag_id", "uppdrag_label", df$uppdrag_label)
  }
  if (!"customer_id" %in% names(df) && "customer_name" %in% names(df)) {
    df$customer_id <- name_to_id(labels$kunder, "customer_id", "customer_name", df$customer_name)
  }

  if (!"customer_id" %in% names(df) && "uppdrag_id" %in% names(df)) {
    upp <- sanitize_nulls_df(wb[["Uppdrag"]]) |>
      mutate(uppdrag_id = as.character(uppdrag_id), customer_id = as.character(customer_id)) |>
      distinct(uppdrag_id, .keep_all = TRUE)
    df <- df |>
      mutate(uppdrag_id = as.character(uppdrag_id)) |>
      left_join(upp[, c("uppdrag_id", "customer_id")], by = "uppdrag_id")
  }
  drop_cols <- intersect(c("consultant_name", "uppdrag_label", "customer_name"), names(df))
  if (length(drop_cols) > 0) df[drop_cols] <- NULL

  df$timpris <- suppressWarnings(as.numeric(df$timpris))
  df
}

drop_display_cols_tid <- function(df, wb, labels) {
  df <- sanitize_nulls_df(df)

  if (!"consultant_id" %in% names(df) && "consultant_name" %in% names(df)) {
    df$consultant_id <- name_to_id(labels$kons, "consultant_id", "consultant_name", df$consultant_name)
  }
  if (!"uppdrag_id" %in% names(df) && "uppdrag_label" %in% names(df)) {
    df$uppdrag_id <- name_to_id(labels$upp, "uppdrag_id", "uppdrag_label", df$uppdrag_label)
  }
  if (!"customer_id" %in% names(df) && "customer_name" %in% names(df)) {
    df$customer_id <- name_to_id(labels$kunder, "customer_id", "customer_name", df$customer_name)
  }

  if (!"customer_id" %in% names(df) && "uppdrag_id" %in% names(df)) {
    upp <- sanitize_nulls_df(wb[["Uppdrag"]]) |>
      mutate(uppdrag_id = as.character(uppdrag_id), customer_id = as.character(customer_id)) |>
      distinct(uppdrag_id, .keep_all = TRUE)
    df <- df |>
      mutate(uppdrag_id = as.character(uppdrag_id)) |>
      left_join(upp[, c("uppdrag_id", "customer_id")], by = "uppdrag_id")
  }

  drop_cols <- intersect(c("consultant_name", "uppdrag_label", "customer_name", "uppgift_name"), names(df))
  if (length(drop_cols) > 0) df[drop_cols] <- NULL
  df
}
