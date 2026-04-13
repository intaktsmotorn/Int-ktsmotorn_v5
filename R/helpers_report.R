# ===================== REPORT HELPERS =====================
# Shared helper functions used by interval_report.R

lookup_timpris_from_uppgift <- function(uppgift_df,
                                        consultant_id,
                                        uppdrag_id,
                                        uppgift_id) {
  u <- sanitize_nulls_df(uppgift_df) |>
    ensure_uppgift_timpris()

  if (!all(c("consultant_id", "uppdrag_id", "uppgift_id", "timpris") %in% names(u))) {
    return(0)
  }

  u$consultant_id <- as.character(u$consultant_id)
  u$uppdrag_id    <- as.character(u$uppdrag_id)
  u$uppgift_id    <- as.character(u$uppgift_id)
  u$timpris       <- suppressWarnings(as.numeric(u$timpris))

  hit <- u[
    u$consultant_id == as.character(consultant_id) &
      u$uppdrag_id  == as.character(uppdrag_id) &
      u$uppgift_id  == as.character(uppgift_id) &
      !is.na(u$timpris),
    , drop = FALSE
  ]

  if (nrow(hit) == 0) return(0)

  if ("created_at" %in% names(hit)) {
    hit <- hit |> coerce_dates(c("created_at"))
    hit <- hit[order(hit$created_at, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  }

  as.numeric(hit$timpris[1])
}
