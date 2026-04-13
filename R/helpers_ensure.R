# ===================== ENSURE SHEETS =====================
# These functions guarantee that every required sheet exists in the workbook
# list (wb) with the correct columns and types.  Each function takes a wb list
# and returns it — creating an empty typed data.frame when the sheet is absent,
# or adding any missing columns when it already exists.
#
# Dependencies (defined in app.R before this file is sourced):
#   sanitize_nulls_df(), ensure_cols(), coerce_dates(), safe_as_date()

ensure_fm_sheet <- function(wb) {
  if (is.null(wb[["FaktureringInformation"]])) {
    wb[["FaktureringInformation"]] <- data.frame(
      faktura_mottagare_id = character(0),
      mottagare_typ        = character(0),
      maklare_id           = character(0),
      customer_id          = character(0),
      kontakt_fornamn      = character(0),
      kontakt_efternamn    = character(0),
      email                = character(0),
      avtal_nummer         = character(0),
      referens             = character(0),
      verksamhet           = character(0),
      kommentar            = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    wb[["FaktureringInformation"]] <- ensure_cols(
      wb[["FaktureringInformation"]],
      c("faktura_mottagare_id", "mottagare_typ", "maklare_id", "customer_id",
        "kontakt_fornamn", "kontakt_efternamn", "email", "avtal_nummer",
        "referens", "verksamhet", "kommentar")
    )
  }
  wb
}

ensure_maklare_sheet <- function(wb) {
  if (is.null(wb[["Maklare"]])) {
    wb[["Maklare"]] <- data.frame(
      maklare_id = character(0),
      maklare_namn = character(0),
      kontakt_fornamn = character(0),
      kontakt_efternamn = character(0),
      email = character(0),
      kommentar = character(0),
      created_at = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
  } else {
    wb[["Maklare"]] <- ensure_cols(
      wb[["Maklare"]],
      c("maklare_id", "maklare_namn", "kontakt_fornamn", "kontakt_efternamn", "email", "kommentar", "created_at")
    ) |> coerce_dates(c("created_at"))
  }
  wb
}

ensure_kunder_sheet <- function(wb) {
  if (is.null(wb[["Kunder"]])) {
    wb[["Kunder"]] <- data.frame(
      customer_id = character(0),
      customer_namn = character(0),
      kontakt_fornamn = character(0),
      kontakt_efternamn = character(0),
      email = character(0),
      created_at = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
  } else {
    wb[["Kunder"]] <- ensure_cols(
      wb[["Kunder"]],
      c("customer_id", "customer_namn", "kontakt_fornamn", "kontakt_efternamn", "email", "created_at")
    ) |> coerce_dates(c("created_at"))
  }
  wb
}

ensure_bonus_history_sheet <- function(wb) {
  if (is.null(wb[["BonusHistory"]])) {
    wb[["BonusHistory"]] <- data.frame(
      bonus_id = character(0),
      consultant_id = character(0),
      bonus_grund = numeric(0),
      created_at = as.Date(character(0)),
      kommentar = character(0),
      stringsAsFactors = FALSE
    )
    return(wb)
  }
  bh <- sanitize_nulls_df(wb[["BonusHistory"]])
  bh <- ensure_cols(bh, c("bonus_id", "consultant_id", "bonus_grund", "created_at", "kommentar"))
  bh$bonus_grund <- suppressWarnings(as.numeric(bh$bonus_grund))
  bh$created_at <- safe_as_date(bh$created_at)
  wb[["BonusHistory"]] <- bh
  wb
}

ensure_group_bonus_history_sheet <- function(wb) {
  if (is.null(wb[["GroupBonusHistory"]])) {
    wb[["GroupBonusHistory"]] <- data.frame(
      group_bonus_id = character(0),
      consultant_id = character(0),
      group_bonus = numeric(0),
      created_at = as.Date(character(0)),
      kommentar = character(0),
      stringsAsFactors = FALSE
    )
    return(wb)
  }
  gb <- sanitize_nulls_df(wb[["GroupBonusHistory"]])
  gb <- ensure_cols(gb, c("group_bonus_id", "consultant_id", "group_bonus", "created_at", "kommentar"))
  gb$group_bonus <- suppressWarnings(as.numeric(gb$group_bonus))
  gb$created_at <- safe_as_date(gb$created_at)
  wb[["GroupBonusHistory"]] <- gb
  wb
}

ensure_sales_bonus_history_sheet <- function(wb) {
  if (is.null(wb[["SalesBonusHistory"]])) {
    wb[["SalesBonusHistory"]] <- data.frame(
      sales_bonus_id = character(0),
      consultant_id = character(0),
      customer_id = character(0),
      sales_bonus = numeric(0),
      created_at = as.Date(character(0)),
      kommentar = character(0),
      stringsAsFactors = FALSE
    )
    return(wb)
  }
  sb <- sanitize_nulls_df(wb[["SalesBonusHistory"]])
  sb <- ensure_cols(sb, c("sales_bonus_id", "consultant_id", "customer_id", "sales_bonus", "created_at", "kommentar"))
  sb$sales_bonus <- suppressWarnings(as.numeric(sb$sales_bonus))
  sb$created_at <- safe_as_date(sb$created_at)
  wb[["SalesBonusHistory"]] <- sb
  wb
}

ensure_timpris_history_sheet <- function(wb) {
  if (is.null(wb[["TimprisHistory"]])) {
    wb[["TimprisHistory"]] <- data.frame(
      timpris_id = character(0),
      consultant_id = character(0),
      uppdrag_id = character(0),
      uppgift_id = character(0),
      timpris = numeric(0),
      created_at = as.Date(character(0)),
      kommentar = character(0),
      stringsAsFactors = FALSE
    )
    return(wb)
  }
  th <- sanitize_nulls_df(wb[["TimprisHistory"]])
  th <- ensure_cols(th, c("timpris_id", "consultant_id", "uppdrag_id", "uppgift_id", "timpris", "created_at", "kommentar"))
  th$timpris <- suppressWarnings(as.numeric(th$timpris))
  th$created_at <- safe_as_date(th$created_at)
  wb[["TimprisHistory"]] <- th
  wb
}

ensure_grundlon_history_sheet <- function(wb) {
  if (is.null(wb[["GrundlonHistory"]])) {
    wb[["GrundlonHistory"]] <- data.frame(
      lon_id = character(0),
      consultant_id = character(0),
      grundlon = numeric(0),
      created_at = as.Date(character(0)),
      kommentar = character(0),
      stringsAsFactors = FALSE
    )
    return(wb)
  }
  gl <- sanitize_nulls_df(wb[["GrundlonHistory"]])
  gl <- ensure_cols(gl, c("lon_id", "consultant_id", "grundlon", "created_at", "kommentar"))
  gl$grundlon <- suppressWarnings(as.numeric(gl$grundlon))
  gl$created_at <- safe_as_date(gl$created_at)
  wb[["GrundlonHistory"]] <- gl
  wb
}

ensure_uppgift_sheet <- function(wb) {
  if (is.null(wb[["Uppgift"]])) {
    wb[["Uppgift"]] <- data.frame(
      uppgift_id    = character(0),
      uppgift_name  = character(0),
      consultant_id = character(0),
      uppdrag_id    = character(0),
      customer_id   = character(0),
      startdatum    = as.Date(character(0)),
      timpris       = numeric(0),
      created_at    = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
  } else {
    wb[["Uppgift"]] <- ensure_cols(
      wb[["Uppgift"]],
      c("uppgift_id","uppgift_name","consultant_id","uppdrag_id","customer_id","startdatum","timpris","created_at")
    ) |> coerce_dates(c("startdatum","created_at"))
  }
  wb
}

ensure_bonus_rapport_sheet <- function(wb) {
  if (is.null(wb[["BonusRapportering"]])) {
    wb[["BonusRapportering"]] <- data.frame(
      bonus_rapport_id  = character(0),
      bonus_type        = character(0),
      start_date        = as.Date(character(0)),
      end_date          = as.Date(character(0)),
      rapporterande_id  = character(0),
      bas_consultant_id = character(0),
      customer_id       = character(0),
      uppdrag_id        = character(0),
      uppgift_id        = character(0),
      bonus_procent     = numeric(0),
      reported_hours    = numeric(0),
      timpris_used      = numeric(0),
      invoiced_amount   = numeric(0),
      bonus_amount      = numeric(0),
      created_at        = as.Date(character(0)),
      stringsAsFactors  = FALSE
    )
  } else {
    d <- ensure_cols(
      wb[["BonusRapportering"]],
      c("bonus_rapport_id", "bonus_type", "start_date", "end_date",
        "rapporterande_id", "bas_consultant_id", "customer_id",
        "uppdrag_id", "uppgift_id", "bonus_procent", "reported_hours",
        "timpris_used", "invoiced_amount", "bonus_amount", "created_at")
    ) |> coerce_dates(c("start_date", "end_date", "created_at"))
    for (col in c("bonus_rapport_id", "bonus_type", "rapporterande_id",
                  "bas_consultant_id", "customer_id", "uppdrag_id", "uppgift_id"))
      d[[col]] <- as.character(d[[col]])
    for (col in c("bonus_procent", "reported_hours", "timpris_used",
                  "invoiced_amount", "bonus_amount"))
      d[[col]] <- as.numeric(d[[col]])
    wb[["BonusRapportering"]] <- d
  }
  wb
}

ensure_tid_sheet <- function(wb) {
  if (is.null(wb[["Tidrapportering"]])) {
    wb[["Tidrapportering"]] <- data.frame(
      tidrapport_id = character(0),
      uppgift_id    = character(0),
      customer_id   = character(0),
      timmar        = numeric(0),
      startdatum    = as.Date(character(0)),
      slutdatum     = as.Date(character(0)),
      consultant_id = character(0),
      uppdrag_id    = character(0),
      created_at    = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
  } else {
    wb[["Tidrapportering"]] <- ensure_cols(
      wb[["Tidrapportering"]],
      c("tidrapport_id","uppgift_id","customer_id","timmar","startdatum","slutdatum","consultant_id","uppdrag_id","created_at")
    ) |> coerce_dates(c("startdatum","slutdatum","created_at"))
  }
  wb
}
