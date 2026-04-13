# ===================== KONFIG =====================
# Central configuration for the Roban Shiny app.
# All environment-specific constants live here.
# Source this file at the top of app.R.
# NOTE: sourced without local=, so all variables land in .GlobalEnv.

# ---- Autentisering -------------------------------------------------------
# Sourced without local= so DB_PATH and PASSPHRASE land in .GlobalEnv,
# making them visible to shinymanager's observer when it re-evaluates
# check_credentials(DB_PATH, passphrase = PASSPHRASE) via rlang::enexpr.
DB_PATH <- "credentials.sqlite"

PASSPHRASE <- Sys.getenv(
  "SHINYMANAGER_PASSPHRASE",
  unset = "Intaktsmotorn042026!"   # local dev fallback only
)

# On Posit Connect (or any deployment where credentials.sqlite is absent),
# create the database automatically using passwords from environment variables.
# Set ADMIN_PASSWORD, SVETLANA_PASSWORD, ROBERT_PASSWORD in Posit Connect's
# Settings → Environment Variables before deploying.
if (!file.exists(DB_PATH)) {
  .creds <- data.frame(
    user     = c("admin",           "svetlana",          "robert"),
    password = c(
      Sys.getenv("SHINYMANAGER_ADMIN_PASSWORD",    unset = "AdminTemp2026!"),
      Sys.getenv("SHINYMANAGER_SVETLANA_PASSWORD", unset = "SvetlanaTemp2026!"),
      Sys.getenv("SHINYMANAGER_ROBERT_PASSWORD",   unset = "RobertTemp2026!")
    ),
    name     = c("Administrator", "Svetlana", "Standardanv\u00e4ndare"),
    role     = c("admin",         "admin",    "user"),
    start    = as.Date(c("2026-01-01", "2026-01-01", "2026-01-01")),
    expire   = as.Date(c("2099-12-31", "2099-12-31", "2099-12-31")),
    admin    = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  shinymanager::create_db(
    credentials_data = .creds,
    sqlite_path      = DB_PATH,
    passphrase       = PASSPHRASE
  )
  rm(.creds)
}
# --------------------------------------------------------------------------

# Path to the Excel workbook used as the data store.
# Relative to the app working directory (project root when launched via shiny::runApp).
TARGET_XLSX <- file.path("data", "Base_data.xlsx")

# Revenue threshold above which the bonus percentage applies (SEK / month).
BONUS_THRESHOLD <- 100000

# Date columns per sheet — used by normalize_wb_dates() to coerce date fields
# after reading the workbook from disk.
DATE_COLS_MASTER <- list(
  "Konsulter"              = c("startdatum", "slutdatum"),
  "Uppdrag"                = c("startdatum", "slutdatum"),
  "Uppgift"                = c("startdatum", "created_at"),
  "Tidrapportering"        = c("startdatum", "slutdatum", "created_at"),
  "TimprisHistory"         = c("created_at"),
  "GrundlonHistory"        = c("created_at"),
  "BonusHistory"           = c("created_at"),
  "GroupBonusHistory"      = c("created_at"),
  "SalesBonusHistory"      = c("created_at"),
  "Maklare"                = c("created_at"),
  "Kunder"                 = c("created_at"),
  "ArbetstimmarGrund"      = c("date"),
  "FaktureringInformation" = c(),
  "BonusRapportering"      = c("start_date", "end_date", "created_at")
)
