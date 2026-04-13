# =============================================================================
# setup_credentials.R
#
# Run this script ONCE to create the encrypted SQLite credentials database.
# Do NOT run it every time the app starts — it would overwrite existing users.
#
# Usage:
#   source("setup_credentials.R")   # from the project root in RStudio
#   or: Rscript setup_credentials.R
# =============================================================================

library(shinymanager)

# ---------------------------------------------------------------------------
# Passphrase — used to encrypt the SQLite database.
#
# BEST PRACTICE: never hard-code this in production.
# Set it as an environment variable instead:
#   In .Renviron (local):   SHINYMANAGER_PASSPHRASE=your-secret-here
#   On Posit Cloud:         Settings → Environment Variables
#
# The value below is only used as a LOCAL FALLBACK for initial setup.
# Change it before deploying.
# ---------------------------------------------------------------------------
PASSPHRASE <- Sys.getenv(
  "SHINYMANAGER_PASSPHRASE",
  unset = "Intaktsmotorn042026!"   # <-- change before production
)

if (nchar(PASSPHRASE) < 12) {
  stop("Passphrase is too short. Set SHINYMANAGER_PASSPHRASE to at least 12 characters.")
}

# ---------------------------------------------------------------------------
# Path to the credentials database (relative to project root)
# ---------------------------------------------------------------------------
DB_PATH <- "credentials.sqlite"

if (file.exists(DB_PATH)) {
  stop(
    sprintf("'%s' already exists. Delete it manually if you want to recreate it.", DB_PATH),
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# Define users
#
# Columns:
#   user        — login username (unique)
#   password    — plain text here; shinymanager hashes it automatically
#   name        — display name shown after login
#   role        — custom role used in your server logic ("admin" or "user")
#   start       — account valid from (NA = always)
#   expire      — account expiry date (NA = never)
#   admin       — TRUE grants access to the shinymanager admin panel
# ---------------------------------------------------------------------------
credentials <- data.frame(
  user     = c("admin",           "svetlana",            "robert"),
  password = c("REPLACE_ME",      "REPLACE_ME",       "REPLACE_ME"),
  name     = c("Administrator",   "Svetlana",            "Standardanv\u00e4ndare"),
  role     = c("admin",           "admin",               "user"),
  start    = c(as.Date("2026-01-01"), as.Date("2026-01-01"), as.Date("2026-01-01")),
  expire   = c(as.Date("2099-12-31"), as.Date("2099-12-31"), as.Date("2099-12-31")),
  admin    = c(TRUE,              TRUE,                  FALSE),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Create the encrypted database
# ---------------------------------------------------------------------------
shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path      = DB_PATH,
  passphrase       = PASSPHRASE
)

cat("\n✓ Credentials database created at:", normalizePath(DB_PATH), "\n")
cat("  Users created:", paste(credentials$user, collapse = ", "), "\n")
cat("\n  IMPORTANT: Change all passwords after first login!\n")
cat("  Admin panel available at: http://localhost:<port>/?admin=true\n\n")
