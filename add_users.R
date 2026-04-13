# =============================================================================
# add_users.R
#
# Run this script to add new users to the credentials database.
# The app does NOT need to be stopped — changes take effect on next login.
#
# Usage: source("add_users.R")
# =============================================================================

library(shinymanager)

PASSPHRASE <- Sys.getenv(
  "SHINYMANAGER_PASSPHRASE",
  unset = "Intaktsmotorn-S3kret-2025!"
)
DB_PATH <- "credentials.sqlite"

# ---------------------------------------------------------------------------
# Read existing users
# ---------------------------------------------------------------------------
existing <- read_db_encrypt(DB_PATH, passphrase = PASSPHRASE)
cat("Existing users:", paste(existing$user, collapse = ", "), "\n\n")

# ---------------------------------------------------------------------------
# Define new users to add
# Edit this data frame — add as many rows as needed.
# ---------------------------------------------------------------------------
new_users <- data.frame(
  user     = c("new_user1"),
  password = c("TempPassword@2025!"),
  name     = c("Ny Användare"),
  role     = c("user"),          # "admin" or "user"
  start    = c(NA),
  expire   = c(NA),
  admin    = c(FALSE),           # TRUE = access to ?admin=true panel
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Check for duplicate usernames before writing
# ---------------------------------------------------------------------------
duplicates <- intersect(new_users$user, existing$user)
if (length(duplicates) > 0) {
  stop("These usernames already exist: ", paste(duplicates, collapse = ", "),
       call. = FALSE)
}

# ---------------------------------------------------------------------------
# Merge and write back
# ---------------------------------------------------------------------------
updated <- rbind(existing, new_users)

write_db_encrypt(
  value      = updated,
  conn       = DB_PATH,
  passphrase = PASSPHRASE
)

cat("✓ Users added:", paste(new_users$user, collapse = ", "), "\n")
cat("  Total users now:", nrow(updated), "\n")
