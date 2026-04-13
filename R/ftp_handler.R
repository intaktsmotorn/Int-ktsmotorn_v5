# =====================================================================
# ftp_handler.R
# Hanterar all FTP-kommunikation för Intaktsmotorn.
#
# Beroenden:
#   - Miljövariabler i .Renviron: FTP_USER, FTP_PASS, FTP_SERVER, FTP_PATH
#   - curl.exe på Windows (sökväg nedan), eller systemets curl på Linux/Mac
#
# Exporterade funktioner:
#   ftp_download(remote_filename, local_path)  → laddar ner en fil
#   ftp_upload(local_path, remote_filename)    → laddar upp en fil
#   ftp_backup_filename(remote_filename)       → genererar backup-filnamn med tidsstämpel
#   ftp_list()                                 → listar filer på FTP-servern
#   ftp_test()                                 → testar att anslutningen fungerar
# =====================================================================

# Sökväg till curl-binären (Windows: explicit sökväg, Linux/Mac: systemets curl)
CURL_BIN <- if (.Platform$OS.type == "windows") {
  "C:/curl/curl-8.19.0_6-win64-mingw/bin/curl.exe"
} else {
  "curl"
}

# --- Intern hjälpfunktion: läs in FTP-credentials från .Renviron ----------

.ftp_creds <- function() {
  # Läser om .Renviron varje gång — säkerställer att ändringar tas upp
  if (file.exists(".Renviron")) readRenviron(".Renviron")
  list(
    user   = Sys.getenv("FTP_USER"),
    pass   = Sys.getenv("FTP_PASS"),
    server = Sys.getenv("FTP_SERVER"),
    path   = Sys.getenv("FTP_PATH")
  )
}

# --- Intern hjälpfunktion: validera att credentials är ifyllda -----------

.ftp_validate <- function(creds) {
  saknas <- names(creds)[vapply(creds, function(x) !nzchar(x), logical(1))]
  if (length(saknas) > 0) {
    stop(paste(
      "Saknar FTP-uppgifter i .Renviron:",
      paste(toupper(paste0("FTP_", saknas)), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Gemensamma curl-flaggor för alla anrop.
# --ftp-skip-pasv-ip: ignorerar det IP som FTP-servern rapporterar i PASV-svaret
#   och använder istället serverns publika IP. Kritiskt när servern sitter
#   bakom NAT (t.ex. hemmarouter) och anropas från Posit Connect Cloud.
.CURL_BASE_ARGS <- c(
  "--ssl", "--insecure", "--ftp-pasv",
  "--ftp-skip-pasv-ip",        # lös NAT/PASV-problem från Posit Connect Cloud
  "--tlsv1.2", "--tls-max", "1.2",
  "--connect-timeout", "30",
  "--max-time", "120"
)

# --- Intern hjälpfunktion: kör curl och returnera output -----------------

.run_curl <- function(args) {
  if (!nzchar(CURL_BIN) || (!file.exists(CURL_BIN) && CURL_BIN != "curl")) {
    stop(paste("curl-binären hittades inte:", CURL_BIN), call. = FALSE)
  }
  system2(CURL_BIN, args = args, stdout = TRUE, stderr = TRUE)
}

# --- Intern hjälpfunktion: kolla om curl-output innehåller felkod ---------

.curl_failed <- function(output) {
  any(grepl("^curl: \\(\\d+\\)", output))
}

.curl_error_msg <- function(output) {
  paste(output[grepl("^curl:", output)], collapse = "\n")
}

# =========================================================================
# ftp_download()
# Laddar ner en fil från FTP-servern till en lokal sökväg.
#
# Argument:
#   remote_filename  Filnamn på FTP-servern (relativ till FTP_PATH)
#   local_path       Lokal destination (default: auto-genererad tempfil)
#
# Returnerar: local_path (osynligt) om det lyckas, annars stop()
# =========================================================================

ftp_download <- function(remote_filename,
                         local_path = tempfile(fileext = ".xlsx")) {
  creds <- .ftp_creds()
  .ftp_validate(creds)

  remote_url <- paste0("ftp://", creds$server, creds$path, remote_filename)
  message(sprintf("[FTP] Laddar ner: %s", remote_url))

  output <- .run_curl(c(
    .CURL_BASE_ARGS,
    "-u", paste0(creds$user, ":", creds$pass),
    "-o", local_path,
    remote_url
  ))

  if (.curl_failed(output)) {
    stop(paste("[FTP] Nedladdning misslyckades:", .curl_error_msg(output)), call. = FALSE)
  }

  if (!file.exists(local_path) || file.info(local_path)$size == 0) {
    stop(paste("[FTP] Nedladdad fil är tom eller saknas:", remote_filename), call. = FALSE)
  }

  message(sprintf("[FTP] OK — %s (%.1f KB)", remote_filename,
                  file.info(local_path)$size / 1024))
  invisible(local_path)
}

# =========================================================================
# ftp_upload()
# Laddar upp en lokal fil till FTP-servern.
#
# Argument:
#   local_path       Sökväg till den lokala filen som ska laddas upp
#   remote_filename  Destinationsnamn på FTP (relativ till FTP_PATH)
#                    Default: samma filnamn som local_path
#
# Returnerar: TRUE (osynligt) om det lyckas, annars stop()
# =========================================================================

ftp_upload <- function(local_path, remote_filename = basename(local_path)) {
  creds <- .ftp_creds()
  .ftp_validate(creds)

  if (!file.exists(local_path)) {
    stop(paste("[FTP] Lokal fil saknas:", local_path), call. = FALSE)
  }

  remote_url <- paste0("ftp://", creds$server, creds$path, remote_filename)
  message(sprintf("[FTP] Laddar upp: %s → %s", basename(local_path), remote_url))

  output <- .run_curl(c(
    .CURL_BASE_ARGS,
    "-u", paste0(creds$user, ":", creds$pass),
    "-T", local_path,
    remote_url
  ))

  if (.curl_failed(output)) {
    stop(paste("[FTP] Uppladdning misslyckades:", .curl_error_msg(output)), call. = FALSE)
  }

  message(sprintf("[FTP] OK — uppladdning klar: %s", remote_filename))
  invisible(TRUE)
}

# =========================================================================
# ftp_backup_filename()
# Genererar ett unikt backup-filnamn med Unix-tidsstämpel.
# T.ex. "Base_data_backup_1775109715.xlsx"
#
# Argument:
#   remote_filename  Originalfilnamn (t.ex. "Base_data.xlsx")
#
# Returnerar: sträng med backup-filnamn
# =========================================================================

ftp_backup_filename <- function(remote_filename) {
  ts_int <- as.integer(Sys.time())
  base   <- tools::file_path_sans_ext(remote_filename)
  paste0(base, "_backup_", ts_int, ".xlsx")
}

# =========================================================================
# ftp_list()
# Listar filer på FTP-servern (råutdata från curl).
#
# Returnerar: character vector med listrader, eller character(0) vid fel
# =========================================================================

ftp_list <- function() {
  creds <- .ftp_creds()
  .ftp_validate(creds)

  remote_url <- paste0("ftp://", creds$server, creds$path)

  output <- .run_curl(c(
    .CURL_BASE_ARGS,
    "--connect-timeout", "15",   # kortare timeout för listning
    "-u", paste0(creds$user, ":", creds$pass),
    remote_url
  ))

  if (.curl_failed(output)) {
    warning(paste("[FTP] Listning misslyckades:", .curl_error_msg(output)))
    return(character(0))
  }

  output
}

# =========================================================================
# ftp_test()
# Testar FTP-anslutningen och skriver ut resultat i konsolen.
# Användbar för felsökning.
#
# Returnerar: TRUE (osynligt) om OK, FALSE annars
# =========================================================================

ftp_test <- function() {
  cat("=== FTP-anslutningstest ===\n")
  creds <- .ftp_creds()
  cat(sprintf("  Server : %s\n", creds$server))
  cat(sprintf("  Sökväg : %s\n", creds$path))
  cat(sprintf("  Användare: %s\n", creds$user))
  cat(sprintf("  Lösenord : %s\n", strrep("*", nchar(creds$pass))))

  tryCatch({
    .ftp_validate(creds)
    filer <- ftp_list()
    cat(sprintf("\n  OK: %d rader i FTP-listning\n", length(filer)))
    if (length(filer) > 0) {
      cat("  Filer:\n")
      for (f in head(filer, 20)) cat("    ", f, "\n")
    }
    cat("===========================\n")
    invisible(TRUE)
  }, error = function(e) {
    cat(sprintf("\n  FEL: %s\n", conditionMessage(e)))
    cat("===========================\n")
    invisible(FALSE)
  })
}
