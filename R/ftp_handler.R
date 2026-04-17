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

ftp_upload <- function(local_path,
                       remote_filename = basename(local_path),
                       remote_path = NULL) {
  creds <- .ftp_creds()
  .ftp_validate(creds)

  if (!file.exists(local_path)) {
    stop(paste("[FTP] Lokal fil saknas:", local_path), call. = FALSE)
  }

  # Om remote_path är angiven används den, annars FTP_PATH från .Renviron
  path       <- if (!is.null(remote_path)) remote_path else creds$path
  remote_url <- paste0("ftp://", creds$server, path, remote_filename)
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
# Genererar ett läsbart backup-filnamn med datum och tid.
# Normal backup:  "Base_data_backup_2026-04-17_14-35-22.xlsx"
# Veckobackup:    "Base_data_backup_2026-04-17_14-35-22_WEEK.xlsx"
#
# Argument:
#   remote_filename  Originalfilnamn (t.ex. "Base_data.xlsx")
#   weekly           TRUE om detta är en veckobackup (suffix _WEEK läggs till)
#
# Returnerar: sträng med backup-filnamn
# =========================================================================

ftp_backup_filename <- function(remote_filename, weekly = FALSE) {
  ts   <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  base <- tools::file_path_sans_ext(remote_filename)
  suffix <- if (isTRUE(weekly)) "_WEEK" else ""
  paste0(base, "_backup_", ts, suffix, ".xlsx")
}

# --- Intern hjälpfunktion: hämta backup-katalog från .Renviron ----------
# Prioritet:
#   1. FTP_BACKUP_PATH i .Renviron (om satt)
#   2. FTP_PATH i .Renviron (samma katalog som huvudfilen — standard)
# Sätt FTP_BACKUP_PATH bara om du vill ha backupar i en separat katalog.

.ftp_backup_path <- function() {
  path <- Sys.getenv("FTP_BACKUP_PATH", unset = "")
  if (!nzchar(path)) {
    # Faller tillbaka på FTP_PATH — backupar hamnar bredvid huvud-filen
    creds <- .ftp_creds()
    path  <- creds$path
  }
  # Säkerställ avslutande snedstreck
  if (!endsWith(path, "/")) path <- paste0(path, "/")
  path
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

# =========================================================================
# ftp_list_dir()
# Listar enbart filnamn (inte metadata) i en angiven FTP-katalog.
# Används internt av backup-rensningen.
#
# Argument:
#   remote_path  Absolut sökväg på FTP-servern (t.ex. "/sda2/leadpoint/")
#                Default: FTP_PATH från .Renviron
#
# Returnerar: character vector med filnamn, eller character(0) vid fel
# =========================================================================

ftp_list_dir <- function(remote_path = NULL) {
  creds <- .ftp_creds()
  .ftp_validate(creds)

  path       <- if (!is.null(remote_path)) remote_path else creds$path
  remote_url <- paste0("ftp://", creds$server, path)

  output <- .run_curl(c(
    .CURL_BASE_ARGS,
    "--connect-timeout", "15",
    "--list-only",                           # returnerar bara filnamn, inte metadata
    "-u", paste0(creds$user, ":", creds$pass),
    remote_url
  ))

  if (.curl_failed(output)) {
    warning(paste("[FTP] Listning misslyckades:", .curl_error_msg(output)))
    return(character(0))
  }

  # Filtrera bort tomma rader
  trimws(output[nzchar(trimws(output))])
}

# =========================================================================
# ftp_delete()
# Raderar en fil på FTP-servern med FTP DELE-kommandot via curl.
#
# Argument:
#   remote_filename  Filnamn att radera (utan sökväg)
#   remote_path      Absolut sökväg på FTP-servern (t.ex. "/sda2/leadpoint/")
#                    Default: FTP_PATH från .Renviron
#
# Returnerar: TRUE (osynligt) om OK, annars stop()
# =========================================================================

ftp_delete <- function(remote_filename, remote_path = NULL) {
  creds <- .ftp_creds()
  .ftp_validate(creds)

  path      <- if (!is.null(remote_path)) remote_path else creds$path
  full_path <- paste0(path, remote_filename)

  # FTP DELE kräver absolut sökväg; curl körs mot rot-URL
  root_url <- paste0("ftp://", creds$server, "/")

  message(sprintf("[FTP] Raderar: %s", full_path))

  output <- .run_curl(c(
    .CURL_BASE_ARGS,
    "-u",  paste0(creds$user, ":", creds$pass),
    "-Q",  paste0("DELE ", full_path),      # raw FTP-kommando
    root_url
  ))

  if (.curl_failed(output)) {
    stop(paste("[FTP] Radering misslyckades:", .curl_error_msg(output)), call. = FALSE)
  }

  message(sprintf("[FTP] Raderad: %s", remote_filename))
  invisible(TRUE)
}

# =========================================================================
# ftp_backup_cleanup()
# Rensar gamla backupfiler i backup-katalogen enligt följande regler:
#   - Behåll ALLA filer som är yngre än days_keep dagar
#   - Behåll veckobackupar (_WEEK i namnet) oavsett ålder
#   - Radera allt annat
#
# Argument:
#   remote_filename  Originalfilnamn (t.ex. "Base_data.xlsx") — används för
#                    att identifiera backupfilerna via namnmönstret
#   backup_path      Backup-katalog på FTP (default: .ftp_backup_path())
#   days_keep        Antal dagar att behålla normala backupar (default: 30)
#
# Returnerar: character vector med raderade filnamn (osynligt)
# =========================================================================

ftp_backup_cleanup <- function(remote_filename, backup_path = NULL, days_keep = 30) {
  if (is.null(backup_path)) backup_path <- .ftp_backup_path()

  message(sprintf("[FTP] Backup-rensning startar i '%s' (behåller %d dagar)", backup_path, days_keep))

  # Lista filer i backup-katalogen
  filenames <- ftp_list_dir(backup_path)
  if (length(filenames) == 0) {
    message("[FTP] Backup-rensning: backup-katalogen är tom eller otillgänglig")
    return(invisible(character(0)))
  }

  # Matcha mot vårt backup-namnmönster:
  # Base_data_backup_2026-04-17_14-35-22.xlsx
  # Base_data_backup_2026-04-17_14-35-22_WEEK.xlsx
  base    <- tools::file_path_sans_ext(remote_filename)
  pattern <- paste0("^", base,
                    "_backup_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}(_WEEK)?\\.(xlsx|XLSX)$")
  backup_files <- filenames[grepl(pattern, filenames, ignore.case = TRUE)]

  if (length(backup_files) == 0) {
    message("[FTP] Backup-rensning: inga backupfiler matchade mönstret")
    return(invisible(character(0)))
  }

  today  <- Sys.Date()
  cutoff <- today - days_keep

  # Bestäm vilka filer som ska raderas
  to_delete <- character(0)
  for (f in backup_files) {
    # Extrahera datumdelen från filnamnet (YYYY-MM-DD)
    date_match <- regmatches(f, regexpr("\\d{4}-\\d{2}-\\d{2}", f))
    if (length(date_match) == 0) next
    file_date <- tryCatch(as.Date(date_match), error = function(e) NA_real_)
    if (is.na(file_date)) next

    is_weekly <- grepl("_WEEK\\.(xlsx|XLSX)$", f, ignore.case = TRUE)
    is_old    <- file_date < cutoff

    # Radera om: äldre än gränsen OCH inte en veckobackup
    if (is_old && !is_weekly) {
      to_delete <- c(to_delete, f)
    }
  }

  if (length(to_delete) == 0) {
    message(sprintf("[FTP] Backup-rensning: inga filer att radera (%d backupar kvar)", length(backup_files)))
    return(invisible(character(0)))
  }

  message(sprintf("[FTP] Backup-rensning: raderar %d av %d filer", length(to_delete), length(backup_files)))

  for (f in to_delete) {
    tryCatch(
      ftp_delete(f, backup_path),
      error = function(e) {
        warning(sprintf("[FTP] Kunde inte radera '%s': %s", f, conditionMessage(e)))
      }
    )
  }

  invisible(to_delete)
}

# =========================================================================
# ftp_create_backup()
# Skapar en backup av en lokal fil på FTP-servern och kör sedan rensning.
#
# Veckobackup skapas automatiskt på söndagar (kan styras med backup_weekday).
#
# Argument:
#   local_path       Sökväg till den lokala filen som ska bli backup
#   remote_filename  Originalfilnamn (t.ex. "Base_data.xlsx") — används för
#                    att namnge backupen och identifiera gamla backupar vid rensning
#   backup_path      Backup-katalog på FTP (default: .ftp_backup_path())
#   days_keep        Antal dagar att behålla normala backupar (default: 30)
#   backup_weekday   Veckodag för veckobackup: 1=mån … 7=sön (default: 7)
#
# Returnerar: backup-filnamnet (osynligt) om OK, annars stop()
# =========================================================================

ftp_create_backup <- function(local_path,
                               remote_filename,
                               backup_path   = NULL,
                               days_keep     = 30,
                               backup_weekday = 7L) {
  if (is.null(backup_path)) backup_path <- .ftp_backup_path()

  # Avgör om det är veckobackup-dag (ISO-veckodag: 1=mån, 7=sön)
  today_wday <- as.integer(format(Sys.Date(), "%u"))
  is_weekly  <- isTRUE(today_wday == as.integer(backup_weekday))

  # Generera läsbart filnamn med datum/tid
  bname <- ftp_backup_filename(remote_filename, weekly = is_weekly)

  # Ladda upp till backup-katalogen
  tryCatch(
    ftp_upload(local_path, bname, remote_path = backup_path),
    error = function(e) {
      stop(sprintf("[FTP] Backup-uppladdning misslyckades (%s): %s", bname, conditionMessage(e)),
           call. = FALSE)
    }
  )

  message(sprintf("[FTP] Backup skapad: %s%s", backup_path, bname))

  # Kör rensning av gamla backupar (fel loggas som varning, stoppar inte appen)
  tryCatch(
    ftp_backup_cleanup(remote_filename, backup_path, days_keep = days_keep),
    error = function(e) {
      warning(sprintf("[FTP] Backup-rensning misslyckades: %s", conditionMessage(e)))
    }
  )

  invisible(bname)
}
