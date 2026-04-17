# FTP-konfiguration och felsökning

Denna guide täcker allt om FTP-integrationen i Intaktsmotorn: hur den fungerar, hur du konfigurerar den, hur du felsöker problem och hur du utökar den.

---

## Innehållsförteckning

1. [Hur FTP-integrationen fungerar](#hur-ftp-integrationen-fungerar)
2. [Konfiguration](#konfiguration)
3. [curl-binären](#curl-binären)
4. [Miljövariabler](#miljövariabler)
5. [FTP-flödet i detalj](#ftp-flödet-i-detalj)
6. [Backup-strategi](#backup-strategi)
7. [Säkerhet](#säkerhet)
8. [Testa anslutningen](#testa-anslutningen)
9. [Felsökning](#felsökning)
10. [API-referens för ftp_handler.R](#api-referens-för-ftp_handlerr)

---

## Hur FTP-integrationen fungerar

Intaktsmotorn använder **inte** lokal fillagring för Excel-data. Istället:

1. Vid appstart **laddas `Base_data.xlsx` ned från FTP** till en temporär lokal fil
2. Data läses in från tempfilen (som sedan raderas)
3. Vid spara **skrivs data till en ny tempfil** och laddas upp till FTP
4. Backuper **skapas och lagras på FTP** med tidsstämpel i filnamnet

Detta gör att appen fungerar i molnmiljöer (Posit Connect) utan persistent lokal lagring.

### Varför curl och inte ett R-paket?

- `curl`-binären hanterar FTP med TLS och NAT-genomströmning robust
- Flaggan `--ftp-skip-pasv-ip` löser PASV-problem när FTP-servern sitter bakom NAT
- Binären finns tillgänglig på alla plattformar (Windows/Linux/Mac)
- Inga extra R-paket behövs för FTP-operationer

---

## Konfiguration

### Fil: R/config.R

```r
# Filnamn på FTP-servern (relativt FTP_PATH)
FTP_REMOTE_FILE <- "Base_data.xlsx"

# Lokal sökväg — används BARA för lock-filen
TARGET_XLSX <- file.path("data", "Base_data.xlsx")
```

### Fil: .Renviron (lokal utveckling)

Skapa `.Renviron` i projektets rotkatalog:

```ini
FTP_USER=ditt_användarnamn
FTP_PASS=ditt_lösenord
FTP_SERVER=din.server.com
FTP_PATH=/sökväg/till/katalog/
```

> `FTP_PATH` **måste** sluta med `/` (t.ex. `/sda2/leadpoint/`).

> `.Renviron` är gitignorerad och ska **aldrig** committas.

---

## curl-binären

### Windows (lokal utveckling)

Standardsökvägen i `ftp_handler.R`:

```r
CURL_BIN <- "C:/curl/curl-8.19.0_6-win64-mingw/bin/curl.exe"
```

**Installera curl på Windows:**

1. Ladda ned från [curl.se/windows](https://curl.se/windows/)
2. Packa upp till `C:/curl/`
3. Verifiera: öppna CMD och kör `C:\curl\curl-8.19.0_6-win64-mingw\bin\curl.exe --version`

Om du väljer en annan sökväg — uppdatera `CURL_BIN` i `R/ftp_handler.R`.

### Linux / Mac / Posit Connect

```r
CURL_BIN <- "curl"   # Systemets curl används automatiskt
```

curl finns förinstallerat på de flesta Linux-system och på Posit Connect Cloud. Verifiera med:

```bash
curl --version
```

### Kontrollera att curl hittas av R

```r
system2("curl", "--version", stdout = TRUE, stderr = TRUE)
```

---

## Miljövariabler

| Variabel      | Beskrivning                                                | Format / Exempel               |
|---------------|------------------------------------------------------------|--------------------------------|
| `FTP_USER`    | Användarnamn för FTP-inloggning                            | `leadpoint`                    |
| `FTP_PASS`    | Lösenord för FTP-inloggning                                | `hemligtlösenord`              |
| `FTP_SERVER`  | Serveradress (utan `ftp://` och utan sökväg)               | `server.example.com`           |
| `FTP_PATH`    | Katalogsökväg på servern — **måste** sluta med `/`         | `/sda2/leadpoint/`             |

Den fullständiga URL som byggs upp internt:

```
ftp://<FTP_SERVER><FTP_PATH><filnamn>
ftp://server.example.com/sda2/leadpoint/Base_data.xlsx
```

### Läsa in miljövariabler

`ftp_handler.R` läser om `.Renviron` vid varje anrop till `.ftp_creds()`. Detta innebär att du kan uppdatera `.Renviron` utan att starta om R-sessionen.

---

## FTP-flödet i detalj

### Vid appstart (`load_from_disk()` i server.R)

```
1. ftp_download("Base_data.xlsx")
   → Skapar tempfil: tempfile(fileext = ".xlsx")
   → Kör: curl --ssl --insecure --ftp-pasv --ftp-skip-pasv-ip ...
          -u user:pass -o /tmp/xxxxx.xlsx ftp://server/path/Base_data.xlsx
   → Returnerar sökväg till tempfil

2. Startup-backup (körs en gång per session):
   ftp_upload(tempfil, "Base_data_backup_1775109739.xlsx")

3. read_wb_clean(tempfil)   ← läser in Excel-data
   on.exit(unlink(tempfil)) ← tempfil raderas automatiskt
```

### Vid spara (write_wb_to_disk() i server.R)

```
1. Skapa lokal lock-fil: data/Base_data.xlsx.lock
   (förhindrar parallella skrivningar i samma process)

2. Skriv workbook till tempfil: tempfile(fileext = ".xlsx")

3. (Om with_backup = TRUE):
   ftp_upload(tempfil, "Base_data_backup_<tidsstämpel>.xlsx")

4. ftp_upload(tempfil, "Base_data.xlsx")
   → Kör: curl --ssl --insecure --ftp-pasv --ftp-skip-pasv-ip ...
          -u user:pass -T /tmp/xxxxx.xlsx ftp://server/path/Base_data.xlsx

5. on.exit(): raderar tempfil och lock-fil
```

### curl-flaggor som används

| Flagga                | Funktion                                                            |
|-----------------------|---------------------------------------------------------------------|
| `--ssl`               | Använd FTPS (FTP med TLS-kryptering)                               |
| `--insecure`          | Hoppa över certifikatvalidering (självutfärdat certifikat)          |
| `--ftp-pasv`          | Passivt FTP-läge (genomgår brandväggar)                            |
| `--ftp-skip-pasv-ip`  | Ignorera IP i PASV-svar — löser NAT-problem med Posit Connect Cloud |
| `--tlsv1.2`           | Använd minst TLS 1.2                                               |
| `--tls-max 1.2`       | Använd max TLS 1.2                                                 |
| `--connect-timeout 30`| Timeout för anslutning (sekunder)                                  |
| `--max-time 120`      | Max total tid per operation (sekunder)                             |

---

## Backup-strategi

### Automatiska backuper

| Tidpunkt              | Backup skapas                  | Filnamn                                    |
|-----------------------|--------------------------------|--------------------------------------------|
| Appstart (en gång)    | Kopia av nuvarande FTP-fil     | `Base_data_backup_<unix-timestamp>.xlsx`   |
| "Spara Alla"-knapp    | Före uppladdning av ny version | `Base_data_backup_<unix-timestamp>.xlsx`   |
| "Avsluta"-knapp       | Före uppladdning av ny version | `Base_data_backup_<unix-timestamp>.xlsx`   |

### Rensa gamla backuper

Backupfiler ackumuleras på FTP. Lista dem med:

```r
source("R/ftp_handler.R")
filer <- ftp_list()
# Filtrera backupfiler
grep("backup", filer, value = TRUE)
```

Ta bort gamla backuper manuellt via FTP-klient (t.ex. FileZilla) eller bygg ett städskript med `ftp_delete()` (ej implementerat — se nedan för hur du kan lägga till det).

---

## Säkerhet

### Lösenordshantering

- FTP-lösenord lagras **aldrig** i kod eller versionshantering
- Lokalt: används `.Renviron` (gitignorerad)
- På Posit Connect: Environment Variables (krypterade)
- Lösenordet skickas till curl som argument (`-u user:pass`) — synligt i processlistan på Linux

### FTP vs SFTP

Appen använder FTP med TLS (`--ssl`). Om servern stöder SFTP (SSH File Transfer Protocol) är det ett säkrare alternativ, men kräver ändringar i `ftp_handler.R` och en annan curl-syntax.

### Certifikatvalidering

`--insecure` används för att hantera självutfärdade certifikat. I produktionsmiljö med ett giltigt certifikat kan denna flagga tas bort från `ftp_handler.R`:

```r
# Ta bort "--insecure" från .CURL_BASE_ARGS om certifikatet är giltigt
.CURL_BASE_ARGS <- c(
  "--ssl", "--ftp-pasv",
  "--ftp-skip-pasv-ip",
  "--tlsv1.2", "--tls-max", "1.2",
  "--connect-timeout", "30",
  "--max-time", "120"
)
```

---

## Testa anslutningen

### Snabbtest

```r
source("R/ftp_handler.R")
ftp_test()
```

Förväntad output:

```
=== FTP-anslutningstest ===
  Server : server.example.com
  Sökväg : /sda2/leadpoint/
  Användare: leadpoint
  Lösenord : **********

  OK: 3 rader i FTP-listning
  Filer:
     Base_data.xlsx
     Base_data_backup_1775109739.xlsx
===========================
```

### Testa nedladdning

```r
source("R/ftp_handler.R")
tmp <- ftp_download("Base_data.xlsx")
cat("Nedladdad till:", tmp, "\n")
cat("Filstorlek:", file.info(tmp)$size, "bytes\n")
file.remove(tmp)
```

### Testa uppladdning

```r
source("R/ftp_handler.R")
# Skapa en testfil
tmp <- tempfile(fileext = ".txt")
writeLines("testinnehåll", tmp)
ftp_upload(tmp, "test_upload.txt")
file.remove(tmp)
```

### Verifiera med extern curl (terminal)

```bash
# Lista filer
curl --ssl --insecure --ftp-pasv --ftp-skip-pasv-ip \
     -u användarnamn:lösenord \
     ftp://server.example.com/sda2/leadpoint/

# Ladda ned
curl --ssl --insecure --ftp-pasv --ftp-skip-pasv-ip \
     -u användarnamn:lösenord \
     -o /tmp/test.xlsx \
     ftp://server.example.com/sda2/leadpoint/Base_data.xlsx
```

---

## Felsökning

### Felkod-tabell (curl-exitkoder)

| curl-felkod | Betydelse                                          | Lösning                                                |
|-------------|----------------------------------------------------|--------------------------------------------------------|
| `(7)`       | Kunde inte ansluta till servern                    | Kontrollera server-adress, port 21, brandvägg          |
| `(9)`       | FTP-åtkomst nekad                                  | Kontrollera FTP_USER och FTP_PASS                      |
| `(23)`      | Kunde inte skriva till lokal fil                   | Kontrollera diskutrymme och rättigheter                |
| `(25)`      | Uppladdning misslyckades (FTP)                     | Kontrollera att FTP_PATH existerar och är skrivbar     |
| `(26)`      | Läsfel (lokal fil saknas vid uppladdning)           | Kontrollera att lokal_path existerar                   |
| `(28)`      | Timeout                                            | Öka `--connect-timeout` eller `--max-time`             |
| `(67)`      | FTP-inloggning nekad                               | Felaktigt lösenord eller användare                     |

### Problem: "Saknar FTP-uppgifter i .Renviron: FTP_USER, FTP_PASS"

```r
# Kontrollera att variablerna är laddade
Sys.getenv("FTP_USER")
Sys.getenv("FTP_PASS")
Sys.getenv("FTP_SERVER")
Sys.getenv("FTP_PATH")

# Ladda om .Renviron utan att starta om R
readRenviron(".Renviron")
```

### Problem: PASV-relaterade anslutningsfel

Symptom: anslutning lyckas men dataöverföring hänger sig.

```
# Flaggan --ftp-skip-pasv-ip är redan aktiverad i .CURL_BASE_ARGS
# Om problemet kvarstår, prova --ftp-port - (aktivt läge)
# Obs: aktiv FTP kräver att servern kan ansluta tillbaka till klienten
```

### Problem: Appen startar lokalt men misslyckas på Posit Connect

Vanligaste orsaker:
1. Miljövariablerna är inte satta på Posit Connect → Sätt dem under Settings → Environment Variables
2. FTP-servern är inte nåbar från internet → Testa utifrån externt nätverk
3. `curl` hittas inte → Kontrollera Posit Connect-loggar, `curl` ska finnas på PATH

### Problem: Filen på FTP är korrupt efter uppladdning

Kontrollera att:
- `writexl::write_xlsx()` lyckades (inga felmeddelanden i loggen)
- Tempfilen skapades och har rätt storlek
- Uppladdningen slutfördes utan curl-fel

```r
# Diagnostik
tmp <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(list(test = data.frame(x = 1:3)), path = tmp)
cat("Tempfilstorlek:", file.info(tmp)$size, "bytes\n")
ftp_upload(tmp, "Base_data_diagnostik.xlsx")
unlink(tmp)
```

---

## API-referens för ftp_handler.R

### `ftp_download(remote_filename, local_path)`

Laddar ned en fil från FTP till en lokal sökväg.

| Parameter         | Typ       | Default                        | Beskrivning                              |
|-------------------|-----------|--------------------------------|------------------------------------------|
| `remote_filename` | character | —                              | Filnamn på FTP (relativt `FTP_PATH`)     |
| `local_path`      | character | `tempfile(fileext = ".xlsx")`  | Lokal destinationssökväg                 |

Returnerar: `local_path` (osynligt) vid lyckat resultat. `stop()` vid fel.

```r
# Exempel
tmp <- ftp_download("Base_data.xlsx")
tmp <- ftp_download("Base_data.xlsx", local_path = "/tmp/min_kopia.xlsx")
```

---

### `ftp_upload(local_path, remote_filename)`

Laddar upp en lokal fil till FTP.

| Parameter         | Typ       | Default                   | Beskrivning                              |
|-------------------|-----------|---------------------------|------------------------------------------|
| `local_path`      | character | —                         | Sökväg till lokal fil                    |
| `remote_filename` | character | `basename(local_path)`    | Destinationsnamn på FTP                  |

Returnerar: `TRUE` (osynligt) vid lyckat resultat. `stop()` vid fel.

```r
# Exempel
ftp_upload("/tmp/ny_data.xlsx", "Base_data.xlsx")
ftp_upload("/tmp/backup.xlsx", "Base_data_backup_123456.xlsx")
```

---

### `ftp_backup_filename(remote_filename)`

Genererar ett unikt backup-filnamn med Unix-tidsstämpel.

```r
ftp_backup_filename("Base_data.xlsx")
# → "Base_data_backup_1775109739.xlsx"
```

---

### `ftp_list()`

Listar filer på FTP-servern.

Returnerar: `character vector` med en rad per fil, eller `character(0)` vid fel.

```r
filer <- ftp_list()
print(filer)
```

---

### `ftp_test()`

Testar FTP-anslutningen och skriver ut diagnostik i konsolen.

Returnerar: `TRUE` (osynligt) om OK, `FALSE` annars.

```r
ftp_test()
```

---

## Lägga till ny FTP-funktion: ftp_delete()

Om du vill kunna radera backupfiler från R kan du lägga till en funktion i `ftp_handler.R`:

```r
ftp_delete <- function(remote_filename) {
  creds <- .ftp_creds()
  .ftp_validate(creds)

  remote_url <- paste0("ftp://", creds$server, creds$path)

  output <- .run_curl(c(
    .CURL_BASE_ARGS,
    "-u", paste0(creds$user, ":", creds$pass),
    "-Q", paste0("DELE ", creds$path, remote_filename),
    remote_url
  ))

  if (.curl_failed(output)) {
    stop(paste("[FTP] Radering misslyckades:", .curl_error_msg(output)), call. = FALSE)
  }

  message(sprintf("[FTP] Raderad: %s", remote_filename))
  invisible(TRUE)
}
```

Använd med försiktighet — FTP-radering är permanent och kan inte ångras.
