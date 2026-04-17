# Intaktsmotorn

En intern Shiny-webbapplikation för Leadpoint för hantering av konsulter, kunder, uppdrag, tidrapportering och bonusberäkningar. Data lagras i en Excel-fil som läses och skrivs via FTP.

---

## Innehållsförteckning

1. [Projektöversikt](#projektöversikt)
2. [Teknisk stack](#teknisk-stack)
3. [Funktioner](#funktioner)
4. [Projektstruktur](#projektstruktur)
5. [Installation och lokal utveckling](#installation-och-lokal-utveckling)
6. [FTP-konfiguration](#ftp-konfiguration)
7. [Autentisering](#autentisering)
8. [Köra appen lokalt](#köra-appen-lokalt)
9. [Deployment till Posit Connect](#deployment-till-posit-connect)
10. [Vanliga problem och lösningar](#vanliga-problem-och-lösningar)
11. [Kontakt och vidareutveckling](#kontakt-och-vidareutveckling)

---

## Projektöversikt

Intäktsmotorn är ett internt verktyg för att hantera affärsdata för Leadpoint. Appen ersätter manuell Excel-hantering med ett webbaserat gränssnitt där behöriga användare kan:

- Lägga till och redigera konsulter, kunder, mäklare och uppdrag
- Registrera tidrapportering per konsult, kund och uppgift
- Beräkna och rapportera individuell bonus, gruppbonus och säljbonus
- Generera intervallrapporter (månadsvis) med export till Excel
- Automatiskt logga historikändringar för grundlön, timpris och bonus

All data sparas i en enda Excel-arbetsbok (`Base_data.xlsx`) på en FTP-server, vilket gör att appen fungerar i molnmiljöer som Posit Connect utan tillgång till lokalt filsystem.

---

## Teknisk stack

| Komponent           | Teknik / Paket                     |
|---------------------|------------------------------------|
| Webbramverk         | R Shiny                            |
| Autentisering       | shinymanager (SQLite-databas)      |
| Datakälla           | Excel (readxl / writexl)           |
| Tabellredigering    | rhandsontable                      |
| Databehandling      | dplyr, lubridate, janitor, stringr |
| FTP-kommunikation   | curl (systembinär via `system2()`) |
| Pakethantering      | renv                               |
| Deployment          | Posit Connect Cloud                |
| R-version           | 4.5.3                              |

> **Obs:** Appen använder **inte** R-paketet `RCurl`. All FTP-kommunikation sker via systemets `curl`-binär anropad med `system2()` i `R/ftp_handler.R`.

---

## Funktioner

### Masterdata (redigerbara tabeller)

| Flik                  | Beskrivning                                                     |
|-----------------------|-----------------------------------------------------------------|
| **Konsulter**         | Grunddata, grundlön, bonusprocent. Historik loggas automatiskt. |
| **Mäklare**           | Kontaktuppgifter för mäklare kopplade till kunder.              |
| **Kunder**            | Kundregister med fakturamottagarkoppling.                       |
| **Faktura mottagare** | Separat hantering av faktureringsadresser per kund/mäklare.     |
| **Uppdrag**           | Uppdrag per kund med datum.                                     |
| **Uppgift**           | Uppgifter per uppdrag med timpris. Ändringar loggas i historik. |
| **Tidrapportering**   | Tidregistrering per konsult, kund, uppdrag och uppgift.         |
| **Bonusrapportering** | Registrering av grupp- och säljbonus per period.                |

### Intervallrapport

- Välj start- och slutmånad för rapporten
- Filtrera per konsult (alla / en / flera)
- Månadsvis uppdelning med detalj- och sammanfattningstabell
- Totaltabell för hela perioden
- Export till Excel (`.xlsx`)

### Historik (read-only)

- GrundlonHistory, TimprisHistory, BonusHistory, GroupBonusHistory, SalesBonusHistory
- Ändringar loggas automatiskt vid spara
- Filtrerbar per konsult

### Backup-strategi

- **Startup-backup:** Skapas på FTP vid varje appstart
- **Manuell backup:** Skapas på FTP vid "Spara alla" och "Avsluta"
- Backupfiler namnges: `Base_data_backup_<unix-tidsstämpel>.xlsx`

---

## Projektstruktur

```
Intaktsmotorn_v5/
│
├── app.R                      # Ingångspunkt — laddar allt, startar appen
│
├── R/
│   ├── config.R               # Central konfiguration (sökvägar, konstanter)
│   ├── ftp_handler.R          # FTP-kommunikation via curl
│   ├── ui.R                   # Shiny UI-definition
│   ├── server.R               # Huvud-server (auth, data-laddning, spara)
│   ├── server_add_handlers.R  # Handlers för "Lägg till"-knappar
│   ├── server_bonus_report.R  # Bonusrapporterings-handlers
│   ├── server_refresh.R       # Hjälpare för att uppdatera dropdowns
│   ├── helpers_core.R         # Kärnfunktioner (sanitize, ensure_cols m.m.)
│   ├── helpers_display.R      # Visningskolumner för tabeller
│   ├── helpers_ensure.R       # Initiering av saknade Excel-flikar
│   ├── helpers_history.R      # Historikloggning och difflogik
│   ├── helpers_hot.R          # rhandsontable-rendering
│   ├── helpers_ids_labels.R   # ID/namn-mappning
│   ├── helpers_report.R       # Rapportberäkningar
│   └── interval_report.R      # Byggare av intervallrapport
│
├── data/
│   └── Base_data.xlsx         # Lokal kopia (används som fallback för lock-fil)
│
├── renv/                      # renv-bibliotek (genererat, committas ej)
├── renv.lock                  # Exakta paketversioner — committas
├── manifest.json              # Posit Connect deployment-manifest
│
├── credentials.sqlite         # shinymanager-databas (gitignorerad)
├── add_users.R                # Engångsskript: lägg till användare
├── setup_credentials.R        # Engångsskript: skapa credentials-databas
│
├── .Renviron                  # Miljövariabler (gitignorerad — innehåller lösenord)
├── .Rprofile                  # Aktiverar renv automatiskt
├── .gitignore
└── .lintr                     # Konfiguration för R-linting
```

---

## Installation och lokal utveckling

### Förutsättningar

- **R** version ≥ 4.5.0
- **RStudio** eller **Visual Studio Code** (med R-tillägg) — se jämförelsetabell nedan
- **curl** installerat och åtkomligt (se [FTP-konfiguration](#ftp-konfiguration))
- Nätverksåtkomst till FTP-servern

### IDE-val: RStudio vs Visual Studio Code

| Funktion                            | RStudio                             | VS Code (med R-tillägg)                          |
|-------------------------------------|-------------------------------------|--------------------------------------------------|
| Installation                        | Ladda ned från posit.co/rstudio     | Ladda ned från code.visualstudio.com             |
| R-tillägg                           | Inbyggt                             | `REditorSupport.r` + `RDebugger` från Marketplace|
| renv-stöd                           | Inbyggt, automatisk aktivering      | Aktiveras via `.Rprofile` (automatiskt i projektet)|
| Shiny-förhandsgranskning            | Inbyggt i Viewer-panelen            | Öppnas i extern webbläsare                       |
| Publish-knapp till Posit Connect    | Inbyggt GUI                         | Via terminal: `rsconnect::deployApp()`           |
| R-terminal                          | Inbyggd                             | Inbyggd terminal med `radian` rekommenderas      |
| Git-integration                     | Inbyggt                             | Inbyggt + GitHub Copilot stöds                   |
| Linting (.lintr)                    | Via `lintr`-paketet                 | Via `lintr`-paketet + Diagnostics i editorn      |

**Rekommenderade VS Code-tillägg för detta projekt:**

```
REditorSupport.r        — R-språkstöd (syntax, autocomplettion, hover-docs)
RDebugger               — Debugger för R
ms-vscode.vscode-json   — JSON-stöd (manifest.json, renv.lock)
eamodio.gitlens         — Förbättrad Git-historik
```

Installera tillägg i VS Code: `Ctrl+Shift+X` → sök på tilläggsnamnet → Installera.

### Steg 1 — Klona/kopiera projektet

```bash
git clone https://github.com/intaktsmotorn/Int-ktsmotorn_v5.git
cd Intaktsmotorn_v5
```

### Steg 2 — Öppna projektet i din IDE

**RStudio:** Dubbelklicka på `.Rprofile` eller välj **File → Open Project** och peka på projektmappen. renv aktiveras automatiskt.

**VS Code:** Öppna mappen med `code .` i terminalen (eller **File → Open Folder**). Öppna en R-terminal med `Ctrl+Shift+P` → *R: Create R Terminal*.

### Steg 3 — Återställ paket med renv

```r
# Kör i R-terminalen (RStudio eller VS Code):
renv::restore()
```

Detta installerar exakt de paketversioner som anges i `renv.lock`. Bekräfta med `y` om du tillfrågas.

### Steg 4 — Konfigurera miljövariabler

Skapa filen `.Renviron` i projektets rotkatalog (gitignoreras automatiskt):

```ini
# FTP-uppgifter
FTP_USER=ditt_ftp_användarnamn
FTP_PASS=ditt_ftp_lösenord
FTP_SERVER=din.ftp.server.com
FTP_PATH=/sökväg/till/mapp/

# shinymanager-autentisering
SHINYMANAGER_PASSPHRASE=ditt_hemliga_lösenord

# Lösenord för användare (används om credentials.sqlite saknas)
SHINYMANAGER_ADMIN_PASSWORD=AdminLösenord!
SHINYMANAGER_SVETLANA_PASSWORD=SvetlanaLösenord!
SHINYMANAGER_ROBERT_PASSWORD=RobertLösenord!
```

> **Säkerhet:** `.Renviron` är gitignorerad och ska **aldrig** committas. Dela aldrig lösenord i klartext i versionshanteringssystemet.

### Steg 5 — (Valfritt) Skapa credentials-databas lokalt

Om `credentials.sqlite` inte finns skapas den automatiskt av `config.R` vid första appstarten med lösenord från `.Renviron`. Du kan även köra:

```r
source("setup_credentials.R")
```

---

## FTP-konfiguration

Se den fullständiga guiden i [FTP_SETUP.md](FTP_SETUP.md).

### Snabböversikt

| Miljövariabel       | Beskrivning                                  | Exempel                       |
|---------------------|----------------------------------------------|-------------------------------|
| `FTP_USER`          | FTP-användarnamn                             | `leadpoint`                   |
| `FTP_PASS`          | FTP-lösenord                                 | `hemligtlösenord`             |
| `FTP_SERVER`        | FTP-serveradress (utan protokoll)            | `server.example.com`          |
| `FTP_PATH`          | Sökväg på servern (med avslutande slash)     | `/data/leadpoint/`            |

### Testa anslutningen

```r
source("R/ftp_handler.R")
ftp_test()
```

---

## Autentisering

Appen använder `shinymanager` med en lokal SQLite-databas (`credentials.sqlite`).

### Användare

| Användarnamn | Roll  | Admin |
|--------------|-------|-------|
| `admin`      | admin | Ja    |
| `svetlana`   | admin | Ja    |
| `robert`     | user  | Nej   |

### Credentials på Posit Connect

På Posit Connect finns ingen lokal `credentials.sqlite`. Databasen skapas automatiskt vid deploy med lösenord från miljövariablerna (se [Deployment-guiden](DEPLOYMENT.md)):

```
SHINYMANAGER_PASSPHRASE
SHINYMANAGER_ADMIN_PASSWORD
SHINYMANAGER_SVETLANA_PASSWORD
SHINYMANAGER_ROBERT_PASSWORD
```

---

## Köra appen lokalt

### Från RStudio

Öppna `app.R` och klicka på **Run App**-knappen (grön play-knapp uppe till höger), eller kör i konsolen:

```r
shiny::runApp()
```

Appen öppnas automatiskt i RStudios inbyggda Viewer eller i webbläsaren.

### Från VS Code

Öppna en R-terminal (`Ctrl+Shift+P` → *R: Create R Terminal*) och kör:

```r
# Alternativ 1 — kör appen (öppnas i extern webbläsare)
shiny::runApp()

# Alternativ 2 — med explicit port
shiny::runApp(port = 3838)

# Alternativ 3 — starta appen och håll terminalen fri
shiny::runApp(launch.browser = TRUE)
```

> **Tips VS Code:** Installera tillägget `REditorSupport.r`. Du kan då högerklicka på `app.R` och välja *Run Shiny App* om du konfigurerar en task.

### Från terminalen (utan IDE)

```bash
Rscript -e "shiny::runApp('.', port = 3838)"
```

Logga in med de credentials som angetts i `.Renviron` eller `credentials.sqlite`.

---

## Deployment till Posit Connect

Se den fullständiga steg-för-steg-guiden i [DEPLOYMENT.md](DEPLOYMENT.md).

### Snabbsammanfattning

1. Se till att `renv.lock` och `manifest.json` är uppdaterade
2. Sätt miljövariablerna i Posit Connect (Settings → Environment Variables)
3. Publicera via `rsconnect::deployApp()` (terminal, RStudio eller VS Code) eller via RStudios Publish-knapp

> **Viktigt:** `credentials.sqlite` och `.Renviron` ska **inte** inkluderas i deploymentpaketet. De är gitignorerade och hanteras separat.

---

## Vanliga problem och lösningar

| Problem | Möjlig orsak | Lösning |
|---------|--------------|---------|
| `curl-binären hittades inte` | Fel sökväg i `ftp_handler.R` | Uppdatera `CURL_BIN` i `ftp_handler.R` eller installera curl |
| `Saknar FTP-uppgifter i .Renviron` | `.Renviron` saknas eller är ofullständig | Kontrollera att alla `FTP_*`-variabler är satta |
| `Nedladdning misslyckades: (7)` | FTP-servern nåbar inte | Kontrollera server, port (21), brandvägg |
| `Nedladdad fil är tom eller saknas` | Fil finns inte på FTP, eller fel sökväg | Verifiera `FTP_PATH` och att filen finns på servern |
| Appen visar `Kan inte hämta Excel-filen från FTP` | FTP-fel vid startup | Kontrollera nätverksåtkomst och kör `ftp_test()` |
| `credentials.sqlite` saknas vid deploy | Korrekt — skapas automatiskt | Sätt `SHINYMANAGER_*`-variabler på Posit Connect |
| PASV-problem från Posit Connect | NAT/brandvägg på FTP-servern | Flaggan `--ftp-skip-pasv-ip` hanterar detta automatiskt |
| Tabeller uppdateras inte efter spara | Fliken öppnades inte — HOT är NULL | Öppna fliken och försök spara igen |

---

## Kontakt och vidareutveckling

**Ansvarig:** Robban (Leadpoint)

### Lägga till en ny användare

```r
# Kör lokalt — uppdaterar credentials.sqlite
source("add_users.R")
# Redigera skriptet för att lägga till rätt användaruppgifter
```

För att en ny användare ska fungera på Posit Connect: radera `credentials.sqlite` från servern och starta om appen, eller lägg till användaren via `shinymanager::add_user()`.

### Lägga till ett nytt Excel-ark (sheet)

1. Skapa en `ensure_<sheetname>_sheet(wb)`-funktion i `R/helpers_ensure.R`
2. Anropa funktionen i `load_from_disk()` i `R/server.R`
3. Lägg till ark-namnet i `rv$snap`-listan och `DATE_COLS_MASTER` i `R/config.R`

### Uppdatera paket

```r
renv::update()        # Uppdatera alla paket
renv::snapshot()      # Spara ny renv.lock
rsconnect::writeManifest()  # Uppdatera manifest.json
```
