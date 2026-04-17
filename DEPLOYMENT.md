# Deployment till Posit Connect

Denna guide beskriver hur Intaktsmotorn deployas till Posit Connect Cloud (connect.posit.cloud) steg för steg.

---

## Innehållsförteckning

1. [Förutsättningar](#förutsättningar)
2. [Förstå deployment-paketet](#förstå-deployment-paketet)
3. [Miljövariabler på Posit Connect](#miljövariabler-på-posit-connect)
4. [curl på Posit Connect](#curl-på-posit-connect)
5. [Steg-för-steg: Första deployment](#steg-för-steg-första-deployment)
6. [Uppdatera en befintlig app](#uppdatera-en-befintlig-app)
7. [Kontrollera att appen fungerar](#kontrollera-att-appen-fungerar)
8. [Felsökning av deployment](#felsökning-av-deployment)
9. [Vanliga frågor](#vanliga-frågor)

---

## Förutsättningar

Innan deployment behövs:

- [ ] Konto på [connect.posit.cloud](https://connect.posit.cloud)
- [ ] `rsconnect`-paketet installerat: `install.packages("rsconnect")`
- [ ] API-nyckel från Posit Connect (Settings → API Keys)
- [ ] FTP-server tillgänglig från internet (Posit Connect Cloud anropar FTP externt)
- [ ] `Base_data.xlsx` uppladdad till FTP-servern innan första start
- [ ] Uppdaterad `renv.lock` och `manifest.json`

---

## Förstå deployment-paketet

### Vad som **inkluderas** i deploymentpaketet

```
app.R
manifest.json
renv.lock
R/config.R
R/ftp_handler.R
R/ui.R
R/server.R
R/server_add_handlers.R
R/server_bonus_report.R
R/server_refresh.R
R/helpers_core.R
R/helpers_display.R
R/helpers_ensure.R
R/helpers_history.R
R/helpers_hot.R
R/helpers_ids_labels.R
R/helpers_report.R
R/interval_report.R
```

### Vad som **inte ska** inkluderas

| Fil / Mapp          | Anledning                                               |
|---------------------|---------------------------------------------------------|
| `.Renviron`         | Innehåller lösenord — gitignorerad, sätts på servern   |
| `credentials.sqlite`| Skapas automatiskt vid första start på servern          |
| `data/*.xlsx`       | Data lagras på FTP, inte lokalt i appen                 |
| `renv/library/`     | Installeras automatiskt av Posit Connect från renv.lock |
| `data/*_backup_*`   | Backupfiler — lagras på FTP                             |

> **Viktigt:** Kontrollera att `.gitignore` är korrekt ifylld. Kör `git status` innan deploy för att verifiera att inga känsliga filer är inkluderade.

---

## Miljövariabler på Posit Connect

Alla hemligheter och konfigurationer sätts som miljövariabler i Posit Connect — **aldrig** i koden eller i commitade filer.

### Var du sätter dem

1. Logga in på Posit Connect
2. Öppna din publicerade app
3. Gå till **Settings** → **Environment Variables**
4. Klicka på **Add Variable** för varje variabel nedan

### Vilka variabler som behövs

| Variabel                        | Beskrivning                                  | Obligatorisk |
|---------------------------------|----------------------------------------------|:------------:|
| `FTP_USER`                      | FTP-användarnamn                             | ✅           |
| `FTP_PASS`                      | FTP-lösenord                                 | ✅           |
| `FTP_SERVER`                    | FTP-serveradress (t.ex. `server.example.com`)| ✅           |
| `FTP_PATH`                      | FTP-katalogsökväg (med avslutande `/`)       | ✅           |
| `SHINYMANAGER_PASSPHRASE`       | Krypteringsnyckel för credentials-databasen  | ✅           |
| `SHINYMANAGER_ADMIN_PASSWORD`   | Lösenord för admin-kontot                    | ✅           |
| `SHINYMANAGER_SVETLANA_PASSWORD`| Lösenord för svetlana-kontot                 | ✅           |
| `SHINYMANAGER_ROBERT_PASSWORD`  | Lösenord för robert-kontot                   | ✅           |

> **Om `SHINYMANAGER_PASSPHRASE` ändras** efter att `credentials.sqlite` skapats måste databasen återskapas — annars går det inte att logga in.

---

## curl på Posit Connect

FTP-kommunikationen sker via systemets `curl`-binär. På Posit Connect Cloud (Linux) finns `curl` installerat som systemkommando.

Kontrollera i `R/ftp_handler.R`:

```r
CURL_BIN <- if (.Platform$OS.type == "windows") {
  "C:/curl/curl-8.19.0_6-win64-mingw/bin/curl.exe"   # Lokal Windows-sökväg
} else {
  "curl"   # Systemets curl på Linux/Mac — fungerar på Posit Connect
}
```

Posit Connect kör Linux, så `"curl"` används automatiskt. Ingen extra konfiguration behövs.

---

## Steg-för-steg: Första deployment

### Steg 1 — Kontrollera renv.lock

Se till att `renv.lock` speglar aktuella paket:

```r
renv::status()      # Kontrollera avvikelser
renv::snapshot()    # Uppdatera renv.lock vid behov
```

### Steg 2 — Uppdatera manifest.json

`manifest.json` beskriver appen för Posit Connect och måste återskapas om du ändrat paket:

```r
rsconnect::writeManifest()
```

Verifiera att `manifest.json` inkluderar rätt R-version och alla beroenden.

### Steg 3 — Koppla rsconnect till Posit Connect

```r
rsconnect::setAccountInfo(
  name   = "ditt-kontonamn",
  token  = "din-api-token",
  secret = "din-api-hemlighet"
)
```

API-token och hemlighet hämtas från Posit Connect: **Account Settings → API Keys → Add Key**.

### Steg 4 — Publicera appen

```r
rsconnect::deployApp(
  appDir    = ".",          # Projektets rotkatalog
  appName   = "intaktsmotorn",
  forceUpdate = TRUE
)
```

Alternativt via RStudio GUI: Klicka på **Publish**-knappen i app.R eller använd **File → Publish...**.

### Steg 5 — Sätt miljövariabler

På Posit Connect: öppna appen → **Settings** → **Environment Variables** → lägg till alla variabler från tabellen ovan.

### Steg 6 — Ladda upp Base_data.xlsx till FTP

Se till att `Base_data.xlsx` finns på FTP-servern i rätt katalog **innan** appen startas. Annars misslyckas startup-laddningen.

```r
# Verifiera att filen finns — kör lokalt
source("R/ftp_handler.R")
ftp_list()
```

### Steg 7 — Starta om appen

På Posit Connect: **Settings** → **Restart** (eller vänta på automatisk restart efter deploy).

### Steg 8 — Verifiera

1. Öppna appens URL
2. Logga in med ett av de konfigurerade kontona
3. Kontrollera att data laddas (inga FTP-felmeddelanden)
4. Kontrollera att Posit Connect-loggen visar `[FTP] OK`-meddelanden

---

## Uppdatera en befintlig app

Vid kodändringar:

```r
# 1. Kontrollera/uppdatera renv.lock om paket ändrades
renv::snapshot()

# 2. Uppdatera manifest.json om paket ändrades
rsconnect::writeManifest()

# 3. Deploya (uppdaterar befintlig app)
rsconnect::deployApp(appName = "intaktsmotorn", forceUpdate = TRUE)
```

> **Data-kontinuitet:** `Base_data.xlsx` på FTP rörs inte av deployment. All historik och data bevaras.

---

## Kontrollera att appen fungerar

### Posit Connect-loggar

1. Öppna appen på Posit Connect
2. Gå till **Logs** (eller **Runtime Logs**)
3. Kontrollera att inga fel visas vid startup

Förväntade meddelanden vid lyckad start:

```
[FTP] Laddar ner: ftp://server.example.com/sda2/leadpoint/Base_data.xlsx
[FTP] OK — Base_data.xlsx (245.3 KB)
[FTP] Laddar upp: Base_data.xlsx → ftp://server.example.com/sda2/leadpoint/Base_data_backup_1775109739.xlsx
[FTP] OK — uppladdning klar: Base_data_backup_1775109739.xlsx
```

### Snabbtest av FTP-anslutning

Kör lokalt för att verifiera att FTP fungerar med samma credentials som Posit Connect:

```r
source("R/ftp_handler.R")
ftp_test()
```

---

## Felsökning av deployment

### Appen startar men kan inte logga in

**Orsak:** `SHINYMANAGER_PASSPHRASE` matchar inte den passphrase som användes för att skapa `credentials.sqlite`.

**Lösning:** Kontrollera att passphrases stämmer. På Posit Connect skapas `credentials.sqlite` automatiskt vid varje ny deployment (om den saknas). Sätt rätt passphrase-variabel.

### Appen kraschar med FTP-fel

**Orsak 1:** Miljövariablerna `FTP_*` är inte satta på Posit Connect.
→ Kontrollera Settings → Environment Variables.

**Orsak 2:** FTP-servern är inte nåbar från internet.
→ Testa: `curl ftp://servern.com` från ett externt nätverk.

**Orsak 3:** `Base_data.xlsx` finns inte på FTP.
→ Ladda upp filen manuellt.

### Deployment misslyckas med paketfel

```r
# Återställ renv.lock-status
renv::restore()
renv::snapshot()
rsconnect::writeManifest()
```

### "R version mismatch"

`manifest.json` kräver R ≥ 4.5.0. Kontrollera att Posit Connect-kontot kör rätt R-version.

### Appen är tom / alla dropdowns saknar val

`load_from_disk()` misslyckades tyst. Kontrollera Posit Connect-loggar för FTP-relaterade felmeddelanden.

---

## Vanliga frågor

**Kan jag ha flera instanser av appen igång samtidigt?**
Ja, men skriv-lock-mekanismen (`data/Base_data.xlsx.lock`) är lokal per instans och skyddar inte mot parallella skrivningar från **olika** instanser. För produktionsmiljöer med hög belastning bör en databas-backend övervägas.

**Behöver jag ladda upp `credentials.sqlite` vid varje deploy?**
Nej. Om `credentials.sqlite` inte ingår i deploymentpaketet skapas den automatiskt från miljövariablerna. Det är det rekommenderade beteendet.

**Hur byter jag lösenord för en användare?**
Uppdatera miljövariabeln på Posit Connect, radera `credentials.sqlite` från servern (eller starta om med ny passphrase), och starta om appen.

**Hur vet jag vilken version av appen som körs?**
Kontrollera git-commitmeddelandet i versionshanteringen eller titta på deployment-historiken i Posit Connect.
