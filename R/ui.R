ui <- fluidPage(
  tags$head(tags$style(HTML("
    .topbar { display:flex; align-items:center; justify-content:space-between; gap: 12px; }
    .topbar .title { font-size: 22px; font-weight: 600; margin: 10px 0; }
    .topbar .mid { flex: 1; display:flex; justify-content:center; }
  "))),
  
  tags$div(
    class = "topbar",
    tags$div(class = "title", "Master redigerbar"),
    tags$div(class = "mid", actionButton("btn_save_all", "Spara Alla \u00e4ndringar", class = "btn-success")),
    tags$div(
      style = "display:flex; align-items:center; gap:12px;",
      uiOutput("ui_logged_in_user"),
      actionButton("btn_avsluta", "Avsluta", icon = icon("power-off"), class = "btn-danger")
    )
  ),
  
  tabsetPanel(
    id = "tabs",
    
    tabPanel(
      "Masterdata",
      tabsetPanel(
        tabPanel(
          "Konsulter",
          h4("Lägg till ny konsult"),
          fluidRow(
            column(4,
                   textInput("kon_fornamn", "Förnamn", placeholder = "t.ex. Anna"),
                   textInput("kon_efternamn", "Efternamn", placeholder = "t.ex. Andersson"),
                   selectInput("kon_type", "Typ", choices = c("Anställd", "Underkonsult"), selected = "Anställd")
            ),
            column(4,
                   numericInput("kon_grundlon", "Grundlön (SEK/mån)", value = NA, min = 0, step = 1000),
                   dateInput("kon_start", "Startdatum", value = Sys.Date()),
                   numericInput("kon_bonus_grund", "Bonus grund (%)", value = NA, min = 0, max = 100, step = 0.5),
                   numericInput("kon_group_bonus", "Gruppbonus (%)", value = NA, min = 0, max = 100, step = 0.5),
                   numericInput("kon_sales_bonus", "Säljbonus (%)", value = NA, min = 0, max = 100, step = 0.5)
            ),
            column(4, br(), actionButton("btn_add_konsult", "Lägg till konsult", class = "btn-primary btn-lg"))
          ),
          tags$hr(),
          fluidRow(column(8, offset = 8, actionButton("btn_save_konsulter", "Spara ändringar i konsulter", class = "btn-success"))),
          h4("Redigera befintliga konsulter"),
          rhandsontable::rHandsontableOutput("hot_konsulter")
        ),
        
        tabPanel(
          "Mäklare",
          h4("Lägg till ny mäklare"),
          fluidRow(
            column(3, textInput("makl_namn", "maklare_namn"), textInput("makl_kfor", "kontakt_fornamn"), textInput("makl_kefter", "kontakt_efternamn")),
            column(3, textInput("makl_email", "email"), textInput("makl_kommentar", "kommentar")),
            column(3, br(), actionButton("btn_add_maklare", "Lägg till mäklare", class = "btn-primary"))
          ),
          tags$hr(),
          fluidRow(column(8, offset = 8, actionButton("btn_save_maklare", "Spara ändringar", class = "btn-success"))),
          h4("Redigera mäklare"),
          rhandsontable::rHandsontableOutput("hot_maklare")
        ),
        
        # ===================== KUNDER (NY LOGIK) =====================
        tabPanel(
          "Kunder",
          h4("Lägg till ny kund"),
          fluidRow(
            column(4,
                   textInput("kund_namn", "customer_namn"),
                   textInput("kund_kfor", "kontakt_fornamn"),
                   textInput("kund_kefter", "kontakt_efternamn")
            ),
            column(4,
                   textInput("kund_email", "email"),
                   tags$hr(),
                   h4("Faktura mottagare typ"),
                   radioButtons("kund_fm_typ", NULL, choices = c("Kund", "Mäklare"), selected = "Kund", inline = TRUE),
                   
                   conditionalPanel(
                     condition = "input.kund_fm_typ == 'Mäklare'",
                     selectInput("kund_fm_maklare", "Välj mäklare (för FM-koppling)", choices = NULL)
                   ),
                   
                   conditionalPanel(
                     condition = "input.kund_fm_typ == 'Kund'",
                     tags$div(style="color:#666; margin-top:8px;",
                              "FM-typ 'Kund': mottagaren kopplas till den nya kunden per automatik.")
                   )
            ),
            column(4, br(), actionButton("btn_add_kund", "Lägg till kund", class = "btn-primary"))
          ),
          tags$hr(),
          fluidRow(column(8, offset = 8, actionButton("btn_save_kunder", "Spara ändringar", class = "btn-success"))),
          h4("Redigera kunder"),
          rhandsontable::rHandsontableOutput("hot_kunder")
        ),
        
        tabPanel(
          "Faktura mottagare",
          h4("Lägg till ny fakturamottagare (manuellt)"),
          fluidRow(
            column(
              4,
              radioButtons("fm_typ", "Mottagare typ", c("Kund", "Mäklare"), inline = TRUE),
              selectInput("fm_koppling", "Koppla till (kund/mäklare)", choices = NULL),
              textInput("fm_kfor", "kontakt_fornamn"),
              textInput("fm_kefter", "kontakt_efternamn")
            ),
            column(
              4,
              textInput("fm_email", "email"),
              textInput("fm_avtal", "avtal_nummer"),
              textInput("fm_ref", "referens"),
              textInput("fm_verksamhet", "verksamhet")
            ),
            column(3, br(), actionButton("btn_add_fm", "Lägg till fakturamottagare", class = "btn-primary"))
          ),
          tags$hr(),
          fluidRow(column(8, offset = 8, actionButton("btn_save_fm", "Spara ändringar", class = "btn-success"))),
          h4("Redigera fakturamottagare"),
          rhandsontable::rHandsontableOutput("hot_fm")
        ),
        
        tabPanel(
          "Uppdrag",
          h4("Lägg till nytt uppdrag"),
          fluidRow(
            column(4, textInput("upp_namn", "uppdrag_name"), selectInput("upp_kund", "kund (namn)", choices = NULL), textInput("upp_besk", "beskrivning")),
            column(4, dateInput("upp_start", "startdatum"), dateInput("upp_slut", "slutdatum")),
            column(3, br(), actionButton("btn_add_uppdrag", "Lägg till uppdrag", class = "btn-primary"))
          ),
          tags$hr(),
          fluidRow(column(8, offset = 8, actionButton("btn_save_uppdrag", "Spara ändringar", class = "btn-success"))),
          h4("Redigera uppdrag (customer_name visas read-only)"),
          rhandsontable::rHandsontableOutput("hot_uppdrag")
        ),
        
        tabPanel(
          "Uppgift",
          h4("Lägg till ny uppgift (välj kund → uppdrag filtreras)"),
          fluidRow(
            column(4,
                   selectInput("uppg_kund", "Kund (namn)", choices = NULL),
                   selectInput("uppg_upp", "Uppdrag (namn)", choices = NULL),
                   textInput("uppg_name", "Uppgift namn", placeholder = "t.ex. Data quality / Analys"),
                   selectInput("uppg_kons", "Konsult (namn)", choices = NULL)
            ),
            column(4,
                   numericInput("uppg_timpris", "Timpris (SEK)", value = NA, min = 0, step = 50),
                   dateInput("uppg_start", "Startdatum", value = Sys.Date())
            ),
            column(4, br(), br(), br(), br(), actionButton("btn_add_uppgift", "Lägg till uppgift", class = "btn-primary btn-lg"))
          ),
          tags$hr(),
          fluidRow(column(8, offset = 8, actionButton("btn_save_uppgift", "Spara ändringar", class = "btn-success"))),
          h4("Redigera uppgift (namn/uppdrag/kund visas read-only)"),
          rhandsontable::rHandsontableOutput("hot_uppgift")
        ),
        
        tabPanel(
          "Tidrapportering",
          h4("Lägg till tid med år/månad"),
          fluidRow(
            column(3,
                   selectInput("tid_kund", "Kund (namn)", choices = NULL),
                   selectInput("tid_upp", "Uppdrag (namn)", choices = NULL),
                   selectInput("tid_task", "Uppgift (namn)", choices = NULL)
            ),
            column(3,
                   selectInput("tid_kons", "Konsult (namn)", choices = NULL),
                   numericInput("tid_ar", "År", value = lubridate::year(Sys.Date()), min = 2000, max = 2100),
                   selectInput("tid_manad", "Månad", choices = setNames(1:12, month.name), selected = lubridate::month(Sys.Date()))
            ),
            column(3, numericInput("tid_timmar", "Timmar denna månad", value = NA, min = 0)),
            column(3, br(), actionButton("btn_add_tid", "Lägg till tid", class = "btn-primary"))
          ),
          tags$hr(),
          h4("Filtrera efter Konsult"),
          fluidRow(
            column(3, selectInput("tid_filter_kons", "Konsult", choices = c("Alla" = ""))),
            column(2, br(), actionButton("btn_filter_tid", "Filtrera", class = "btn-info")),
            column(2, br(), actionButton("btn_clear_filter_tid", "Rensa filter", class = "btn-default"))
          ),
          tags$hr(),
          fluidRow(
            column(9, h4("Redigera tidrapportering (namn/uppdrag/kund/uppgift visas read-only)",
                         style = "margin-top:6px;")),
            column(3, actionButton("btn_save_tid", "Spara ändringar", class = "btn-success pull-right"))
          ),
          br(),
          rhandsontable::rHandsontableOutput("hot_tid")
        ),

        # ===================== BONUSRAPPORTERING =====================
        tabPanel(
          "Bonusrapportering",
          h4("Rapportera bonus"),
          fluidRow(
            # --- Column 1: type + period + rapporterande ---
            column(
              3,
              radioButtons("bon_type", "Bonustyp",
                           choices  = c("Gruppbonus", "S\u00e4ljbonus"),
                           selected = "Gruppbonus",
                           inline   = TRUE),
              numericInput("bon_year",  "\u00c5r",
                           value = lubridate::year(Sys.Date()),
                           min = 2000, max = 2100),
              selectInput("bon_month", "M\u00e5nad",
                          choices  = setNames(1:12, month.name),
                          selected = lubridate::month(Sys.Date())),
              selectInput("bon_rapporterande",
                          "Rapporterande person (bonus\u00e4gare)",
                          choices = NULL)
            ),
            # --- Column 2: kund → uppdrag → uppgift → bas-konsult ---
            column(
              3,
              selectInput("bon_kund",    "Kund",    choices = NULL),
              selectInput("bon_uppdrag", "Uppdrag", choices = NULL),
              selectInput("bon_uppgift", "Uppgift", choices = NULL),
              selectInput("bon_bas_kons",
                          "Bas-konsult (vars timmar faktureras)",
                          choices = NULL),
              
            ),
            # --- Column 3: auto-calculated preview + register button ---
            column(
              3,
              h5("Ber\u00e4kning (automatisk)"),
              uiOutput("bon_calc_display"),
              br(),
              actionButton("btn_add_bonus_rapport",
                           "Registrera bonus",
                           class = "btn-primary btn-lg")
            )
          ),
          tags$hr(),
          h4("Filtrera efter Konsult"),
          fluidRow(
            column(3, selectInput("bon_filter_kons", "Konsult", choices = c("Alla" = ""))),
            column(2, br(), actionButton("btn_filter_bon", "Filtrera", class = "btn-info")),
            column(2, br(), actionButton("btn_clear_filter_bon", "Rensa filter", class = "btn-default"))
          ),
          tags$hr(),
          h4("Registrerade bonusrader (read-only)"),
          rhandsontable::rHandsontableOutput("hot_bonus_rapport")
        )
      )
    ),
    
    tabPanel(
      "Intervallrapport",
      h4("Skapa intervallrapport"),
      fluidRow(
        column(
          3,
          numericInput("irep_start_year",  "Start\u00e5r",    value = lubridate::year(Sys.Date()), min = 2000, max = 2100),
          selectInput( "irep_start_month", "Startm\u00e5nad", choices = setNames(1:12, month.name), selected = lubridate::month(Sys.Date())),
          numericInput("irep_end_year",    "Slut\u00e5r",     value = lubridate::year(Sys.Date()), min = 2000, max = 2100),
          selectInput( "irep_end_month",   "Slutm\u00e5nad",  choices = setNames(1:12, month.name), selected = lubridate::month(Sys.Date()))
        ),
        column(
          3,
          br(),
          actionButton("btn_build_interval_report", "Skapa rapport", class = "btn-primary"),
          br(), br(),
          downloadButton("download_interval_report", "Spara som Excel", class = "btn-success")
        ),
        column(6, helpText(
          "Rapporten t\u00e4cker ett intervall av m\u00e5nader.",
          "Tidrapporteringsrader inkluderas om perioden \u00f6verlappar valt intervall.",
          "Timpris\u00e4ndringar i historiken aktiveras fr\u00e5n samma m\u00e5nads start.",
          "Grundl\u00f6ns\u00e4ndringar aktiveras fr\u00e5n 1 jan n\u00e4sta \u00e5r.",
          "Bonustr\u00f6skel till\u00e4mpas per m\u00e5nad."
        ))
      ),
      wellPanel(
        h5("Filtrering av konsulter"),
        radioButtons("irep_filter_mode", label = NULL,
          choices = c(
            "Alla konsulter"      = "all",
            "V\u00e4lj en konsult"     = "one",
            "V\u00e4lj flera konsulter" = "many"
          ),
          selected = "all"
        ),
        conditionalPanel(
          condition = "input.irep_filter_mode == 'one'",
          selectInput("irep_filter_one", "Konsult",
            choices = c("L\u00e4ser in..." = ""), selected = "")
        ),
        conditionalPanel(
          condition = "input.irep_filter_mode == 'many'",
          selectInput("irep_filter_many", "Konsulter",
            choices = c("L\u00e4ser in..." = ""), selected = "", multiple = TRUE)
        )
      ),
      tags$hr(),
      uiOutput("irep_monthly_sections"),
      tags$hr(),
      h4("Totalt f\u00f6r hela intervallet"),
      rhandsontable::rHandsontableOutput("hot_irep_totals"),
      tags$hr(),
      h4("Group Bonus \u2013 Total per m\u00e5nad"),
      rhandsontable::rHandsontableOutput("hot_irep_group_bonus"),
      tags$hr(),
      h4("Sales Bonus \u2013 Total per m\u00e5nad"),
      rhandsontable::rHandsontableOutput("hot_irep_sales_bonus")
    ),

    tabPanel(
      "Historik (read-only)",
      tabsetPanel(
        tabPanel("GrundlonHistory",
          h4("Filtrera efter Konsult"),
          fluidRow(
            column(3, selectInput("gl_filter_kons", "Konsult", choices = c("Alla" = ""))),
            column(2, br(), actionButton("btn_filter_gl", "Filtrera", class = "btn-info")),
            column(2, br(), actionButton("btn_clear_filter_gl", "Rensa filter", class = "btn-default"))
          ),
          tags$hr(),
          rhandsontable::rHandsontableOutput("hot_gl_hist")
        ),
        tabPanel("TimprisHistory",
          h4("Filtrera efter Konsult"),
          fluidRow(
            column(3, selectInput("tp_filter_kons", "Konsult", choices = c("Alla" = ""))),
            column(2, br(), actionButton("btn_filter_tp", "Filtrera", class = "btn-info")),
            column(2, br(), actionButton("btn_clear_filter_tp", "Rensa filter", class = "btn-default"))
          ),
          tags$hr(),
          rhandsontable::rHandsontableOutput("hot_tp_hist")
        ),
        tabPanel("BonusHistory",
          h4("Filtrera efter Konsult"),
          fluidRow(
            column(3, selectInput("bo_filter_kons", "Konsult", choices = c("Alla" = ""))),
            column(2, br(), actionButton("btn_filter_bo", "Filtrera", class = "btn-info")),
            column(2, br(), actionButton("btn_clear_filter_bo", "Rensa filter", class = "btn-default"))
          ),
          tags$hr(),
          rhandsontable::rHandsontableOutput("hot_bo_hist")
        ),
        tabPanel("GroupBonusHistory", rhandsontable::rHandsontableOutput("hot_gb_hist")),
        tabPanel("SalesBonusHistory", rhandsontable::rHandsontableOutput("hot_sb_hist"))
      )
    )
  )
)

