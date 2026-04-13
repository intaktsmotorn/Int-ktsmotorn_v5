server <- function(input, output, session) {

  # ===================== AUTENTISERING =====================
  # secure_server() validates every session against the credentials database.
  # DB_PATH and PASSPHRASE are globals defined in app.R before this is sourced.
  # res_auth is a reactive list — available anywhere in this server function.
  res_auth <- secure_server(
    check_credentials = check_credentials(DB_PATH, passphrase = PASSPHRASE)
  )

  # Display the logged-in user's name and username in the topbar.
  output$ui_logged_in_user <- renderUI({
    req(res_auth$user)
    name     <- res_auth$name %||% res_auth$user
    username <- res_auth$user
    role     <- res_auth$role %||% ""
    tags$span(
      style = "font-size:13px; color:#555; white-space:nowrap;",
      icon("user-circle"), " ",
      tags$strong(name),
      tags$span(style = "color:#aaa;", paste0(" (", username, ")")),
      if (nzchar(role)) tags$span(
        style = "margin-left:6px; padding:2px 7px; border-radius:10px;
                 background:#eaf0fb; color:#2c7be5; font-size:11px;",
        role
      )
    )
  })

  # ===================== REACTIVE VALUES =====================

  rv <- reactiveValues(
    wb = NULL,
    labels = NULL,
    snap = list(),
    interval_report = NULL,
    interval_month_keys = NULL,
    irep_include_bonus = TRUE,
    irep_consultant_filter = NULL,
    tid_task_lookup = NULL,
    startup_backup_done = FALSE
  )
  
  load_from_disk <- function() {
    validate(need(file.exists(TARGET_XLSX), "Kan inte hitta Excel-filen p\u00e5 disk."))
    
    if (!isTRUE(rv$startup_backup_done)) {
      bk <- create_startup_backup(TARGET_XLSX, wb_for_fallback = NULL)
      if (isTRUE(bk$ok)) {
        showNotification(paste("Backup skapad vid start:", basename(bk$path)), type = "message", duration = 4)
      } else {
        showNotification("Kunde inte skapa startup-backup (filen kan vara l\u00e5st eller saknar beh\u00f6righet).", type = "error", duration = 6)
      }
      rv$startup_backup_done <- TRUE
    }
    
    wb <- read_wb_clean(TARGET_XLSX)
    
    wb <- ensure_fm_sheet(wb)
    wb <- ensure_maklare_sheet(wb)
    wb <- ensure_kunder_sheet(wb)
    wb <- ensure_grundlon_history_sheet(wb)
    wb <- ensure_bonus_history_sheet(wb)
    wb <- ensure_group_bonus_history_sheet(wb)
    wb <- ensure_sales_bonus_history_sheet(wb)
    wb <- ensure_timpris_history_sheet(wb)
    wb <- ensure_uppgift_sheet(wb)
    wb <- ensure_tid_sheet(wb)
    wb <- ensure_bonus_rapport_sheet(wb)
    
    wb <- normalize_wb_dates(wb)
    wb <- sync_grundlon_from_history(wb)
    
    rv$wb <- wb
    rv$labels <- make_labels(rv$wb)
    
    for (nm in c(
      "Konsulter","Maklare","Kunder","Uppdrag","Uppgift","Tidrapportering",
      "FaktureringInformation","GrundlonHistory","BonusHistory",
      "GroupBonusHistory","SalesBonusHistory","TimprisHistory","ArbetstimmarGrund",
      "BonusRapportering"
    )) rv$snap[[nm]] <- rv$wb[[nm]]

    # dropdowns
    updateSelectInput(session, "upp_kund", choices = rv$labels$kunder$customer_name)
    updateSelectInput(session, "uppg_kund", choices = rv$labels$kunder$customer_name)
    updateSelectInput(session, "uppg_kons", choices = rv$labels$kons$consultant_name)
    updateSelectInput(session, "tid_kons", choices = rv$labels$kons$consultant_name)
    updateSelectInput(session, "tid_kund", choices = rv$labels$kunder$customer_name)
    updateSelectInput(session, "fm_koppling", choices = rv$labels$kunder$customer_name)
    
    # NY: Mäklare dropdown i Kund-fliken
    updateSelectInput(session, "kund_fm_maklare", choices = rv$labels$makl$maklare_name)

    # Intervallrapport: konsultfilter-dropdowns
    irep_kons_choices <- setNames(rv$labels$kons$consultant_id, rv$labels$kons$consultant_name)
    updateSelectInput(session, "irep_filter_one",  choices = irep_kons_choices)
    updateSelectInput(session, "irep_filter_many", choices = irep_kons_choices)

    # Bonusrapportering: initialise static dropdowns on load.
    # bon_rapporterande is filtered for the default type (Gruppbonus).
    updateSelectInput(session, "bon_kund",
                      choices = rv$labels$kunder$customer_name)
    bon_rap_ch <- bon_filtered_consultant_names(rv$wb, "Gruppbonus")
    updateSelectInput(session, "bon_rapporterande",
                      choices  = bon_rap_ch,
                      selected = if (length(bon_rap_ch) > 0L) bon_rap_ch[1L] else NULL)
    
    # förvälj senaste kund i tid_kund
    kunder_df <- sanitize_nulls_df(rv$wb[["Kunder"]])
    if (nrow(kunder_df) > 0) {
      latest_row <- kunder_df[nrow(kunder_df), , drop = FALSE]
      latest_name <- latest_row$customer_namn
      if (is.na(latest_name) || trimws(latest_name) == "") latest_name <- latest_row$customer_id
      updateSelectInput(session, "tid_kund", choices = rv$labels$kunder$customer_name, selected = latest_name)
    }
  }
  
  observeEvent(TRUE, { load_from_disk() }, once = TRUE)
  
  # ---------- FM: val av koppling baserat på typ (manuell FM-flik) ----------
  observeEvent(input$fm_typ, {
    req(rv$labels)
    if (identical(input$fm_typ, "M\u00e4klare")) {
      updateSelectInput(session, "fm_koppling", choices = rv$labels$makl$maklare_name)
    } else {
      updateSelectInput(session, "fm_koppling", choices = rv$labels$kunder$customer_name)
    }
  })
  
  # ---------- Uppgift: filtrera uppdrag per kund ----------
  observeEvent(input$uppg_kund, {
    req(rv$wb, rv$labels, input$uppg_kund)
    cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$uppg_kund)
    upp <- sanitize_nulls_df(rv$wb[["Uppdrag"]])
    
    if (!"customer_id" %in% names(upp)) {
      updateSelectInput(session, "uppg_upp", choices = rv$labels$upp$uppdrag_label)
      return()
    }
    
    upp_f <- upp |>
      filter(as.character(customer_id) == as.character(cust_id)) |>
      mutate(uppdrag_label = ifelse(is.na(uppdrag_name) | uppdrag_name == "", uppdrag_id, uppdrag_name))
    
    choices <- unique(upp_f$uppdrag_label)
    if (length(choices) == 0) choices <- character(0)
    updateSelectInput(session, "uppg_upp", choices = choices, selected = if (length(choices) > 0) choices[1] else NULL)
  }, ignoreInit = TRUE)
  
  # ---------- Tidrapport: filtrera uppdrag per kund ----------
  observeEvent(input$tid_kund, {
    req(rv$wb, rv$labels, input$tid_kund)
    cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$tid_kund)
    
    upp <- sanitize_nulls_df(rv$wb[["Uppdrag"]]) |>
      mutate(
        uppdrag_label = ifelse(is.na(uppdrag_name) | uppdrag_name == "", uppdrag_id, uppdrag_name),
        customer_id = as.character(customer_id)
      )
    
    upp_f <- upp |> filter(customer_id == as.character(cust_id))
    choices <- unique(upp_f$uppdrag_label)
    if (length(choices) == 0) choices <- character(0)
    
    updateSelectInput(session, "tid_upp", choices = choices, selected = if (length(choices) > 0) choices[1] else NULL)
  }, ignoreInit = TRUE)
  
  # ---------- Tidrapport: filtrera uppgift per kund + uppdrag ----------
  observeEvent(list(input$tid_kund, input$tid_upp), {
    req(rv$wb, rv$labels)
    
    if (is.null(input$tid_kund) || !nzchar(input$tid_kund) ||
        is.null(input$tid_upp)  || !nzchar(input$tid_upp)) {
      updateSelectInput(session, "tid_task", choices = character(0), selected = NULL)
      return()
    }
    
    cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$tid_kund)
    upp_id  <- name_to_id(rv$labels$upp, "uppdrag_id", "uppdrag_label", input$tid_upp)
    #EXTRA!!!
    tasks <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
      ensure_cols(c("uppgift_id","uppgift_name","customer_id","uppdrag_id")) |>
      mutate(
        customer_id = as.character(customer_id),
        uppdrag_id = as.character(uppdrag_id),
        consultant_id = as.character(consultant_id),
        uppgift_id = as.character(uppgift_id),
        uppgift_name = ifelse(is.na(uppgift_name) | trimws(uppgift_name) == "", uppgift_id, uppgift_name),
        task_label = paste0(uppgift_name, " (", uppgift_id, ")")
      ) |>
      filter(customer_id == as.character(cust_id), uppdrag_id == as.character(upp_id))
    rv$tid_task_lookup <- tasks |> select(task_label, uppgift_id, consultant_id)
    #EXTRA!!!
    choices <- unique(tasks$task_label)
    if (length(choices) == 0) choices <- character(0)
    updateSelectInput(session, "tid_task", choices = choices, selected = if (length(choices) > 0) choices[1] else NULL)
  }, ignoreInit = TRUE)
  
  #Ny filtrering efter konsult
  observeEvent(input$tid_task, {
    req(rv$wb, rv$labels)
    
    # Om inget val -> visa alla
    if (is.null(input$tid_task) || !nzchar(input$tid_task)) {
      updateSelectInput(session, "tid_kons", choices = rv$labels$kons$consultant_name)
      return()
    }
    
    # Hitta consultant_id för vald uppgift
    lookup <- sanitize_nulls_df(rv$tid_task_lookup)
    if (nrow(lookup) == 0) {
      updateSelectInput(session, "tid_kons", choices = rv$labels$kons$consultant_name)
      return()
    }
    
    cid <- lookup$consultant_id[match(input$tid_task, lookup$task_label)]
    cid <- as.character(cid)
    cid <- cid[!is.na(cid) & cid != ""]
    
    if (length(cid) == 0) {
      updateSelectInput(session, "tid_kons", choices = rv$labels$kons$consultant_name)
      return()
    }
    
    # Map to consultant_name och filtrera dropdown
    cons_names <- id_to_name(rv$labels$kons, "consultant_id", "consultant_name", cid)
    cons_names <- unique(cons_names)
    
    updateSelectInput(session, "tid_kons", choices = cons_names, selected = cons_names[1])
  }, ignoreInit = TRUE)
  #NY!!!
  # ===================== HOT OUTPUTS =====================
  
  output$hot_konsulter <- rhandsontable::renderRHandsontable({
    req(rv$wb)
    df <- sanitize_nulls_df(rv$wb[["Konsulter"]]) |> coerce_dates(c("startdatum", "slutdatum"))
    for (bc in c("bonus_grund", "group_bonus", "sales_bonus")) {
      if (!bc %in% names(df)) df[[bc]] <- NA_real_
      df[[bc]] <- suppressWarnings(as.numeric(df[[bc]]))
    }
    if ("grundlon" %in% names(df)) df$grundlon <- suppressWarnings(as.numeric(df$grundlon))
    hot_with_date_cols(df, c("startdatum", "slutdatum"))
  })
  
  output$hot_maklare <- rhandsontable::renderRHandsontable({
    req(rv$wb)
    df <- ensure_cols(rv$wb[["Maklare"]], c("maklare_id","maklare_namn","kontakt_fornamn","kontakt_efternamn","email","kommentar","created_at"))
    hot_with_date_cols(df, c("created_at"))
  })
  
  output$hot_kunder <- rhandsontable::renderRHandsontable({
    req(rv$wb)
    df <- ensure_cols(rv$wb[["Kunder"]], c("customer_id","customer_namn","kontakt_fornamn","kontakt_efternamn","email","created_at"))
    hot_with_date_cols(df, c("created_at"))
  })
  
  output$hot_fm <- rhandsontable::renderRHandsontable({
    req(rv$wb)
    rhandsontable::rhandsontable(sanitize_nulls_df(rv$wb[["FaktureringInformation"]]), stretchH = "all")
  })
  #NY!!!
  output$hot_uppdrag <- rhandsontable::renderRHandsontable({
    req(rv$wb, rv$labels)
    
    df <- sanitize_nulls_df(rv$wb[["Uppdrag"]]) |>
      coerce_dates(c("startdatum", "slutdatum")) |>
      add_customer_name_to_uppdrag(rv$wb, rv$labels)

    if ("uppdrag_id" %in% names(df))
      df <- df[order(-as.integer(sub(".*-(\\d+)$", "\\1", df$uppdrag_id)), na.last = TRUE), ]

    # Dölj i UI
    for (col in c("customer_id", "faktura_mottagare_id")) {
      if (col %in% names(df)) df[[col]] <- NULL
    }
    
    h <- hot_with_date_cols(df, c("startdatum", "slutdatum"))
    
    # Read-only på visningskolumner
    for (nm in intersect(c("customer_name", "faktura_mottagare_typ"), names(df))) {
      h <- rhandsontable::hot_col(h, nm, readOnly = TRUE)
    }
    
    h
  })
  #NY!!!
  output$hot_uppgift <- rhandsontable::renderRHandsontable({
    req(rv$wb, rv$labels)
    df <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
      ensure_uppgift_timpris() |>
      coerce_dates(c("startdatum","created_at")) |>
      add_display_cols_to_uppgift(rv$wb, rv$labels)
    
    df <- ensure_cols(df, c("uppgift_name"), default = NA_character_)
    if ("uppgift_id" %in% names(df) && "uppgift_name" %in% names(df)) df <- df |> relocate(uppgift_name, .after = uppgift_id)

    df <- df[order(-as.integer(sub(".*-(\\d+)$", "\\1", df$uppgift_id)), na.last = TRUE), ]

    for (col in c("consultant_id","uppdrag_id","customer_id","slutdatum")) if (col %in% names(df)) df[[col]] <- NULL

    h <- hot_with_date_cols(df, c("startdatum","created_at"))
    for (nm in intersect(c("consultant_name","uppdrag_label","customer_name","created_at"), names(df))) h <- rhandsontable::hot_col(h, nm, readOnly = TRUE)
    h
  })

  # ---- Tidrapportering consultant filter ----
  tid_kons_filter <- reactiveVal("")

  observe({
    req(rv$labels$kons)
    choices <- c("Alla" = "", setNames(
      rv$labels$kons$consultant_name,
      rv$labels$kons$consultant_name
    ))
    updateSelectInput(session, "tid_filter_kons", choices = choices)
  })

  observeEvent(input$btn_filter_tid, {
    tid_kons_filter(input$tid_filter_kons)
  })

  observeEvent(input$btn_clear_filter_tid, {
    tid_kons_filter("")
    updateSelectInput(session, "tid_filter_kons", selected = "")
  })

  output$hot_tid <- rhandsontable::renderRHandsontable({
    req(rv$wb, rv$labels)
    df <- sanitize_nulls_df(rv$wb[["Tidrapportering"]]) |>
      coerce_dates(c("startdatum","slutdatum","created_at")) |>
      add_display_cols_to_tidrapport(rv$wb, rv$labels)

    df <- df[order(
      is.na(df$created_at),
      desc(df$created_at),
      desc(df$tidrapport_id)
    ), ]

    if (nzchar(tid_kons_filter())) {
      df <- df[!is.na(df$consultant_name) & df$consultant_name == tid_kons_filter(), ]
    }

    df <- order_tid_cols_for_view(df)

    for (cc in c("consultant_id","uppdrag_id","customer_id")) if (cc %in% names(df)) df[[cc]] <- NULL

    h <- hot_with_date_cols(df, c("startdatum","slutdatum","created_at"))
    for (nm in intersect(c("consultant_name","uppdrag_label","customer_name","uppgift_name","created_at"), names(df))) h <- rhandsontable::hot_col(h, nm, readOnly = TRUE)
    h
  })
  
  # ===================== INTERVALLRAPPORT =====================

  observeEvent(input$btn_build_interval_report, {
    req(rv$wb, rv$labels,
        input$irep_start_year, input$irep_start_month,
        input$irep_end_year,   input$irep_end_month)

    sy <- as.integer(input$irep_start_year);  sm <- as.integer(input$irep_start_month)
    ey <- as.integer(input$irep_end_year);    em <- as.integer(input$irep_end_month)

    start_d <- as.Date(sprintf("%04d-%02d-01", sy, sm))
    end_d   <- as.Date(sprintf("%04d-%02d-01", ey, em))
    if (start_d > end_d) {
      showNotification("Startm\u00e5nad m\u00e5ste vara f\u00f6re eller lika med slutm\u00e5nad.", type = "error", duration = 4)
      return()
    }

    consultant_filter <- switch(input$irep_filter_mode,
      "one"  = { v <- input$irep_filter_one;  if (!is.null(v) && nzchar(v)) v else NULL },
      "many" = { v <- input$irep_filter_many; if (length(v) > 0) v else NULL },
      NULL  # "all"
    )

    rv$interval_report          <- build_interval_report(
      rv$wb, rv$labels, sy, sm, ey, em,
      bonus_threshold   = BONUS_THRESHOLD,
      consultant_filter = consultant_filter
    )
    rv$irep_include_bonus      <- is.null(consultant_filter)
    rv$irep_consultant_filter  <- consultant_filter

    # Register one pair of renderers per month key (YYYYMM string).
    # Using local() captures mk by value so each closure refers to its own month.
    month_keys <- sort(names(rv$interval_report$arb_hours_by_month))
    rv$interval_month_keys <- month_keys

    for (mk in month_keys) {
      local({
        .mk <- mk
        output[[paste0("hot_irep_d_", .mk)]] <- rhandsontable::renderRHandsontable({
          req(rv$interval_report)
          df <- rv$interval_report$detail
          df_m <- df[format(as.Date(df$period_date), "%Y%m") == .mk, , drop = FALSE]
          hot_with_date_cols(df_m, "period_date", read_only = TRUE)
        })
        output[[paste0("hot_irep_s_", .mk)]] <- rhandsontable::renderRHandsontable({
          req(rv$interval_report)
          df <- rv$interval_report$summary
          df_m <- df[format(as.Date(df$period_date), "%Y%m") == .mk, , drop = FALSE]
          hot_with_date_cols(df_m, "period_date", read_only = TRUE)
        })
      })
    }
  })

  # Builds one visible section per month from the registered month keys.
  output$irep_monthly_sections <- renderUI({
    req(rv$interval_report, rv$interval_month_keys)
    month_keys <- rv$interval_month_keys
    arb_map    <- rv$interval_report$arb_hours_by_month
    mgs        <- rv$interval_report$monthly_grand_summaries  # [+]

    fmt_kr  <- function(x) format(round(x),    big.mark = "\u00a0", scientific = FALSE)
    fmt_h   <- function(x) format(round(x, 1), big.mark = "\u00a0", nsmall = 1)
    fmt_pct <- function(x) if (is.na(x)) "\u2013" else paste0(x, "\u00a0%")

    sections <- lapply(month_keys, function(mk) {
      yr  <- as.integer(substr(mk, 1, 4))
      mo  <- as.integer(substr(mk, 5, 6))
      lbl <- paste(month.name[mo], yr)
      arb_h <- arb_map[[mk]]
      arb_txt <- if (!is.null(arb_h) && arb_h > 0)
        paste("Arbetstimmar grund:", arb_h)
      else
        "Arbetstimmar grund: saknas"

      # [+] Look up the pre-computed monthly grand summary row via period_date
      mk_date <- as.Date(paste0(substr(mk, 1, 4), "-", substr(mk, 5, 6), "-01"))
      gs_row <- if (!is.null(mgs) && nrow(mgs) > 0)
        mgs[!is.na(mgs$period_date) & mgs$period_date == mk_date, , drop = FALSE]
      else
        NULL

      monthly_total_ui <- if (!is.null(gs_row) && nrow(gs_row) == 1) {
        tagList(
          tags$p(tags$strong(paste0("TOTALT F\u00d6R M\u00c5NADEN \u2014 ", lbl))),
          tags$table(
            class = "table table-condensed table-bordered",
            style = "width:auto; font-size:88%; margin-top:4px; margin-bottom:12px;",
            tags$tr(
              tags$th("Summa debiterat (kr)"),
              tags$th("Summa bonus (kr)"),
              tags$th("Summa l\u00f6n totalt (kr)"),
              tags$th("Snitt timmar/konsult"),
              tags$th("Snitt debiteringsgrad")
            ),
            tags$tr(
              tags$td(tags$b(fmt_kr(gs_row$sum_total_debiterad))),
              tags$td(tags$b(fmt_kr(gs_row$sum_bonus_belopp))),
              tags$td(tags$b(fmt_kr(gs_row$sum_lon_total))),
              tags$td(tags$b(fmt_h(gs_row$snitt_total_timmar))),
              tags$td(tags$b(fmt_pct(gs_row$snitt_debiteringsgrad_monad)))
            )
          )
        )
      } else NULL

      tagList(
        tags$hr(),
        h4(lbl),
        tags$p(tags$strong(arb_txt)),
        h5("Detalj (konsult \u2022 kund \u2022 uppdrag)"),
        rhandsontable::rHandsontableOutput(paste0("hot_irep_d_", mk)),
        h5("Sammanfattning per konsult"),
        rhandsontable::rHandsontableOutput(paste0("hot_irep_s_", mk)),
        monthly_total_ui  # [+] summary row after each month's table
      )
    })
    do.call(tagList, sections)
  })
  output$hot_irep_totals  <- rhandsontable::renderRHandsontable({
    req(rv$interval_report)
    rhandsontable::rhandsontable(rv$interval_report$totals,  stretchH = "all", readOnly = TRUE)
  })

  output$hot_irep_group_bonus <- rhandsontable::renderRHandsontable({
    req(rv$interval_report)
    df <- rv$interval_report$group_bonus_summary
    if (nrow(df) == 0) return(NULL)
    hot_with_date_cols(df, "period_date", read_only = TRUE)
  })

  output$hot_irep_sales_bonus <- rhandsontable::renderRHandsontable({
    req(rv$interval_report)
    df <- rv$interval_report$sales_bonus_summary
    if (nrow(df) == 0) return(NULL)
    hot_with_date_cols(df, "period_date", read_only = TRUE)
  })

  output$download_interval_report <- downloadHandler(
    filename = function() {
      sprintf("Intervallrapport_%04d%02d_%04d%02d.xlsx",
              as.integer(input$irep_start_year), as.integer(input$irep_start_month),
              as.integer(input$irep_end_year),   as.integer(input$irep_end_month))
    },
    content = function(file) {
      req(rv$interval_report)
      report <- rv$interval_report
      cf     <- rv$irep_consultant_filter
      if (!is.null(cf) && length(cf) > 0) {
        kons_lbl <- rv$labels$kons
        kons_lbl$consultant_id <- as.character(kons_lbl$consultant_id)
        keep <- kons_lbl[kons_lbl$consultant_id %in% as.character(cf), , drop = FALSE]
        keep_key <- paste(as.character(keep$fornamn), as.character(keep$efternamn))
        filter_by_name <- function(df) {
          if (nrow(df) == 0 || !all(c("fornamn","efternamn") %in% names(df))) return(df)
          df[paste(df$fornamn, df$efternamn) %in% keep_key, , drop = FALSE]
        }
        report$detail  <- filter_by_name(report$detail)
        report$summary <- filter_by_name(report$summary)
        report$totals  <- filter_by_name(report$totals)
      }
      writexl::write_xlsx(interval_report_workbook_list(report, include_bonus = isTRUE(rv$irep_include_bonus)), path = file)
    }
  )

  # ===================== HISTORIK (read-only med namn) =====================
  
  hist_view_with_names <- function(tbl, labels) {
    t <- sanitize_nulls_df(tbl)
    if ("consultant_id" %in% names(t)) t$consultant_name <- id_to_name(labels$kons, "consultant_id", "consultant_name", t$consultant_id)
    if ("customer_id" %in% names(t)) t$customer_name <- id_to_name(labels$kunder, "customer_id", "customer_name", t$customer_id)
    if ("maklare_id" %in% names(t)) t$maklare_name <- id_to_name(labels$makl, "maklare_id", "maklare_name", t$maklare_id)
    if ("uppdrag_id" %in% names(t)) t$uppdrag_label <- id_to_name(labels$upp, "uppdrag_id", "uppdrag_label", t$uppdrag_id)
    front <- intersect(c("consultant_name","customer_name","maklare_name","uppdrag_label"), names(t))
    t[, c(front, setdiff(names(t), front)), drop = FALSE]
  }
  
  # ---- History tab consultant filters ----
  gl_kons_filter <- reactiveVal("")
  tp_kons_filter <- reactiveVal("")
  bo_kons_filter <- reactiveVal("")

  observe({
    req(rv$labels$kons)
    choices <- c("Alla" = "", setNames(rv$labels$kons$consultant_name, rv$labels$kons$consultant_name))
    updateSelectInput(session, "gl_filter_kons", choices = choices)
    updateSelectInput(session, "tp_filter_kons", choices = choices)
    updateSelectInput(session, "bo_filter_kons", choices = choices)
  })

  observeEvent(input$btn_filter_gl,       { gl_kons_filter(input$gl_filter_kons) })
  observeEvent(input$btn_clear_filter_gl, { gl_kons_filter(""); updateSelectInput(session, "gl_filter_kons", selected = "") })
  observeEvent(input$btn_filter_tp,       { tp_kons_filter(input$tp_filter_kons) })
  observeEvent(input$btn_clear_filter_tp, { tp_kons_filter(""); updateSelectInput(session, "tp_filter_kons", selected = "") })
  observeEvent(input$btn_filter_bo,       { bo_kons_filter(input$bo_filter_kons) })
  observeEvent(input$btn_clear_filter_bo, { bo_kons_filter(""); updateSelectInput(session, "bo_filter_kons", selected = "") })

  output$hot_gl_hist <- rhandsontable::renderRHandsontable({
    req(rv$wb, rv$labels)
    df <- hist_view_with_names(rv$wb[["GrundlonHistory"]], rv$labels) |> coerce_dates(c("created_at"))
    if (nzchar(gl_kons_filter())) df <- df[!is.na(df$consultant_name) & df$consultant_name == gl_kons_filter(), ]
    hot_with_date_cols(df, c("created_at"), read_only = TRUE)
  })
  output$hot_tp_hist <- rhandsontable::renderRHandsontable({
    req(rv$wb, rv$labels)
    df <- hist_view_with_names(rv$wb[["TimprisHistory"]], rv$labels) |> coerce_dates(c("created_at"))
    if (nzchar(tp_kons_filter())) df <- df[!is.na(df$consultant_name) & df$consultant_name == tp_kons_filter(), ]
    hot_with_date_cols(df, c("created_at"), read_only = TRUE)
  })
  output$hot_bo_hist <- rhandsontable::renderRHandsontable({
    req(rv$wb, rv$labels)
    df <- hist_view_with_names(rv$wb[["BonusHistory"]], rv$labels) |> coerce_dates(c("created_at"))
    if (nzchar(bo_kons_filter())) df <- df[!is.na(df$consultant_name) & df$consultant_name == bo_kons_filter(), ]
    hot_with_date_cols(df, c("created_at"), read_only = TRUE)
  })
  output$hot_gb_hist <- rhandsontable::renderRHandsontable({ req(rv$wb, rv$labels); hot_with_date_cols(hist_view_with_names(rv$wb[["GroupBonusHistory"]], rv$labels) |> coerce_dates(c("created_at")), c("created_at"), read_only = TRUE) })
  output$hot_sb_hist <- rhandsontable::renderRHandsontable({ req(rv$wb, rv$labels); hot_with_date_cols(hist_view_with_names(rv$wb[["SalesBonusHistory"]], rv$labels) |> coerce_dates(c("created_at")), c("created_at"), read_only = TRUE) })
  
  # ===================== COMMIT från UI =====================
  
  commit_sheet_from_hot <- function(sheet_name, hot_input, date_cols = NULL, postprocess = NULL) {
    if (is.null(hot_input)) {
      showNotification(
        paste0("Tabellen '", sheet_name, "' har inte \u00f6ppnats \u00e4n \u2014 \u00f6ppna fliken och f\u00f6rs\u00f6k igen."),
        type = "warning", duration = 4
      )
      return(invisible(FALSE))
    }
    new <- rhandsontable::hot_to_r(hot_input) |> sanitize_nulls_df()
    if (!is.null(date_cols)) new <- coerce_dates(new, date_cols)
    if (is.function(postprocess)) new <- postprocess(new)
    rv$wb[[sheet_name]] <- new
    invisible(TRUE)
  }
  
  # ===================== VALIDERING AV FORMULÄR =====================
  
  # Returns a character vector of error messages; empty vector = valid.
  validate_add_form <- function(checks) {
    msgs <- character(0)
    for (chk in checks) {
      if (!isTRUE(chk$ok)) msgs <- c(msgs, chk$msg)
    }
    msgs
  }
  
  # Shows all errors in one notification. Returns TRUE if no errors, FALSE otherwise.
  show_validation_errors <- function(errors) {
    if (length(errors) == 0L) return(invisible(TRUE))
    showNotification(
      paste(c("Kan inte spara:", paste0("\u2022 ", errors)), collapse = "\n"),
      type = "error",
      duration = 7
    )
    invisible(FALSE)
  }
  
  # ===================== HISTORIK-LOGIK vid spar =====================
  
  log_history_after_sheet_change <- function(sheet_name) {
    #NEW!!!
    if (identical(sheet_name, "Konsulter")) {
      old <- sanitize_nulls_df(rv$snap[["Konsulter"]]) |> coerce_dates(c("startdatum", "slutdatum"))
      new <- sanitize_nulls_df(rv$wb[["Konsulter"]])   |> coerce_dates(c("startdatum", "slutdatum"))
      
      # En rad per konsult-id (tar första förekomsten om dubbletter finns)
      old1 <- norm_one_per_id(old, "consultant_id")
      new1 <- norm_one_per_id(new, "consultant_id")
      
      # ===================== GRUNDLÖN =====================
      if (all(c("consultant_id", "grundlon") %in% names(old1)) && all(c("consultant_id", "grundlon") %in% names(new1))) {
        m <- full_join(
          old1 |> transmute(consultant_id, grundlon_old = grundlon),
          new1 |> transmute(consultant_id, grundlon_new = grundlon),
          by = "consultant_id"
        )
        
        ch <- changed_num(m$grundlon_old, m$grundlon_new)
        changed_ids <- m$consultant_id[ch]
        
        if (length(changed_ids) > 0) {
          rv$wb <- ensure_grundlon_history_sheet(rv$wb)
          gl <- sanitize_nulls_df(rv$wb[["GrundlonHistory"]]) |>
            coerce_char_cols(c("lon_id", "consultant_id"))
          
          rows <- data.frame(
            lon_id        = sprintf("GL-%03d", (nrow(gl) + 1):(nrow(gl) + length(changed_ids))),
            consultant_id = as.character(changed_ids),
            grundlon      = suppressWarnings(as.numeric(m$grundlon_new[ch])),
            created_at    = Sys.Date(),
            kommentar     = "Auto: \u00e4ndring grundlon i Konsulter-tabell",
            stringsAsFactors = FALSE
          ) |> coerce_dates(c("created_at"))
          
          rv$wb[["GrundlonHistory"]] <- bind_rows(gl, rows)
        }
      }
      
      # ===================== BONUS_GRUND =====================
      if (all(c("consultant_id", "bonus_grund") %in% names(old1)) && all(c("consultant_id", "bonus_grund") %in% names(new1))) {
        m <- full_join(
          old1 |> transmute(consultant_id, bonus_old = bonus_grund),
          new1 |> transmute(consultant_id, bonus_new = bonus_grund),
          by = "consultant_id"
        )
        
        ch <- changed_num(m$bonus_old, m$bonus_new)
        changed_ids <- m$consultant_id[ch]
        
        if (length(changed_ids) > 0) {
          rv$wb <- ensure_bonus_history_sheet(rv$wb)
          bh <- sanitize_nulls_df(rv$wb[["BonusHistory"]]) |>
            coerce_char_cols(c("bonus_id", "consultant_id"))
          
          rows <- data.frame(
            bonus_id      = sprintf("BN-%03d", (nrow(bh) + 1):(nrow(bh) + length(changed_ids))),
            consultant_id = as.character(changed_ids),
            bonus_grund   = suppressWarnings(as.numeric(m$bonus_new[ch])),
            created_at    = Sys.Date(),
            kommentar     = "Auto: \u00e4ndring bonus_grund i Konsulter-tabell",
            stringsAsFactors = FALSE
          ) |> coerce_dates(c("created_at"))
          
          rv$wb[["BonusHistory"]] <- bind_rows(bh, rows)
        }
        
      }
      # ===================== GROUP_BONUS =====================
      if (all(c("consultant_id", "group_bonus") %in% names(old1)) &&
          all(c("consultant_id", "group_bonus") %in% names(new1))) {
        m <- full_join(
          old1 |> transmute(consultant_id, group_bonus_old = group_bonus),
          new1 |> transmute(consultant_id, group_bonus_new = group_bonus),
          by = "consultant_id"
        )
        
        ch <- changed_num(m$group_bonus_old, m$group_bonus_new)
        changed_ids <- m$consultant_id[ch]
        
        if (length(changed_ids) > 0) {
          rv$wb <- ensure_group_bonus_history_sheet(rv$wb)
          gb <- sanitize_nulls_df(rv$wb[["GroupBonusHistory"]]) |>
            coerce_char_cols(c("group_bonus_id", "consultant_id"))
          
          rows <- data.frame(
            group_bonus_id = sprintf(
              "GB-%03d",
              (nrow(gb) + 1):(nrow(gb) + length(changed_ids))
            ),
            consultant_id = as.character(changed_ids),
            group_bonus = suppressWarnings(as.numeric(m$group_bonus_new[ch])),
            created_at = Sys.Date(),
            kommentar = "Auto: \u00e4ndring group_bonus i Konsulter-tabell",
            stringsAsFactors = FALSE
          ) |> coerce_dates(c("created_at"))
          
          rv$wb[["GroupBonusHistory"]] <- bind_rows(gb, rows)
        }
      }
      
      # ===================== SALES_BONUS =====================
      if (all(c("consultant_id", "sales_bonus") %in% names(old1)) &&
          all(c("consultant_id", "sales_bonus") %in% names(new1))) {
        m <- full_join(
          old1 |> transmute(consultant_id, sales_bonus_old = sales_bonus),
          new1 |> transmute(consultant_id, sales_bonus_new = sales_bonus),
          by = "consultant_id"
        )
        
        ch <- changed_num(m$sales_bonus_old, m$sales_bonus_new)
        changed_ids <- m$consultant_id[ch]
        
        if (length(changed_ids) > 0) {
          rv$wb <- ensure_sales_bonus_history_sheet(rv$wb)
          sb <- sanitize_nulls_df(rv$wb[["SalesBonusHistory"]]) |>
            coerce_char_cols(c("sales_bonus_id", "consultant_id", "customer_id"))
          
          rows <- data.frame(
            sales_bonus_id = sprintf(
              "SB-%03d",
              (nrow(sb) + 1):(nrow(sb) + length(changed_ids))
            ),
            consultant_id = as.character(changed_ids),
            customer_id = NA_character_,
            sales_bonus = suppressWarnings(as.numeric(m$sales_bonus_new[ch])),
            created_at = Sys.Date(),
            kommentar = "Auto: \u00e4ndring sales_bonus i Konsulter-tabell",
            stringsAsFactors = FALSE
          ) |> coerce_dates(c("created_at"))
          
          rv$wb[["SalesBonusHistory"]] <- bind_rows(sb, rows)
        }
      }
    }
    #NEW!!!
    if (identical(sheet_name, "Uppgift")) {
      old <- sanitize_nulls_df(rv$snap[["Uppgift"]]) |>
        ensure_uppgift_timpris() |>
        ensure_cols(c("customer_id","created_at")) |>
        coerce_dates(c("startdatum","created_at"))

      new <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
        ensure_uppgift_timpris() |>
        ensure_cols(c("customer_id","created_at")) |>
        coerce_dates(c("startdatum","created_at"))
      
      d <- diff_uppgift_timpris(old, new)
      if (nrow(d) > 0) {
        rv$wb <- ensure_timpris_history_sheet(rv$wb)
        th <- sanitize_nulls_df(rv$wb[["TimprisHistory"]])
        
        add <- lapply(seq_len(nrow(d)), function(i) {
          r <- d[i,]
          data.frame(
            timpris_id = next_id(th, "TP"),
            consultant_id = as.character(r$consultant_id),
            uppdrag_id = as.character(r$uppdrag_id),
            uppgift_id = as.character(r$uppgift_id),
            timpris = suppressWarnings(as.numeric(r$timpris.new)),
            created_at = Sys.Date(),
            kommentar = "Auto: \u00e4ndring timpris i Uppgift-tabell",
            stringsAsFactors = FALSE
          )
        })
        add <- do.call(rbind, add) |> coerce_dates(c("created_at"))
        rv$wb[["TimprisHistory"]] <- bind_rows(th, add)
      }
    }
  }
  
  # ===================== Skriv till disk =====================
  
  write_wb_to_disk <- function(with_backup = FALSE) {
    req(rv$wb)

    lock_path <- paste0(TARGET_XLSX, ".lock")
    if (file.exists(lock_path)) {
      showNotification(
        "Filen sparas redan av en annan session \u2014 f\u00f6rs\u00f6k igen om ett \u00f6gonblick.",
        type = "error", duration = 6
      )
      return(invisible(FALSE))
    }
    writeLines(as.character(Sys.time()), lock_path)
    on.exit(suppressWarnings(file.remove(lock_path)), add = TRUE)

    if (with_backup) {
      bpath <- backup_path_same_dir(TARGET_XLSX)
      ok_copy <- FALSE
      if (file.exists(TARGET_XLSX)) ok_copy <- suppressWarnings(file.copy(TARGET_XLSX, bpath, overwrite = FALSE))
      if (!isTRUE(ok_copy)) suppressWarnings(writexl::write_xlsx(rv$wb, path = bpath))
    }

    result <- tryCatch(
      { writexl::write_xlsx(rv$wb, path = TARGET_XLSX); TRUE },
      error = function(e) {
        showNotification(
          paste0("Fel vid sparande till Excel: ", conditionMessage(e)),
          type = "error", duration = 8
        )
        FALSE
      }
    )
    invisible(result)
  }

  persist_changes <- function(changed_sheets,
                              refresh_labels = FALSE,
                              refresh_selects = character(0),
                              selected_tid_kund = NULL,
                              with_write = TRUE,
                              with_notify = FALSE,
                              notify = NULL) {
    sheets <- unique(as.character(changed_sheets))
    sheets <- sheets[!is.na(sheets) & nzchar(sheets)]

    for (nm in sheets) rv$snap[[nm]] <- rv$wb[[nm]]

    if (isTRUE(refresh_labels)) rv$labels <- make_labels(rv$wb)

    refresh_keys <- unique(refresh_selects)
    refresh_keys <- refresh_keys[!is.na(refresh_keys) & nzchar(refresh_keys)]

    if (length(refresh_keys) > 0) {
      other_keys <- setdiff(refresh_keys, "tid_kund")
      if (length(other_keys) > 0) refresh_selects_by_keys(other_keys)
      if ("tid_kund" %in% refresh_keys) {
        refresh_selects_by_keys("tid_kund", selected_tid_kund = selected_tid_kund)
      }
    }

    if (isTRUE(with_write) && !isTRUE(write_wb_to_disk(FALSE))) return(invisible(FALSE))

    if (isTRUE(with_notify) && !is.null(notify) && nzchar(notify)) {
      showNotification(notify, type = "message", duration = 2)
    }

    invisible(TRUE)
  }

  persist_after_add <- function(changed_sheets,
                                refresh_labels = FALSE,
                                refresh_selects = character(0),
                                selected_tid_kund = NULL,
                                with_notify = FALSE,
                                notify = NULL) {
    persist_changes(
      changed_sheets = changed_sheets,
      refresh_labels = refresh_labels,
      refresh_selects = refresh_selects,
      selected_tid_kund = selected_tid_kund,
      with_write = TRUE,
      with_notify = with_notify,
      notify = notify
    )
  }


    refresh_selects_by_keys <- make_refresh_selects(session, rv)

    register_add_handlers(
      input               = input,
      session             = session,
      rv                  = rv,
      persist_after_add   = persist_after_add,
      validate_add_form   = validate_add_form,
      show_validation_errors = show_validation_errors,
      refresh_selects_by_keys = refresh_selects_by_keys
    )

    register_bonus_report_handlers(
      input             = input,
      output            = output,
      session           = session,
      rv                = rv,
      persist_after_add = persist_after_add
    )

  save_registry <- function() {
    list(
      "Konsulter" = list(
        hot = input$hot_konsulter,
        date_cols = c("startdatum", "slutdatum"),
        postprocess = function(df) {
          for (bc in c("bonus_grund", "group_bonus", "sales_bonus")) {
            if (!bc %in% names(df)) df[[bc]] <- NA_real_
            df[[bc]] <- safe_as_numeric(df[[bc]], bc)
          }
          if ("grundlon" %in% names(df)) df$grundlon <- safe_as_numeric(df$grundlon, "grundlon")
          df
        },
        history = TRUE,
        history_sheet = "Konsulter",
        refresh_labels = TRUE,
        refresh_selects = c("uppg_kons", "tid_kons"),
        notify = "Sparat (Konsulter + historik)."
      ),
      "Maklare" = list(
        hot = input$hot_maklare,
        date_cols = c("created_at"),
        postprocess = function(df) ensure_cols(df, c("maklare_id", "maklare_namn", "kontakt_fornamn", "kontakt_efternamn", "email", "kommentar", "created_at")),
        history = FALSE,
        refresh_labels = TRUE,
        refresh_selects = c("kund_fm_maklare"),
        notify = "Sparat (M\u00e4klare)."
      ),
      "Kunder" = list(
        hot = input$hot_kunder,
        date_cols = c("created_at"),
        postprocess = function(df) ensure_cols(df, c("customer_id", "customer_namn", "kontakt_fornamn", "kontakt_efternamn", "email", "created_at")),
        history = FALSE,
        refresh_labels = TRUE,
        refresh_selects = c("upp_kund", "uppg_kund", "tid_kund", "fm_koppling"),
        notify = "Sparat (Kunder)."
      ),
      "FaktureringInformation" = list(
        hot = input$hot_fm,
        date_cols = NULL,
        postprocess = function(df) ensure_cols(df, c(
          "faktura_mottagare_id", "mottagare_typ", "maklare_id", "customer_id",
          "kontakt_fornamn", "kontakt_efternamn", "email", "avtal_nummer",
          "referens", "verksamhet", "kommentar"
        )),
        history = FALSE,
        refresh_labels = FALSE,
        refresh_selects = character(0),
        notify = "Sparat (Faktura mottagare)."
      ),
      "Uppdrag" = list(
        hot = input$hot_uppdrag,
        date_cols = c("startdatum", "slutdatum"),
        postprocess = function(df) {
          df <- drop_display_cols_uppdrag(df, wb = rv$wb, labels = rv$labels)
          ensure_cols(df, c("uppdrag_id", "customer_id", "uppdrag_name", "beskrivning",
                            "faktura_mottagare_id", "startdatum", "slutdatum"))
        },
        history = FALSE,
        refresh_labels = TRUE,
        refresh_selects = c("tid_upp"),
        notify = "Sparat (Uppdrag)."
      ),
      "Uppgift" = list(
        hot = input$hot_uppgift,
        date_cols = c("startdatum", "created_at"),
        postprocess = function(df) {
          df <- drop_display_cols_uppgift(df, wb = rv$wb, labels = rv$labels)
          ensure_cols(df, c("uppgift_id", "uppgift_name", "consultant_id", "uppdrag_id", "customer_id", "startdatum", "timpris", "created_at"))
        },
        history = TRUE,
        history_sheet = "Uppgift",
        refresh_labels = FALSE,
        refresh_selects = character(0),
        notify = "Sparat (Uppgift)."
      ),
      "Tidrapportering" = list(
        hot = input$hot_tid,
        date_cols = c("startdatum", "slutdatum", "created_at"),
        postprocess = function(df) {
          df <- drop_display_cols_tid(df, wb = rv$wb, labels = rv$labels)
          ensure_cols(df, c("tidrapport_id", "uppgift_id", "customer_id", "timmar", "startdatum", "slutdatum", "consultant_id", "uppdrag_id", "created_at"))
        },
        history = FALSE,
        refresh_labels = FALSE,
        refresh_selects = character(0),
        notify = "Sparat (Tidrapportering)."
      )
    )
  }

  save_sheet <- function(sheet_name, with_write = TRUE, with_notify = TRUE) {
    req(rv$wb)
    reg <- save_registry()[[sheet_name]]
    if (is.null(reg)) return(invisible(FALSE))

    if (!isTRUE(commit_sheet_from_hot(
      sheet_name = sheet_name,
      hot_input = reg$hot,
      date_cols = reg$date_cols,
      postprocess = reg$postprocess
    ))) return(invisible(FALSE))

    if (isTRUE(reg$history) && !is.null(reg$history_sheet)) {
      log_history_after_sheet_change(reg$history_sheet)
    }

    persist_changes(
      changed_sheets = sheet_name,
      refresh_labels = reg$refresh_labels,
      refresh_selects = reg$refresh_selects,
      with_write = with_write,
      with_notify = with_notify,
      notify = reg$notify
    )
  }
  
  # ===================== PER-FLIK SAVE =====================
  
  observeEvent(input$btn_save_konsulter, {
    save_sheet("Konsulter", with_write = TRUE, with_notify = TRUE)
  })
  
  observeEvent(input$btn_save_maklare, {
    save_sheet("Maklare", with_write = TRUE, with_notify = TRUE)
  })
  
  observeEvent(input$btn_save_kunder, {
    save_sheet("Kunder", with_write = TRUE, with_notify = TRUE)
  })
  
  observeEvent(input$btn_save_fm, {
    save_sheet("FaktureringInformation", with_write = TRUE, with_notify = TRUE)
  })
  
  observeEvent(input$btn_save_uppdrag, {
    save_sheet("Uppdrag", with_write = TRUE, with_notify = TRUE)
  })
  
  observeEvent(input$btn_save_uppgift, {
    save_sheet("Uppgift", with_write = TRUE, with_notify = TRUE)
  })
  
  observeEvent(input$btn_save_tid, {
    save_sheet("Tidrapportering", with_write = TRUE, with_notify = TRUE)
  })
  
  # ===================== GLOBAL SAVE ALL =====================
  
  observeEvent(input$btn_save_all, {
    req(rv$wb)
    reg <- save_registry()

    for (sheet_name in names(reg)) {
      try({
        if (!is.null(reg[[sheet_name]]$hot)) {
          save_sheet(sheet_name, with_write = FALSE, with_notify = FALSE)
        }
      }, silent = TRUE)
    }
    
    for (nm in c(
      "Konsulter","Maklare","Kunder","Uppdrag","Uppgift","Tidrapportering",
      "FaktureringInformation","GrundlonHistory","BonusHistory",
      "GroupBonusHistory","SalesBonusHistory","TimprisHistory","ArbetstimmarGrund",
      "BonusRapportering"
    )) rv$snap[[nm]] <- rv$wb[[nm]]

    if (!isTRUE(write_wb_to_disk(with_backup = TRUE))) return()
    
    rv$labels <- make_labels(rv$wb)
    refresh_selects_by_keys(c("upp_kund", "uppg_kund", "uppg_kons", "tid_kons", "tid_kund", "tid_upp", "fm_koppling", "kund_fm_maklare"))
    
    showNotification("Sparat ALLT + backup (inkl historik).", type="message", duration=3)
  })

  # ===================== AVSLUTA (EXIT) =====================

  observeEvent(input$btn_avsluta, {
    showModal(modalDialog(
      title = "Avsluta applikationen",
      "Är du säker på att du vill spara alla ändringar och avsluta applikationen?",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Avbryt"),
        actionButton("confirm_avsluta", "Spara och avsluta", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_avsluta, {
    removeModal()
    req(rv$wb)

    reg <- save_registry()
    for (sheet_name in names(reg)) {
      try({
        if (!is.null(reg[[sheet_name]]$hot)) {
          save_sheet(sheet_name, with_write = FALSE, with_notify = FALSE)
        }
      }, silent = TRUE)
    }

    for (nm in c(
      "Konsulter","Maklare","Kunder","Uppdrag","Uppgift","Tidrapportering",
      "FaktureringInformation","GrundlonHistory","BonusHistory",
      "GroupBonusHistory","SalesBonusHistory","TimprisHistory","ArbetstimmarGrund",
      "BonusRapportering"
    )) rv$snap[[nm]] <- rv$wb[[nm]]

    if (!isTRUE(write_wb_to_disk(with_backup = TRUE))) {
      showNotification(
        "Sparandet misslyckades \u2014 applikationen avslutas INTE. Kontrollera att Excel-filen inte \u00e4r \u00f6ppen i ett annat program.",
        type = "error", duration = 10
      )
      return()
    }

    showNotification("Alla \u00e4ndringar sparade och backup skapad. Appen avslutas...", type = "message", duration = 3)
    session$onFlushed(function() {
      Sys.sleep(1)
      stopApp()
    }, once = TRUE)
  })
}
