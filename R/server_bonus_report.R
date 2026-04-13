# ===================== BONUSRAPPORTERING SERVER =====================
# Handles the shared Gruppbonus / Säljbonus reporting form.
# Sourced globally in app.R (before server.R).
# register_bonus_report_handlers() is called from inside server().
#
# Depends on (already in scope when sourced):
#   sanitize_nulls_df, coerce_dates, ensure_cols, ensure_uppgift_timpris,
#   id_to_name, name_to_id, month_bounds, str_squish,
#   resolve_timpris_for_month, resolve_group_bonus_for_month,
#   resolve_sales_bonus_for_month, ensure_bonus_rapport_sheet,
#   hot_with_date_cols
#
# Why a separate file: keeps the bonus registration logic isolated so server.R
# stays readable and the feature can be extended without touching existing code.

# ------------------------------------------------------------------
# ID generator: BR-NNN, globally sequential across all entries.
# ------------------------------------------------------------------
generate_bonus_rapport_id <- function(existing_br, year, month) {
  existing <- as.character(existing_br[["bonus_rapport_id"]])
  nums     <- suppressWarnings(as.integer(sub("^BR-(\\d+)$", "\\1", existing)))
  nums     <- nums[!is.na(nums)]
  n        <- if (length(nums) == 0L) 1L else max(nums) + 1L
  sprintf("BR-%03d", n)
}

# ------------------------------------------------------------------
# Helper: consultant names filtered by bonus type availability.
# Returns a plain character vector of names (same pattern as the rest
# of the app, so name_to_id() can resolve IDs later).
# ------------------------------------------------------------------
bon_filtered_consultant_names <- function(wb, bon_type) {
  kons <- sanitize_nulls_df(wb[["Konsulter"]]) |>
    mutate(
      consultant_id   = as.character(consultant_id),
      consultant_name = str_squish(paste(fornamn, efternamn))
    )

  if (identical(bon_type, "Gruppbonus")) {
    hist_ids <- as.character(
      sanitize_nulls_df(wb[["GroupBonusHistory"]])$consultant_id
    )
    mask <- (!is.na(suppressWarnings(as.numeric(kons$group_bonus))) &
               suppressWarnings(as.numeric(kons$group_bonus)) > 0) |
            (kons$consultant_id %in% hist_ids)
  } else {
    hist_ids <- as.character(
      sanitize_nulls_df(wb[["SalesBonusHistory"]])$consultant_id
    )
    mask <- (!is.na(suppressWarnings(as.numeric(kons$sales_bonus))) &
               suppressWarnings(as.numeric(kons$sales_bonus)) > 0) |
            (kons$consultant_id %in% hist_ids)
  }

  unique(kons$consultant_name[mask])
}

# ------------------------------------------------------------------
# Main registration function — called from server().
# ------------------------------------------------------------------
register_bonus_report_handlers <- function(input, output, session, rv, persist_after_add) {

  # ---- 1. Rapporterande: re-filter when bonus type changes ----
  # Why ignoreInit = TRUE: load_from_disk() initialises the dropdown
  # directly; we only need to re-run when the user switches the radio.
  observeEvent(input$bon_type, {
    req(rv$wb)
    ch <- bon_filtered_consultant_names(rv$wb, input$bon_type)
    updateSelectInput(session, "bon_rapporterande", choices = ch,
                      selected = if (length(ch) > 0L) ch[1L] else NULL)
  }, ignoreInit = TRUE)

  # ---- 2. Uppdrag: filter by selected customer ----
  observeEvent(input$bon_kund, {
    req(rv$wb, rv$labels, input$bon_kund)
    cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$bon_kund)

    upp <- sanitize_nulls_df(rv$wb[["Uppdrag"]]) |>
      mutate(
        customer_id   = as.character(customer_id),
        uppdrag_label = ifelse(is.na(uppdrag_name) | uppdrag_name == "",
                               uppdrag_id, uppdrag_name)
      ) |>
      filter(customer_id == as.character(cust_id))

    ch <- unique(upp$uppdrag_label)
    if (length(ch) == 0L) ch <- character(0L)
    updateSelectInput(session, "bon_uppdrag", choices = ch,
                      selected = if (length(ch) > 0L) ch[1L] else NULL)
  }, ignoreInit = TRUE)

  # ---- 3. Uppgift: filter by customer + uppdrag ----
  observeEvent(list(input$bon_kund, input$bon_uppdrag), {
    req(rv$wb, rv$labels, input$bon_kund, input$bon_uppdrag)

    cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$bon_kund)
    upp_id  <- name_to_id(rv$labels$upp,    "uppdrag_id",  "uppdrag_label", input$bon_uppdrag)

    tasks <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
      mutate(
        customer_id  = as.character(customer_id),
        uppdrag_id   = as.character(uppdrag_id),
        uppgift_id   = as.character(uppgift_id),
        uppgift_name = ifelse(is.na(uppgift_name) | trimws(uppgift_name) == "",
                              uppgift_id, uppgift_name),
        task_label   = paste0(uppgift_name, " (", uppgift_id, ")")
      ) |>
      filter(customer_id == as.character(cust_id),
             uppdrag_id  == as.character(upp_id))

    ch <- unique(tasks$task_label)
    if (length(ch) == 0L) ch <- character(0L)
    updateSelectInput(session, "bon_uppgift", choices = ch,
                      selected = if (length(ch) > 0L) ch[1L] else NULL)
  }, ignoreInit = TRUE)

  # ---- 4. Bas-konsult: filter by consultants with tid in this uppgift+month ----
  observeEvent(list(input$bon_uppgift, input$bon_year, input$bon_month), {
    req(rv$wb, rv$labels, input$bon_uppgift, input$bon_year, input$bon_month)

    y  <- as.integer(input$bon_year)
    m  <- as.integer(input$bon_month)
    mb <- month_bounds(y, m)

    # Resolve uppgift_id from task_label
    tasks <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
      mutate(
        uppgift_id   = as.character(uppgift_id),
        uppgift_name = ifelse(is.na(uppgift_name) | trimws(uppgift_name) == "",
                              uppgift_id, uppgift_name),
        task_label   = paste0(uppgift_name, " (", uppgift_id, ")")
      )
    task_id <- tasks$uppgift_id[match(input$bon_uppgift, tasks$task_label)]

    if (is.na(task_id) || !nzchar(task_id)) {
      updateSelectInput(session, "bon_bas_kons", choices = character(0L))
      return()
    }

    # Consultants with reported hours in this uppgift for this month
    tid <- sanitize_nulls_df(rv$wb[["Tidrapportering"]]) |>
      coerce_dates(c("startdatum", "slutdatum")) |>
      mutate(
        uppgift_id    = as.character(uppgift_id),
        consultant_id = as.character(consultant_id)
      ) |>
      filter(
        uppgift_id == as.character(task_id),
        !is.na(startdatum) & !is.na(slutdatum),
        startdatum <= as.Date(mb$end),
        slutdatum  >= as.Date(mb$start)
      )

    if (nrow(tid) > 0L) {
      cons_ids <- unique(tid$consultant_id)
    } else {
      # Fallback: consultant assigned to the uppgift
      task_row <- tasks[tasks$uppgift_id == as.character(task_id), , drop = FALSE]
      cons_ids <- if (nrow(task_row) > 0L && "consultant_id" %in% names(task_row))
        as.character(task_row$consultant_id[1L]) else character(0L)
    }

    cons_ids <- cons_ids[!is.na(cons_ids) & nzchar(cons_ids)]
    if (length(cons_ids) == 0L) {
      updateSelectInput(session, "bon_bas_kons", choices = character(0L))
      return()
    }

    cons_names <- id_to_name(rv$labels$kons, "consultant_id", "consultant_name", cons_ids)
    updateSelectInput(session, "bon_bas_kons",
                      choices  = unique(cons_names),
                      selected = cons_names[1L])
  }, ignoreInit = TRUE)

  # ---- 5. Reactive calculation (used for display and for saving) ----
  bon_calc <- reactive({
    req(rv$wb, rv$labels,
        input$bon_type, input$bon_year, input$bon_month,
        input$bon_rapporterande, input$bon_bas_kons,
        input$bon_uppgift, input$bon_kund)

    y  <- as.integer(input$bon_year)
    m  <- as.integer(input$bon_month)
    mb <- month_bounds(y, m)
    fd <- as.Date(mb$start)

    # Resolve IDs from display names
    rap_id  <- name_to_id(rv$labels$kons,   "consultant_id", "consultant_name", input$bon_rapporterande)
    bas_id  <- name_to_id(rv$labels$kons,   "consultant_id", "consultant_name", input$bon_bas_kons)
    cust_id <- name_to_id(rv$labels$kunder, "customer_id",   "customer_name",   input$bon_kund)

    # Resolve uppgift_id and its uppdrag_id from the task_label
    tasks <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
      mutate(
        uppgift_id   = as.character(uppgift_id),
        uppdrag_id   = as.character(uppdrag_id),
        uppgift_name = ifelse(is.na(uppgift_name) | trimws(uppgift_name) == "",
                              uppgift_id, uppgift_name),
        task_label   = paste0(uppgift_name, " (", uppgift_id, ")")
      )
    task_row <- tasks[match(input$bon_uppgift, tasks$task_label), , drop = FALSE]
    req(nrow(task_row) > 0L)
    task_id <- task_row$uppgift_id[1L]
    upp_id  <- task_row$uppdrag_id[1L]

    # Reported hours for bas-konsult in this uppgift + month
    tid <- sanitize_nulls_df(rv$wb[["Tidrapportering"]]) |>
      coerce_dates(c("startdatum", "slutdatum")) |>
      mutate(
        uppgift_id    = as.character(uppgift_id),
        consultant_id = as.character(consultant_id),
        timmar        = suppressWarnings(as.numeric(timmar))
      ) |>
      filter(
        consultant_id == as.character(bas_id),
        uppgift_id    == as.character(task_id),
        !is.na(startdatum) & !is.na(slutdatum),
        startdatum <= as.Date(mb$end),
        slutdatum  >= fd
      )

    reported_hours <- sum(tid$timmar, na.rm = TRUE)

    # Timpris for this month (uses existing resolver)
    th_hist <- if (!is.null(rv$wb[["TimprisHistory"]]))
      sanitize_nulls_df(rv$wb[["TimprisHistory"]]) |> coerce_dates(c("created_at"))
    else data.frame()

    timpris <- resolve_timpris_for_month(
      timpris_history  = th_hist,
      uppgift_df       = sanitize_nulls_df(rv$wb[["Uppgift"]]) |> ensure_uppgift_timpris(),
      consultant_id    = as.character(bas_id),
      uppdrag_id       = as.character(upp_id),
      uppgift_id       = as.character(task_id),
      target_first_day = fd
    )

    invoiced_amount <- reported_hours * timpris

    # Bonus % — resolver differs by type
    bonus_pct <- if (identical(input$bon_type, "Gruppbonus")) {
      gb_hist <- if (!is.null(rv$wb[["GroupBonusHistory"]]))
        sanitize_nulls_df(rv$wb[["GroupBonusHistory"]]) |> coerce_dates(c("created_at"))
      else data.frame()
      resolve_group_bonus_for_month(gb_hist, rv$wb[["Konsulter"]], rap_id, fd)
    } else {
      # Säljbonus: keyed on rapporterande consultant + customer
      sb_hist <- if (!is.null(rv$wb[["SalesBonusHistory"]]))
        sanitize_nulls_df(rv$wb[["SalesBonusHistory"]]) |> coerce_dates(c("created_at"))
      else data.frame()
      resolve_sales_bonus_for_month(sb_hist, rv$wb[["Konsulter"]], rap_id, cust_id, fd)
    }

    list(
      rap_id          = rap_id,
      bas_id          = bas_id,
      cust_id         = cust_id,
      task_id         = task_id,
      upp_id          = upp_id,
      reported_hours  = reported_hours,
      timpris         = timpris,
      invoiced_amount = invoiced_amount,
      bonus_pct       = bonus_pct,
      bonus_amount    = round(invoiced_amount * bonus_pct / 100, 2)
    )
  })

  # ---- 6. Show the auto-calculated values below the form ----
  output$bon_calc_display <- renderUI({
    req(rv$wb)
    calc <- tryCatch(bon_calc(), error = function(e) NULL)
    if (is.null(calc)) {
      return(helpText("Fyll i alla f\u00e4lt ovan f\u00f6r att se ber\u00e4kning."))
    }
    tags$table(
      class = "table table-condensed table-bordered",
      style = "width:auto; font-size:90%; margin-top:8px;",
      tags$tr(tags$td("Rapporterade timmar:"),
              tags$td(tags$b(format(calc$reported_hours, big.mark = "\u00a0")))),
      tags$tr(tags$td("Timpris (kr):"),
              tags$td(tags$b(format(calc$timpris, big.mark = "\u00a0", nsmall = 0)))),
      tags$tr(tags$td("Fakturerat belopp (kr):"),
              tags$td(tags$b(format(round(calc$invoiced_amount), big.mark = "\u00a0")))),
      tags$tr(tags$td("Aktiv bonus %:"),
              tags$td(tags$b(calc$bonus_pct, "%"))),
      tags$tr(
        tags$td(tags$strong("Bonusbelopp (kr):")),
        tags$td(tags$b(tags$span(style = "color:#2a7a2a; font-size:105%;",
                                  format(calc$bonus_amount, big.mark = "\u00a0", nsmall = 2))))
      )
    )
  })

  # ---- 7. Register (save) button ----
  observeEvent(input$btn_add_bonus_rapport, {
    req(rv$wb, rv$labels)

    calc <- tryCatch(bon_calc(), error = function(e) NULL)
    if (is.null(calc)) {
      showNotification(
        "Ber\u00e4kning misslyckades \u2014 kontrollera att alla f\u00e4lt \u00e4r ifyllda.",
        type = "error", duration = 5
      )
      return()
    }

    y  <- as.integer(input$bon_year)
    m  <- as.integer(input$bon_month)
    mb <- month_bounds(y, m)

    rv$wb <- ensure_bonus_rapport_sheet(rv$wb)
    br    <- sanitize_nulls_df(rv$wb[["BonusRapportering"]])

    new_row <- data.frame(
      bonus_rapport_id  = generate_bonus_rapport_id(br, y, m),
      bonus_type        = ifelse(identical(input$bon_type, "Gruppbonus"),
                                 "group_bonus", "sales_bonus"),
      start_date        = as.Date(mb$start),
      end_date          = as.Date(mb$end),
      rapporterande_id  = as.character(calc$rap_id),
      bas_consultant_id = as.character(calc$bas_id),
      customer_id       = as.character(calc$cust_id),
      uppdrag_id        = as.character(calc$upp_id),
      uppgift_id        = as.character(calc$task_id),
      bonus_procent     = calc$bonus_pct,
      reported_hours    = calc$reported_hours,
      timpris_used      = calc$timpris,
      invoiced_amount   = round(calc$invoiced_amount, 2),
      bonus_amount      = calc$bonus_amount,
      created_at        = Sys.Date(),
      stringsAsFactors  = FALSE
    ) |> coerce_dates(c("start_date", "end_date", "created_at"))

    rv$wb[["BonusRapportering"]] <- bind_rows(br, new_row)

    if (!isTRUE(persist_after_add(changed_sheets = "BonusRapportering"))) return()

    showNotification(
      paste0("Bonusrad ", new_row$bonus_rapport_id, " registrerad!"),
      type = "message", duration = 4
    )
  })

  # ---- Bonusrapportering consultant filter ----
  bon_kons_filter <- reactiveVal("")

  observe({
    req(rv$labels$kons)
    choices <- c("Alla" = "", setNames(rv$labels$kons$consultant_name, rv$labels$kons$consultant_name))
    updateSelectInput(session, "bon_filter_kons", choices = choices)
  })

  observeEvent(input$btn_filter_bon,       { bon_kons_filter(input$bon_filter_kons) })
  observeEvent(input$btn_clear_filter_bon, { bon_kons_filter(""); updateSelectInput(session, "bon_filter_kons", selected = "") })

  # ---- 8. Read-only table showing all registered bonus rows ----
  output$hot_bonus_rapport <- rhandsontable::renderRHandsontable({
    req(rv$wb, rv$labels)

    rv$wb <- ensure_bonus_rapport_sheet(rv$wb)
    br <- sanitize_nulls_df(rv$wb[["BonusRapportering"]]) |>
      coerce_dates(c("start_date", "end_date", "created_at"))

    if (nrow(br) == 0L) {
      # Return an empty placeholder so the table renders cleanly
      br_disp <- data.frame(
        bonus_rapport_id       = character(0),
        bonus_type             = character(0),
        start_date             = as.Date(character(0)),
        end_date               = as.Date(character(0)),
        rapporterande_name     = character(0),
        bas_consultant_name    = character(0),
        customer_name          = character(0),
        uppdrag_label          = character(0),
        uppgift_label          = character(0),
        bonus_procent          = numeric(0),
        reported_hours         = numeric(0),
        invoiced_amount        = numeric(0),
        bonus_amount           = numeric(0),
        created_at             = as.Date(character(0)),
        stringsAsFactors       = FALSE
      )
      return(hot_with_date_cols(br_disp,
                                c("start_date", "end_date", "created_at"),
                                read_only = TRUE))
    }

    # Resolve display names (IDs stay hidden — stored in wb, not shown)
    kons   <- sanitize_nulls_df(rv$wb[["Konsulter"]]) |>
      mutate(consultant_id   = as.character(consultant_id),
             consultant_name = str_squish(paste(fornamn, efternamn)))
    kunder <- sanitize_nulls_df(rv$wb[["Kunder"]]) |>
      mutate(customer_id   = as.character(customer_id),
             customer_name = ifelse(is.na(customer_namn) | customer_namn == "",
                                    customer_id, customer_namn))
    upp    <- sanitize_nulls_df(rv$wb[["Uppdrag"]]) |>
      mutate(uppdrag_id    = as.character(uppdrag_id),
             uppdrag_label = ifelse(is.na(uppdrag_name) | uppdrag_name == "",
                                    uppdrag_id, uppdrag_name))
    tsk    <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
      mutate(uppgift_id    = as.character(uppgift_id),
             uppgift_label = ifelse(is.na(uppgift_name) | trimws(uppgift_name) == "",
                                    uppgift_id, uppgift_name))

    br_disp <- br |>
      mutate(
        rapporterande_name  = id_to_name(kons,   "consultant_id", "consultant_name", rapporterande_id),
        bas_consultant_name = id_to_name(kons,   "consultant_id", "consultant_name", bas_consultant_id),
        customer_name       = id_to_name(kunder, "customer_id",   "customer_name",   customer_id),
        uppdrag_label       = id_to_name(upp,    "uppdrag_id",    "uppdrag_label",   uppdrag_id),
        uppgift_label       = id_to_name(tsk,    "uppgift_id",    "uppgift_label",   uppgift_id)
      ) |>
      select(bonus_rapport_id, bonus_type, start_date, end_date,
             rapporterande_name, bas_consultant_name, customer_name,
             uppdrag_label, uppgift_label,
             bonus_procent, reported_hours, invoiced_amount, bonus_amount,
             created_at)

    br_disp <- br_disp[order(-as.integer(sub(".*-(\\d+)$", "\\1", br_disp$bonus_rapport_id)), na.last = TRUE), ]

    if (nzchar(bon_kons_filter()))
      br_disp <- br_disp[!is.na(br_disp$rapporterande_name) & br_disp$rapporterande_name == bon_kons_filter(), ]

    hot_with_date_cols(br_disp,
                       c("start_date", "end_date", "created_at"),
                       read_only = TRUE)
  })
}
