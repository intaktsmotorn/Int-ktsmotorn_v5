# Add-form handlers: register_add_handlers(input, session, rv, ...).
# Sourced globally (app.r). Called from server() after all server-local helpers are defined.
# All dependencies are passed explicitly as function arguments.

register_add_handlers <- function(input, session, rv,
                                   persist_after_add,
                                   validate_add_form,
                                   show_validation_errors,
                                   refresh_selects_by_keys) {

  # ===================== ADD: KONSULT (+ historik) =====================
  
  observeEvent(input$btn_add_konsult, {
    req(rv$wb, input$kon_fornamn, input$kon_efternamn, input$kon_type)
    
    errs <- validate_add_form(list(
      list(ok = is.null(input$kon_grundlon)   || is.na(input$kon_grundlon)   || as.numeric(input$kon_grundlon)   >= 0,
           msg = "Grundl\u00f6n m\u00e5ste vara 0 eller positivt."),
      list(ok = is.null(input$kon_bonus_grund) || is.na(input$kon_bonus_grund) || as.numeric(input$kon_bonus_grund) >= 0,
           msg = "Bonusgrund m\u00e5ste vara 0 eller positivt."),
      list(ok = is.null(input$kon_group_bonus) || is.na(input$kon_group_bonus) || as.numeric(input$kon_group_bonus) >= 0,
           msg = "Gruppbonus m\u00e5ste vara 0 eller positivt."),
      list(ok = is.null(input$kon_sales_bonus) || is.na(input$kon_sales_bonus) || as.numeric(input$kon_sales_bonus) >= 0,
           msg = "S\u00e4ljbonus m\u00e5ste vara 0 eller positivt.")
    ))
    if (!isTRUE(show_validation_errors(errs))) return()
    
    kons <- sanitize_nulls_df(rv$wb[["Konsulter"]]) |>
      coerce_dates(c("startdatum","slutdatum")) |>
      ensure_cols(c("bonus_grund","group_bonus","sales_bonus"), default = NA_real_) |>
      coerce_char_cols(c("consultant_id"))
    
    new_id <- next_seq_id(kons$consultant_id, "CONS", width = 3)
    
    new_row <- data.frame(
      consultant_id = new_id,
      fornamn = input$kon_fornamn,
      efternamn = input$kon_efternamn,
      type = input$kon_type,
      grundlon = if (!is.null(input$kon_grundlon) && !is.na(input$kon_grundlon)) as.numeric(input$kon_grundlon) else NA_real_,
      bonus_grund = if (!is.null(input$kon_bonus_grund) && !is.na(input$kon_bonus_grund)) as.numeric(input$kon_bonus_grund) else NA_real_,
      group_bonus = if (!is.null(input$kon_group_bonus) && !is.na(input$kon_group_bonus)) as.numeric(input$kon_group_bonus) else NA_real_,
      sales_bonus = if (!is.null(input$kon_sales_bonus) && !is.na(input$kon_sales_bonus)) as.numeric(input$kon_sales_bonus) else NA_real_,
      startdatum = safe_as_date(input$kon_start),
      slutdatum = NA,
      stringsAsFactors = FALSE
    ) |> coerce_dates(c("startdatum","slutdatum"))
    
    rv$wb[["Konsulter"]] <- bind_rows(kons, new_row)
    
    # historik: grundlön
    if (!is.na(new_row$grundlon[1])) {
      rv$wb <- ensure_grundlon_history_sheet(rv$wb)
      gl <- sanitize_nulls_df(rv$wb[["GrundlonHistory"]]) |>
        coerce_char_cols(c("lon_id", "consultant_id"))
      gl_row <- data.frame(
        lon_id = next_id(gl, "GL"),
        consultant_id = new_id,
        grundlon = new_row$grundlon[1],
        created_at = Sys.Date(),
        kommentar = "Auto vid ny konsult",
        stringsAsFactors = FALSE
      ) |> coerce_dates(c("created_at"))
      rv$wb[["GrundlonHistory"]] <- bind_rows(gl, gl_row)
    }
    
    # historik: bonus
    if (!is.na(new_row$bonus_grund[1])) {
      rv$wb <- ensure_bonus_history_sheet(rv$wb)
      bh <- sanitize_nulls_df(rv$wb[["BonusHistory"]]) |>
        coerce_char_cols(c("bonus_id", "consultant_id"))
      bh_row <- data.frame(
        bonus_id = next_id(bh, "BN"),
        consultant_id = new_id,
        bonus_grund = new_row$bonus_grund[1],
        created_at = Sys.Date(),
        kommentar = "Auto vid ny konsult",
        stringsAsFactors = FALSE
      ) |> coerce_dates(c("created_at"))
      rv$wb[["BonusHistory"]] <- bind_rows(bh, bh_row)
    }
    
    # historik: group bonus
    if (!is.na(new_row$group_bonus[1])) {
      rv$wb <- ensure_group_bonus_history_sheet(rv$wb)
      gb <- sanitize_nulls_df(rv$wb[["GroupBonusHistory"]]) |>
        coerce_char_cols(c("group_bonus_id", "consultant_id"))
      gb_row <- data.frame(
        group_bonus_id = next_id(gb, "GB"),
        consultant_id = new_id,
        group_bonus = new_row$group_bonus[1],
        created_at = Sys.Date(),
        kommentar = "Auto vid ny konsult",
        stringsAsFactors = FALSE
      ) |> coerce_dates(c("created_at"))
      rv$wb[["GroupBonusHistory"]] <- bind_rows(gb, gb_row)
    }
    
    # historik: sales bonus
    if (!is.na(new_row$sales_bonus[1])) {
      rv$wb <- ensure_sales_bonus_history_sheet(rv$wb)
      sb <- sanitize_nulls_df(rv$wb[["SalesBonusHistory"]]) |>
        coerce_char_cols(c("sales_bonus_id", "consultant_id", "customer_id"))
      sb_row <- data.frame(
        sales_bonus_id = next_id(sb, "SB"),
        consultant_id = new_id,
        customer_id = NA_character_,
        sales_bonus = new_row$sales_bonus[1],
        created_at = Sys.Date(),
        kommentar = "Auto vid ny konsult",
        stringsAsFactors = FALSE
      ) |> coerce_dates(c("created_at"))
      rv$wb[["SalesBonusHistory"]] <- bind_rows(sb, sb_row)
    }

    changed_sheets <- c("Konsulter")
    if (!is.na(new_row$grundlon[1])) changed_sheets <- c(changed_sheets, "GrundlonHistory")
    if (!is.na(new_row$bonus_grund[1])) changed_sheets <- c(changed_sheets, "BonusHistory")
    if (!is.na(new_row$group_bonus[1])) changed_sheets <- c(changed_sheets, "GroupBonusHistory")
    if (!is.na(new_row$sales_bonus[1])) changed_sheets <- c(changed_sheets, "SalesBonusHistory")

    if (!isTRUE(persist_after_add(
      changed_sheets = changed_sheets,
      refresh_labels = TRUE,
      refresh_selects = c("uppg_kons", "tid_kons")
    ))) return()
    
    reset_form(session,
               ids = c("kon_fornamn","kon_efternamn","kon_type","kon_grundlon","kon_bonus_grund","kon_group_bonus","kon_sales_bonus","kon_start"),
               defaults = list(kon_type = "Anst\u00e4lld")
    )
  })
  
  # ===================== ADD: MÄKLARE + AUTO FM (som tidigare) =====================
  #NEW!!!
  observeEvent(input$btn_add_maklare, {
    req(rv$wb, input$makl_namn)
    
    errs <- validate_add_form(list(
      list(ok = is.null(input$makl_email) || !nzchar(input$makl_email) || grepl("@", input$makl_email, fixed = TRUE),
           msg = "E-postadressen verkar ogiltig (saknar @).")
    ))
    if (!isTRUE(show_validation_errors(errs))) return()
    
    mk <- ensure_cols(
      rv$wb[["Maklare"]],
      c("maklare_id", "maklare_namn", "kontakt_fornamn", "kontakt_efternamn", "email", "kommentar", "created_at")
    )
    
    new_id <- make_broker_id(input$makl_namn, existing_ids = mk$maklare_id)
    
    new_row <- data.frame(
      maklare_id = new_id,
      maklare_namn = input$makl_namn,
      kontakt_fornamn = input$makl_kfor,
      kontakt_efternamn = input$makl_kefter,
      email = input$makl_email,
      kommentar = input$makl_kommentar,
      created_at = Sys.Date(),
      stringsAsFactors = FALSE
    ) |> coerce_dates(c("created_at"))
    
    rv$wb[["Maklare"]] <- bind_rows(mk, new_row)
    
    # OBS: Ingen auto-rad i FaktureringInformation längre
    
    if (!isTRUE(persist_after_add(
      changed_sheets = c("Maklare"),
      refresh_labels = TRUE,
      refresh_selects = c("kund_fm_maklare")
    ))) return()

    # Uppdatera dropdowns som beror på mäklare
    if (!is.null(input$fm_typ) && identical(input$fm_typ, "M\u00e4klare")) {
      updateSelectInput(session, "fm_koppling", choices = rv$labels$makl$maklare_name)
    }
    
    reset_form(session, ids = c("makl_namn", "makl_kfor", "makl_kefter", "makl_email", "makl_kommentar"))
    
    showNotification("Ny m\u00e4klare tillagd!", type = "message", duration = 3)
  })
  #NEW!!! 
  # ===================== ADD: KUND + AUTO FM (NY LOGIK) =====================
  
  observeEvent(input$btn_add_kund, {
    req(rv$wb, input$kund_namn, rv$labels, input$kund_fm_typ)
    
    errs <- validate_add_form(list(
      list(ok = is.null(input$kund_email) || !nzchar(input$kund_email) || grepl("@", input$kund_email, fixed = TRUE),
           msg = "E-postadressen verkar ogiltig (saknar @)."),
      list(ok = !identical(input$kund_fm_typ, "M\u00e4klare") ||
                  (!is.null(input$kund_fm_maklare) && nzchar(trimws(input$kund_fm_maklare))),
           msg = "V\u00e4lj en m\u00e4klare n\u00e4r FM-typ \u00e4r 'M\u00e4klare'.")
    ))
    if (!isTRUE(show_validation_errors(errs))) return()
    
    kunder <- sanitize_nulls_df(rv$wb[["Kunder"]]) |>
      ensure_cols(c("customer_id"), default = NA_character_)
    
    existing_ids <- kunder$customer_id
    existing_ids <- toupper(trimws(as.character(existing_ids)))
    existing_ids <- existing_ids[!is.na(existing_ids) & nzchar(existing_ids)]
    
    new_id <- make_customer_id(input$kund_namn, existing_ids = existing_ids)
    
    new_row <- data.frame(
      customer_id = new_id,
      customer_namn = input$kund_namn,
      kontakt_fornamn = input$kund_kfor,
      kontakt_efternamn = input$kund_kefter,
      email = input$kund_email,
      created_at = Sys.Date(),
      stringsAsFactors = FALSE
    ) |> coerce_dates(c("created_at"))
    
    rv$wb[["Kunder"]] <- bind_rows(kunder, new_row)
    
    # skapa FM-rad beroende på val
    rv$wb <- ensure_fm_sheet(rv$wb)
    fm <- sanitize_nulls_df(rv$wb[["FaktureringInformation"]])
    
    if (identical(input$kund_fm_typ, "Kund")) {
      fm_row <- data.frame(
        faktura_mottagare_id = next_id(fm, "FM"),
        mottagare_typ = "Kund",
        maklare_id = NA_character_,
        customer_id = new_id,
        kontakt_fornamn = NA_character_,
        kontakt_efternamn = NA_character_,
        email = NA_character_,
        avtal_nummer = NA_character_,
        referens = NA_character_,
        verksamhet = NA_character_,
        kommentar = "Auto fr\u00e5n Kunder (FM-typ Kund)",
        stringsAsFactors = FALSE
      )
    } else {
      # Mäklare vald -> kräver val av mäklare
      req(input$kund_fm_maklare)
      makl_id <- name_to_id(rv$labels$makl, "maklare_id", "maklare_name", input$kund_fm_maklare)
      
      fm_row <- data.frame(
        faktura_mottagare_id = next_id(fm, "FM"),
        mottagare_typ = "M\u00e4klare",
        maklare_id = as.character(makl_id),
        customer_id = new_id,
        kontakt_fornamn = NA_character_,
        kontakt_efternamn = NA_character_,
        email = NA_character_,
        avtal_nummer = NA_character_,
        referens = NA_character_,
        verksamhet = NA_character_,
        kommentar = "Auto fr\u00e5n Kunder (FM-typ M\u00e4klare)",
        stringsAsFactors = FALSE
      )
    }
    
    rv$wb[["FaktureringInformation"]] <- bind_rows(fm, fm_row)

    last_kund <- new_row$customer_namn[1]
    if (is.na(last_kund) || trimws(last_kund) == "") last_kund <- new_id

    if (!isTRUE(persist_after_add(
      changed_sheets = c("Kunder", "FaktureringInformation"),
      refresh_labels = TRUE,
      refresh_selects = c("upp_kund", "uppg_kund", "tid_kund", "fm_koppling", "kund_fm_maklare"),
      selected_tid_kund = last_kund
    ))) return()
    
    reset_form(session,
               ids = c("kund_namn","kund_kfor","kund_kefter","kund_email","kund_fm_typ","kund_fm_maklare"),
               defaults = list(kund_fm_typ = "Kund")
    )
    
    showNotification("Ny kund + fakturamottagare skapad!", type = "message", duration = 3)
  })
  
  # ===================== ADD: FM (MANUELL) =====================
  
  observeEvent(input$btn_add_fm, {
    req(rv$wb, input$fm_typ, rv$labels)
    
    errs <- validate_add_form(list(
      list(ok = !input$fm_typ %in% c("Kund", "M\u00e4klare") ||
                  (!is.null(input$fm_koppling) && nzchar(trimws(input$fm_koppling))),
           msg = paste0("V\u00e4lj en koppling f\u00f6r FM-typ '", input$fm_typ, "'.") ),
      list(ok = is.null(input$fm_email) || !nzchar(input$fm_email) || grepl("@", input$fm_email, fixed = TRUE),
           msg = "E-postadressen verkar ogiltig (saknar @).")
    ))
    if (!isTRUE(show_validation_errors(errs))) return()
    
    fm <- sanitize_nulls_df(rv$wb[["FaktureringInformation"]])
    new_id <- next_id(fm, "FM")
    
    cust_id <- NA_character_
    makl_id <- NA_character_
    
    if (identical(input$fm_typ, "Kund") && !is.null(input$fm_koppling) && nzchar(input$fm_koppling)) {
      cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$fm_koppling)
    }
    if (identical(input$fm_typ, "M\u00e4klare") && !is.null(input$fm_koppling) && nzchar(input$fm_koppling)) {
      makl_id <- name_to_id(rv$labels$makl, "maklare_id", "maklare_name", input$fm_koppling)
    }
    
    new_row <- data.frame(
      faktura_mottagare_id = new_id,
      mottagare_typ = input$fm_typ,
      maklare_id = makl_id,
      customer_id = cust_id,
      kontakt_fornamn = input$fm_kfor,
      kontakt_efternamn = input$fm_kefter,
      email = input$fm_email,
      avtal_nummer = input$fm_avtal,
      referens = input$fm_ref,
      verksamhet = input$fm_verksamhet,
      kommentar = "",
      stringsAsFactors = FALSE
    )
    
    rv$wb[["FaktureringInformation"]] <- bind_rows(fm, new_row)
    reset_form(session, ids = c("fm_kfor","fm_kefter","fm_email","fm_avtal","fm_ref","fm_verksamhet"))
  })
  
  # ===================== ADD: UPPDRAG =====================
  
  observeEvent(input$btn_add_uppdrag, {
    req(rv$wb, input$upp_namn, input$upp_kund, rv$labels)
    
    errs <- validate_add_form(list(
      list(
        ok = { s <- safe_as_date(input$upp_start); e <- safe_as_date(input$upp_slut)
               is.na(s) || is.na(e) || s <= e },
        msg = "Startdatum m\u00e5ste vara f\u00f6re eller lika med slutdatum."
      )
    ))
    if (!isTRUE(show_validation_errors(errs))) return()
    
    upp <- sanitize_nulls_df(rv$wb[["Uppdrag"]]) |> coerce_dates(c("startdatum","slutdatum"))
    cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$upp_kund)
    new_id <- next_id(upp, "UPP")
    
    new_row <- data.frame(
      uppdrag_id = new_id,
      customer_id = cust_id,
      uppdrag_name = input$upp_namn,
      beskrivning = input$upp_besk,
      faktura_mottagare_id = "",
      startdatum = safe_as_date(input$upp_start),
      slutdatum = safe_as_date(input$upp_slut),
      stringsAsFactors = FALSE
    ) |> coerce_dates(c("startdatum","slutdatum"))
    
    rv$wb[["Uppdrag"]] <- bind_rows(upp, new_row)

    if (!isTRUE(persist_after_add(
      changed_sheets = c("Uppdrag"),
      refresh_labels = TRUE,
      refresh_selects = c("tid_upp")
    ))) return()

    reset_form(session, ids = c("upp_namn","upp_kund","upp_besk","upp_start","upp_slut"))
  })
  
  # ===================== ADD: UPPGIFT (+ TimprisHistory) =====================
  
  observeEvent(input$btn_add_uppgift, {
    req(rv$wb, input$uppg_kons, input$uppg_kund, input$uppg_upp, rv$labels)
    
    errs <- validate_add_form(list(
      list(
        ok = is.null(input$uppg_timpris) || is.na(input$uppg_timpris) || as.numeric(input$uppg_timpris) >= 0,
        msg = "Timpris m\u00e5ste vara 0 eller positivt."
      )
    ))
    if (!isTRUE(show_validation_errors(errs))) return()
    
    df <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
      ensure_uppgift_timpris() |>
      coerce_dates(c("startdatum","created_at"))
    
    cons_id <- name_to_id(rv$labels$kons, "consultant_id", "consultant_name", input$uppg_kons)
    cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$uppg_kund)
    
    upp_tbl <- sanitize_nulls_df(rv$wb[["Uppdrag"]]) |>
      mutate(uppdrag_label = ifelse(is.na(uppdrag_name) | uppdrag_name == "", uppdrag_id, uppdrag_name))
    
    upp_id <- upp_tbl$uppdrag_id[match(input$uppg_upp, upp_tbl$uppdrag_label)]
    upp_id <- ifelse(is.na(upp_id), input$uppg_upp, upp_id)
    
    new_id <- next_id(df, "TASK")
    
    new_row <- data.frame(
      uppgift_id = new_id,
      uppgift_name = input$uppg_name,
      consultant_id = cons_id,
      uppdrag_id = upp_id,
      customer_id = cust_id,
      startdatum = safe_as_date(input$uppg_start),
      timpris = ifelse(is.na(input$uppg_timpris), NA_real_, as.numeric(input$uppg_timpris)),
      created_at = Sys.Date(),
      stringsAsFactors = FALSE
    ) |> coerce_dates(c("startdatum","created_at"))
    
    rv$wb[["Uppgift"]] <- bind_rows(df, new_row)
    
    if (!is.na(new_row$timpris[1])) {
      rv$wb <- ensure_timpris_history_sheet(rv$wb)
      th <- sanitize_nulls_df(rv$wb[["TimprisHistory"]])
      
      hist_row <- data.frame(
        timpris_id = next_id(th, "TP"),
        consultant_id = cons_id,
        uppdrag_id = upp_id,
        uppgift_id = new_id,
        timpris = new_row$timpris[1],
        created_at = Sys.Date(),
        kommentar = "Auto fr\u00e5n Uppgift-formul\u00e4r",
        stringsAsFactors = FALSE
      ) |> coerce_dates(c("created_at"))
      
      rv$wb[["TimprisHistory"]] <- bind_rows(th, hist_row)
    }

    changed_sheets <- c("Uppgift")
    if (!is.na(new_row$timpris[1])) changed_sheets <- c(changed_sheets, "TimprisHistory")

    if (!isTRUE(persist_after_add(changed_sheets = changed_sheets))) return()

    reset_form(session, ids = c("uppg_kund","uppg_upp","uppg_name","uppg_kons","uppg_timpris","uppg_start"))
    showNotification("Ny uppgift tillagd!", type = "message", duration = 3)
  })
  
  # ===================== ADD: TIDRAPPORT (år/månad -> datumintervall) =====================
  
  observeEvent(input$btn_add_tid, {
    req(rv$wb, rv$labels, input$tid_kund, input$tid_upp, input$tid_task, input$tid_kons, input$tid_ar, input$tid_manad, input$tid_timmar)
    
    errs <- validate_add_form(list(
      list(
        ok = { h <- suppressWarnings(as.numeric(input$tid_timmar)); !is.na(h) && h > 0 },
        msg = "Timmar m\u00e5ste vara ett positivt tal (> 0)."
      )
    ))
    if (!isTRUE(show_validation_errors(errs))) return()
    
    df <- sanitize_nulls_df(rv$wb[["Tidrapportering"]]) |>
      ensure_cols(c("tidrapport_id","consultant_id","uppdrag_id","uppgift_id","customer_id","timmar","startdatum","slutdatum","created_at")) |>
      coerce_dates(c("startdatum","slutdatum","created_at"))
    
    y <- as.integer(input$tid_ar); m <- as.integer(input$tid_manad)
    mb <- month_bounds(y, m)
    
    cons_id <- name_to_id(rv$labels$kons, "consultant_id", "consultant_name", input$tid_kons)
    cust_id <- name_to_id(rv$labels$kunder, "customer_id", "customer_name", input$tid_kund)
    upp_id  <- name_to_id(rv$labels$upp, "uppdrag_id", "uppdrag_label", input$tid_upp)
    
    tasks <- sanitize_nulls_df(rv$wb[["Uppgift"]]) |>
      ensure_cols(c("uppgift_id","uppgift_name","customer_id","uppdrag_id","consultant_id")) |>
      mutate(
        customer_id   = as.character(customer_id),
        uppdrag_id    = as.character(uppdrag_id),
        uppgift_id    = as.character(uppgift_id),
        consultant_id = as.character(consultant_id),   # <-- LÄGG TILL
        uppgift_name  = ifelse(is.na(uppgift_name) | trimws(uppgift_name) == "", uppgift_id, uppgift_name),
        task_label    = paste0(uppgift_name, " (", uppgift_id, ")")
      )
    
    task_id <- tasks$uppgift_id[match(input$tid_task, tasks$task_label)]
    task_id <- ifelse(is.na(task_id), NA_character_, as.character(task_id))
   # New!!!
    # efter task_id är uträknad
    task_cons_id <- tasks$consultant_id[match(input$tid_task, tasks$task_label)]
    task_cons_id <- ifelse(is.na(task_cons_id) | task_cons_id == "", cons_id, as.character(task_cons_id))
    
    
    #NEW!!!
      
      
    
    new_tid_id <- generate_tidrapport_id(df, task_cons_id, input$tid_ar, input$tid_manad)
    
    new_row <- data.frame(
      tidrapport_id = new_tid_id,
      uppgift_id = task_id,
      customer_id = as.character(cust_id),
      timmar = suppressWarnings(as.numeric(input$tid_timmar)),
      startdatum = as.Date(mb$start),
      slutdatum = as.Date(mb$end),
      consultant_id = as.character(task_cons_id),
      uppdrag_id = as.character(upp_id),
      created_at = Sys.Date(),
      stringsAsFactors = FALSE
    ) |> coerce_dates(c("startdatum","slutdatum","created_at"))
    
    rv$wb[["Tidrapportering"]] <- bind_rows(df, new_row)

    if (!isTRUE(persist_after_add(c("Tidrapportering")))) return()
    
    reset_form(session, ids = c("tid_kons","tid_upp","tid_task","tid_ar","tid_manad","tid_timmar"),
               defaults = list(tid_ar = lubridate::year(Sys.Date()), tid_manad = lubridate::month(Sys.Date())))
  })
}
