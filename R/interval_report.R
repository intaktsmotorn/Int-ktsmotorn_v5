# ===================== INTERVALLRAPPORT =====================
# Temporal resolution helpers and main build function.
# Sourced at top level of app.r so all shared helpers are in scope.

# Returns the timpris to use for a (consultant, uppdrag, uppgift) combination
# in the month identified by target_first_day.
# Rule: a TimprisHistory change recorded in month X is active from month X.
#       → keep rows where created_at is before first day of month X+1.
# Fallback: current Uppgift$timpris via lookup_timpris_from_uppgift().
resolve_timpris_for_month <- function(timpris_history, uppgift_df,
                                       consultant_id, uppdrag_id, uppgift_id,
                                       target_first_day) {
  .cid <- as.character(consultant_id)
  .uid <- as.character(uppdrag_id)
  .tid <- as.character(uppgift_id)
  .next_month_first <- as.Date(target_first_day) %m+% months(1)

  th <- sanitize_nulls_df(timpris_history) |> coerce_dates(c("created_at"))

  if (nrow(th) > 0 &&
      all(c("consultant_id", "uppdrag_id", "uppgift_id", "timpris", "created_at") %in% names(th))) {
    th$consultant_id <- as.character(th$consultant_id)
    th$uppdrag_id    <- as.character(th$uppdrag_id)
    th$uppgift_id    <- as.character(th$uppgift_id)
    th$timpris       <- suppressWarnings(as.numeric(th$timpris))

    mask <- th$consultant_id == .cid &
            th$uppdrag_id    == .uid &
            th$uppgift_id    == .tid &
            !is.na(th$created_at) &
            th$created_at    <  .next_month_first &
            !is.na(th$timpris)

    hit <- th[mask, , drop = FALSE]
    if (nrow(hit) > 0) {
      hit <- hit[order(hit$created_at, decreasing = TRUE), , drop = FALSE]
      return(as.numeric(hit$timpris[1]))
    }
  }

  lookup_timpris_from_uppgift(uppgift_df,
                               consultant_id = .cid,
                               uppdrag_id    = .uid,
                               uppgift_id    = .tid)
}

# Returns the grundlön to use for a consultant in the year target_year.
#
# Case 1 – New consultant (startdatum falls in target_year or later):
#   Their register grundlön IS their starting salary.  Return it immediately;
#   any GrundlonHistory entries recorded in the same year apply from Jan 1
#   of the NEXT year (handled automatically by Case 2 in subsequent calls).
#
# Case 2 – Existing consultant (startdatum before target_year):
#   Yearly activation rule: a change recorded in year Y is active from Y+1.
#   → keep only rows where year(created_at) < target_year, pick the most recent.
#   Fallback: current Konsulter$grundlon, or 0 if missing.
resolve_grundlon_for_month <- function(grundlon_history, konsulter_df,
                                        consultant_id, target_first_day) {
  .cid  <- as.character(consultant_id)
  .fd   <- as.Date(target_first_day)
  .year <- as.integer(format(.fd, "%Y"))

  # Resolve register row once — needed for both cases and as fallback.
  kons <- sanitize_nulls_df(konsulter_df)
  kons$consultant_id <- as.character(kons$consultant_id)
  kons$grundlon      <- suppressWarnings(as.numeric(kons$grundlon))
  krow <- kons[kons$consultant_id == .cid, , drop = FALSE]
  reg_grundlon <- if (nrow(krow) > 0 && !is.na(krow$grundlon[1]))
    as.numeric(krow$grundlon[1]) else 0

  # Never count salary before actual employment start.
  if (nrow(krow) > 0 && "startdatum" %in% names(krow)) {
    sd <- suppressWarnings(as.Date(as.character(krow$startdatum[1])))
    if (!is.na(sd) && !is.na(.fd)) {
      ld <- as.Date(month_bounds(.year, as.integer(format(.fd, "%m")))$end)
      if (ld < sd) return(0)
    }
  }

  # Yearly activation rule:
  # - change recorded in January -> active from Jan 1 same year
  # - change recorded Feb-Dec  -> active from Jan 1 next year
  # For multiple changes mapping to the same active year, latest created_at wins.
  gl <- sanitize_nulls_df(grundlon_history) |> coerce_dates(c("created_at"))

  if (nrow(gl) > 0 &&
      all(c("consultant_id", "grundlon", "created_at") %in% names(gl))) {
    gl$consultant_id <- as.character(gl$consultant_id)
    gl$grundlon      <- suppressWarnings(as.numeric(gl$grundlon))
    gl$active_year   <- ifelse(
      lubridate::month(gl$created_at) == 1,
      as.integer(format(gl$created_at, "%Y")),
      as.integer(format(gl$created_at, "%Y")) + 1L
    )

    mask <- gl$consultant_id == .cid &
            !is.na(gl$created_at) &
            !is.na(gl$active_year) &
            gl$active_year <= .year &
            !is.na(gl$grundlon)

    hit <- gl[mask, , drop = FALSE]
    if (nrow(hit) > 0) {
      hit <- hit[order(hit$active_year, hit$created_at, decreasing = TRUE), , drop = FALSE]
      return(as.numeric(hit$grundlon[1]))
    }
  }

  reg_grundlon
}

# Returns the bonus_grund (%) to use for a consultant in the month at target_first_day.
# Rule: a BonusHistory change recorded in month X becomes active from the first day of month X+1.
#       → keep rows where created_at is strictly before first day of target month.
# Fallback: current Konsulter$bonus_grund, or 0 if missing.
resolve_bonus_for_month <- function(bonus_history, konsulter_df,
                                     consultant_id, target_first_day) {
  .cid <- as.character(consultant_id)

  bh <- sanitize_nulls_df(bonus_history) |> coerce_dates(c("created_at"))

  if (nrow(bh) > 0 &&
      all(c("consultant_id", "bonus_grund", "created_at") %in% names(bh))) {
    bh$consultant_id <- as.character(bh$consultant_id)
    bh$bonus_grund   <- suppressWarnings(as.numeric(bh$bonus_grund))

    mask <- bh$consultant_id == .cid &
            !is.na(bh$created_at) &
            bh$created_at    <  target_first_day &
            !is.na(bh$bonus_grund)

    hit <- bh[mask, , drop = FALSE]
    if (nrow(hit) > 0) {
      hit <- hit[order(hit$created_at, decreasing = TRUE), , drop = FALSE]
      return(as.numeric(hit$bonus_grund[1]))
    }
  }

  kons <- sanitize_nulls_df(konsulter_df)
  kons$consultant_id <- as.character(kons$consultant_id)
  kons$bonus_grund   <- suppressWarnings(as.numeric(kons$bonus_grund))
  row <- kons[kons$consultant_id == .cid, , drop = FALSE]
  if (nrow(row) > 0 && !is.na(row$bonus_grund[1])) as.numeric(row$bonus_grund[1]) else 0
}

# Returns the group_bonus (%) to use for a consultant in the month at target_first_day.
# Rule: a GroupBonusHistory change recorded in month X becomes active from the first day
#       of month X+1 → keep rows where created_at is strictly before first day of target month.
# Fallback: current Konsulter$group_bonus, or 0 if missing.
resolve_group_bonus_for_month <- function(group_bonus_history, konsulter_df,
                                           consultant_id, target_first_day) {
  .cid <- as.character(consultant_id)
  .fd  <- as.Date(target_first_day)

  gb <- sanitize_nulls_df(group_bonus_history) |> coerce_dates(c("created_at"))

  if (nrow(gb) > 0 &&
      all(c("consultant_id", "group_bonus", "created_at") %in% names(gb))) {
    gb$consultant_id <- as.character(gb$consultant_id)
    gb$group_bonus   <- suppressWarnings(as.numeric(gb$group_bonus))

    # Guard: !is.na(gb$consultant_id) prevents NA propagation in the mask.
    # Without it, NA == .cid evaluates to NA (not FALSE), and df[NA, ] in R
    # returns a row of all NAs, which would corrupt the result.
    mask <- !is.na(gb$consultant_id) &
            gb$consultant_id == .cid &
            !is.na(gb$created_at) &
            gb$created_at < .fd &
            !is.na(gb$group_bonus)

    hit <- gb[mask, , drop = FALSE]
    if (nrow(hit) > 0) {
      hit <- hit[order(hit$created_at, decreasing = TRUE), , drop = FALSE]
      return(as.numeric(hit$group_bonus[1]))
    }
  }

  kons <- sanitize_nulls_df(konsulter_df)
  kons$consultant_id <- as.character(kons$consultant_id)
  kons$group_bonus   <- suppressWarnings(as.numeric(kons$group_bonus))
  # Guard: !is.na() prevents df[NA, ] from returning a row of all NAs.
  row <- kons[!is.na(kons$consultant_id) & kons$consultant_id == .cid, , drop = FALSE]
  if (nrow(row) > 0 && !is.na(row$group_bonus[1])) as.numeric(row$group_bonus[1]) else 0
}

# Returns the sales_bonus (%) to use for a (consultant, customer) pair in the month
# at target_first_day.
#
# Activation rule: a change recorded in month X is active FROM month X (same month).
#   → keep rows where created_at < first day of month X+1
#   → if multiple entries in the same month, the latest created_at wins.
# Note: this differs from group/individual bonus (those activate from X+1).
#
# Lookup order:
#   1. Exact match:   SalesBonusHistory row for (consultant_id, customer_id).
#   2. Generic match: SalesBonusHistory row for (consultant_id, customer_id = NA).
#      History entries created without a specific customer (e.g. when the consultant
#      is first registered) have customer_id = NA and serve as a customer-agnostic rate.
#   3. Fallback:      Konsulter$sales_bonus for the consultant, or 0 if missing.
#
# Important: all mask comparisons guard with !is.na() before ==.
# In R, NA == x evaluates to NA (not FALSE), and df[NA, ] returns a row of all NAs,
# which would cause the function to return NA instead of continuing to the fallback.
resolve_sales_bonus_for_month <- function(sales_bonus_history, konsulter_df,
                                           consultant_id, customer_id, target_first_day) {
  .cid              <- as.character(consultant_id)
  .kid              <- as.character(customer_id)
  .fd               <- as.Date(target_first_day)
  .next_month_first <- .fd %m+% months(1)   # first day of the month after target

  sb <- sanitize_nulls_df(sales_bonus_history) |> coerce_dates(c("created_at"))

  if (nrow(sb) > 0 &&
      all(c("consultant_id", "customer_id", "sales_bonus", "created_at") %in% names(sb))) {
    sb$consultant_id <- as.character(sb$consultant_id)
    sb$customer_id   <- as.character(sb$customer_id)
    sb$sales_bonus   <- suppressWarnings(as.numeric(sb$sales_bonus))

    # Shared date-range predicate (recorded before the start of the NEXT month).
    in_range <- !is.na(sb$consultant_id) &
                sb$consultant_id == .cid &
                !is.na(sb$created_at) &
                sb$created_at    <  .next_month_first &
                !is.na(sb$sales_bonus)

    # --- Step 1: customer-specific entry ---
    # !is.na(sb$customer_id) is required: without it, NA == .kid evaluates to NA,
    # the mask contains NA, and df[NA, ] in R returns a row of all NAs.
    mask_exact <- in_range &
                  !is.na(sb$customer_id) &
                  sb$customer_id == .kid

    hit <- sb[mask_exact, , drop = FALSE]
    if (nrow(hit) > 0) {
      # Latest created_at wins when multiple entries exist for the same period.
      hit <- hit[order(hit$created_at, decreasing = TRUE), , drop = FALSE]
      return(as.numeric(hit$sales_bonus[1]))
    }

    # --- Step 2: customer-agnostic entry (customer_id = NA in history) ---
    # Used when the bonus was registered without a specific customer (e.g. on
    # consultant creation). Acts as a general rate before the Konsulter fallback.
    mask_generic <- in_range & is.na(sb$customer_id)

    hit_generic <- sb[mask_generic, , drop = FALSE]
    if (nrow(hit_generic) > 0) {
      hit_generic <- hit_generic[order(hit_generic$created_at, decreasing = TRUE), , drop = FALSE]
      return(as.numeric(hit_generic$sales_bonus[1]))
    }
  }

  # --- Step 3: fallback to current Konsulter$sales_bonus (not customer-specific) ---
  kons <- sanitize_nulls_df(konsulter_df)
  kons$consultant_id <- as.character(kons$consultant_id)
  kons$sales_bonus   <- suppressWarnings(as.numeric(kons$sales_bonus))
  # Guard: !is.na() prevents df[NA, ] from returning a row of all NAs.
  row <- kons[!is.na(kons$consultant_id) & kons$consultant_id == .cid, , drop = FALSE]
  if (nrow(row) > 0 && !is.na(row$sales_bonus[1])) as.numeric(row$sales_bonus[1]) else 0
}

# ===================== MAIN BUILD FUNCTION =====================
# Returns list(detail, summary, totals).
# - detail:  one row per (month, consultant, customer)
# - summary: one row per (month, consultant) with resolved grundlön/bonus
# - totals:  one row per consultant, summed across the whole interval
build_interval_report <- function(wb, labels,
                                   start_year, start_month,
                                   end_year,   end_month,
                                   bonus_threshold = BONUS_THRESHOLD,
                                   consultant_filter = NULL) {

  start_year  <- as.integer(start_year)
  start_month <- as.integer(start_month)
  end_year    <- as.integer(end_year)
  end_month   <- as.integer(end_month)

  # Helper: empty data frame with the bonus summary schema.
  empty_bonus_summary <- function() data.frame(
    period_date        = as.Date(character(0)),
    bonus_type         = character(0),
    rapporterande_name = character(0),
    customer_name      = character(0),
    uppdrag_label      = character(0),
    uppgift_label      = character(0),
    bonus_procent      = numeric(0),
    invoiced_amount    = numeric(0),
    bonus_amount       = numeric(0),
    stringsAsFactors   = FALSE
  )

  make_empty <- function() {
    list(
      detail = data.frame(
        period_date = as.Date(character(0)),
        fornamn = character(0), efternamn = character(0),
        kundnamn = character(0), uppdrag_label = character(0),
        timmar = numeric(0), timpris_used = numeric(0),
        total_summa = numeric(0), debiteringsgrad = numeric(0),
        stringsAsFactors = FALSE
      ),
      summary = data.frame(
        period_date = as.Date(character(0)),
        fornamn = character(0), efternamn = character(0),
        total_debiterad = numeric(0), total_timmar = numeric(0),  # [+] total_timmar
        grundlon_used = numeric(0), bonus_percent_used = numeric(0),
        bonus_belopp = numeric(0), lon_total = numeric(0),
        debiteringsgrad = numeric(0),
        stringsAsFactors = FALSE
      ),
      totals = data.frame(
        fornamn = character(0), efternamn = character(0),
        total_debiterad = numeric(0), total_timmar = numeric(0),
        grundlon_total = numeric(0), bonus_belopp_total = numeric(0),
        lon_total = numeric(0), snitt_debiteringsgrad = numeric(0),  # [+]
        stringsAsFactors = FALSE
      ),
      monthly_grand_summaries = data.frame(
        period_date                 = as.Date(character(0)),
        sum_total_debiterad         = numeric(0),
        sum_bonus_belopp            = numeric(0),
        sum_lon_total               = numeric(0),
        snitt_total_timmar          = numeric(0),
        snitt_debiteringsgrad_monad = numeric(0),
        stringsAsFactors = FALSE
      ),
      arb_hours_by_month  = list(),
      group_bonus_summary = empty_bonus_summary(),
      sales_bonus_summary = empty_bonus_summary()
    )
  }

  if (anyNA(c(start_year, start_month, end_year, end_month))) return(make_empty())
  if (start_month < 1 || start_month > 12 || end_month < 1 || end_month > 12) return(make_empty())

  start_first <- as.Date(sprintf("%04d-%02d-01", start_year, start_month))
  end_first   <- as.Date(sprintf("%04d-%02d-01", end_year,   end_month))
  if (start_first > end_first) return(make_empty())

  month_firsts <- seq(start_first, end_first, by = "month")

  # Pre-load all source tables once
  tid     <- sanitize_nulls_df(wb[["Tidrapportering"]]) |>
               coerce_dates(c("startdatum", "slutdatum", "created_at"))
  uppgift <- sanitize_nulls_df(wb[["Uppgift"]]) |> ensure_uppgift_timpris()
  kons    <- sanitize_nulls_df(wb[["Konsulter"]])
  kunder  <- sanitize_nulls_df(wb[["Kunder"]])
  arb     <- sanitize_nulls_df(wb[["ArbetstimmarGrund"]]) |> coerce_dates(c("date"))

  th_hist <- if (!is.null(wb[["TimprisHistory"]]))
    sanitize_nulls_df(wb[["TimprisHistory"]]) |> coerce_dates(c("created_at"))
  else data.frame()

  gl_hist <- if (!is.null(wb[["GrundlonHistory"]]))
    sanitize_nulls_df(wb[["GrundlonHistory"]]) |> coerce_dates(c("created_at"))
  else data.frame()

  bh_hist <- if (!is.null(wb[["BonusHistory"]]))
    sanitize_nulls_df(wb[["BonusHistory"]]) |> coerce_dates(c("created_at"))
  else data.frame()

  if (nrow(tid) == 0) return(make_empty())

  tid <- tid |> mutate(
    consultant_id = as.character(consultant_id),
    uppdrag_id    = as.character(uppdrag_id),
    uppgift_id    = as.character(uppgift_id),
    customer_id   = as.character(customer_id),
    timmar        = suppressWarnings(as.numeric(timmar))
  )

  if (!is.null(consultant_filter) && length(consultant_filter) > 0) {
    tid <- tid[tid$consultant_id %in% as.character(consultant_filter), , drop = FALSE]
  }

  kons_names <- kons |>
    mutate(consultant_id = as.character(consultant_id),
           fornamn       = as.character(fornamn),
           efternamn     = as.character(efternamn)) |>
    select(consultant_id, fornamn, efternamn)

  kund_map <- kunder |>
    mutate(
      customer_id = as.character(customer_id),
      kundnamn    = ifelse(is.na(customer_namn) | customer_namn == "",
                           customer_id, as.character(customer_namn))
    ) |>
    select(customer_id, kundnamn)

  uppdrag_map <- sanitize_nulls_df(wb[["Uppdrag"]]) |>
    mutate(
      uppdrag_id    = as.character(uppdrag_id),
      uppdrag_label = ifelse(is.na(uppdrag_name) | trimws(as.character(uppdrag_name)) == "",
                             uppdrag_id, as.character(uppdrag_name))
    ) |>
    select(uppdrag_id, uppdrag_label)

  monthly_detail  <- vector("list", length(month_firsts))
  monthly_summary <- vector("list", length(month_firsts))
  monthly_gs_list <- vector("list", length(month_firsts))  # [+] one row per month
  arb_hours_list  <- list()

  for (i in seq_along(month_firsts)) {
    fd <- month_firsts[[i]]
    y  <- as.integer(format(fd, "%Y"))
    m  <- as.integer(format(fd, "%m"))
    ld <- as.Date(month_bounds(y, m)$end)

    # Overlap filter: startdatum <= last_day AND slutdatum >= first_day
    tid_m <- tid[!is.na(tid$startdatum) & !is.na(tid$slutdatum) &
                   tid$startdatum <= ld & tid$slutdatum >= fd, , drop = FALSE]
    if (nrow(tid_m) == 0) next

    # ArbetstimmarGrund for this month
    hit_arb   <- arb[!is.na(arb$date) & arb$date == fd, , drop = FALSE]
    arb_hours <- if (nrow(hit_arb) > 0) {
      v <- suppressWarnings(as.numeric(hit_arb$arbetstimmar[1]))
      if (is.na(v)) 0 else v
    } else 0
    arb_hours_list[[sprintf("%04d%02d", y, m)]] <- arb_hours

    # Resolve timpris per Tidrapportering row using history
    tid_m$timpris_used <- vapply(seq_len(nrow(tid_m)), function(j) {
      resolve_timpris_for_month(
        timpris_history  = th_hist,
        uppgift_df       = uppgift,
        consultant_id    = tid_m$consultant_id[j],
        uppdrag_id       = tid_m$uppdrag_id[j],
        uppgift_id       = tid_m$uppgift_id[j],
        target_first_day = fd
      )
    }, numeric(1))

    tid_m$total_summa <- tid_m$timmar * tid_m$timpris_used

    # Attach consultant names, customer names and uppdrag names
    tid_m_named <- merge(tid_m,     kons_names,  by = "consultant_id", all.x = TRUE)
    tid_m_named <- merge(tid_m_named, kund_map,  by = "customer_id",   all.x = TRUE)
    tid_m_named <- merge(tid_m_named, uppdrag_map, by = "uppdrag_id",  all.x = TRUE)
    is_na_kund  <- is.na(tid_m_named$kundnamn)
    tid_m_named$kundnamn[is_na_kund]         <- tid_m_named$customer_id[is_na_kund]
    is_na_upp   <- is.na(tid_m_named$uppdrag_label)
    tid_m_named$uppdrag_label[is_na_upp]     <- tid_m_named$uppdrag_id[is_na_upp]

    # Detail: group by consultant + customer + uppdrag
    detail_m <- tid_m_named |>
      mutate(period_date = fd) |>
      group_by(period_date, fornamn, efternamn, kundnamn, uppdrag_label) |>
      summarise(
        timmar      = sum(timmar,      na.rm = TRUE),
        total_summa = sum(total_summa, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        timpris_used    = ifelse(timmar > 0, total_summa / timmar, NA_real_),
        debiteringsgrad = ifelse(
          arb_hours > 0 & !is.na(timpris_used),
          round((total_summa / (arb_hours * timpris_used)) * 100, 2),
          NA_real_
        )
      ) |>
      select(period_date, fornamn, efternamn, kundnamn, uppdrag_label,
             timmar, timpris_used, total_summa, debiteringsgrad) |>
      arrange(fornamn, efternamn, kundnamn, uppdrag_label)

    # Resolve grundlön and bonus per unique consultant for this month
    cons_ids <- unique(tid_m$consultant_id)
    cons_resolved <- data.frame(
      consultant_id    = cons_ids,
      grundlon_used    = vapply(cons_ids, function(cid)
        resolve_grundlon_for_month(gl_hist, kons, cid, fd),       numeric(1)),
      bonus_grund_used = vapply(cons_ids, function(cid)
        resolve_bonus_for_month(bh_hist, kons, cid, fd), numeric(1)),
      stringsAsFactors = FALSE
    )
    # Attach names for joining to detail (which groups by fornamn/efternamn)
    cons_resolved <- merge(cons_resolved, kons_names, by = "consultant_id", all.x = TRUE)

    # Summary: group by consultant for this month
    summary_m <- detail_m |>
      group_by(period_date, fornamn, efternamn) |>
      summarise(
        total_debiterad = sum(total_summa,    na.rm = TRUE),
        total_timmar    = sum(timmar,          na.rm = TRUE),  # [+] sum hours per consultant/month
        # [*] correct formula: total consultant hours / scheduled hours for the month
        debiteringsgrad = if (arb_hours > 0)
                            round((sum(timmar, na.rm = TRUE) / arb_hours) * 100, 2)
                          else NA_real_,
        .groups = "drop"
      ) |>
      left_join(
        cons_resolved |> select(fornamn, efternamn, grundlon_used, bonus_grund_used),
        by = c("fornamn", "efternamn")
      ) |>
      mutate(
        grundlon_used      = ifelse(is.na(grundlon_used),    0, grundlon_used),
        bonus_percent_used = ifelse(is.na(bonus_grund_used), 0, bonus_grund_used),
        bonus_rate         = bonus_percent_used / 100,
        bonus_belopp       = ifelse(total_debiterad > bonus_threshold,
                                    (total_debiterad - bonus_threshold) * bonus_rate,
                                    0),
        lon_total          = grundlon_used + bonus_belopp
      ) |>
      select(period_date, fornamn, efternamn, total_debiterad, total_timmar,  # [+] total_timmar
             grundlon_used, bonus_percent_used, bonus_belopp, lon_total, debiteringsgrad) |>
      arrange(fornamn, efternamn)

    # Monthly grand summary: one row covering all consultants for this month.
    # snitt_debiteringsgrad_monad = sum(all consultant timmar) / (arb_hours * n_consultants) * 100.
    mk <- sprintf("%04d%02d", y, m)
    monthly_gs_list[[i]] <- data.frame(
      period_date                 = fd,                                              # Date: first day of month
      sum_total_debiterad         = sum(summary_m$total_debiterad, na.rm = TRUE),
      sum_bonus_belopp            = sum(summary_m$bonus_belopp,    na.rm = TRUE),
      sum_lon_total               = sum(summary_m$lon_total,       na.rm = TRUE),
      snitt_total_timmar          = round(mean(summary_m$total_timmar, na.rm = TRUE), 2),
      snitt_debiteringsgrad_monad = if (arb_hours > 0 && nrow(summary_m) > 0)
        round(sum(summary_m$total_timmar, na.rm = TRUE) /
                (arb_hours * nrow(summary_m)) * 100, 2)
      else NA_real_,
      stringsAsFactors = FALSE
    )

    monthly_detail[[i]]  <- detail_m
    monthly_summary[[i]] <- summary_m
  }

  monthly_detail  <- Filter(Negate(is.null), monthly_detail)
  monthly_summary <- Filter(Negate(is.null), monthly_summary)
  monthly_gs_list <- Filter(Negate(is.null), monthly_gs_list)  # [+]

  if (length(monthly_detail) == 0) return(make_empty())

  detail_all  <- bind_rows(monthly_detail)
  summary_all <- bind_rows(monthly_summary)

  # Total scheduled hours across the whole selected interval (same for all consultants).
  # Used as the denominator in the weighted-average debiteringsgrad.
  total_arb_hours_interval <- sum(unlist(arb_hours_list), na.rm = TRUE)  # [+]

  # Interval totals: one row per consultant, summed across all months.
  # snitt_debiteringsgrad is a weighted average: sum(timmar) / sum(arb_hours) * 100.
  totals <- summary_all |>
    group_by(fornamn, efternamn) |>
    summarise(
      total_debiterad    = sum(total_debiterad,  na.rm = TRUE),
      total_timmar       = sum(total_timmar,     na.rm = TRUE),
      grundlon_total     = sum(grundlon_used,    na.rm = TRUE),
      bonus_belopp_total = sum(bonus_belopp,     na.rm = TRUE),
      lon_total          = sum(lon_total,        na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # [+] weighted average: consultant's total hours / total scheduled hours in interval
      snitt_debiteringsgrad = if (total_arb_hours_interval > 0)
        round((total_timmar / total_arb_hours_interval) * 100, 2)
      else NA_real_
    ) |>
    arrange(fornamn, efternamn)

  monthly_grand_summaries <- if (length(monthly_gs_list) > 0)   # [+]
    bind_rows(monthly_gs_list)
  else
    data.frame(period_date = as.Date(character(0)),
               sum_total_debiterad = numeric(0), sum_bonus_belopp = numeric(0),
               sum_lon_total = numeric(0), snitt_total_timmar = numeric(0),
               snitt_debiteringsgrad_monad = numeric(0), stringsAsFactors = FALSE)

  # ---- BonusRapportering summaries ----------------------------------------
  # Skipped when consultant_filter is active (bonus is only meaningful for all consultants).
  if (!is.null(consultant_filter) && length(consultant_filter) > 0) {
    group_bonus_summary <- empty_bonus_summary()
    sales_bonus_summary <- empty_bonus_summary()
  } else {

  # Build one row per registered bonus entry that falls inside the interval.
  # period_date = start_date of the registration (already stored as the 1st of the month).

  br_raw <- sanitize_nulls_df(wb[["BonusRapportering"]]) |>
    coerce_dates(c("start_date"))

  bonus_cols_ok <- nrow(br_raw) > 0 && "start_date" %in% names(br_raw)

  if (bonus_cols_ok) {
    br_raw$period_date      <- as.Date(br_raw$start_date)
    br_raw$rapporterande_id <- as.character(br_raw$rapporterande_id)
    br_raw$customer_id      <- as.character(br_raw$customer_id)
    br_raw$uppdrag_id       <- as.character(br_raw$uppdrag_id)
    br_raw$uppgift_id       <- as.character(br_raw$uppgift_id)
    br_raw$bonus_type       <- as.character(br_raw$bonus_type)
    br_raw$bonus_procent    <- suppressWarnings(as.numeric(br_raw$bonus_procent))
    br_raw$invoiced_amount  <- suppressWarnings(as.numeric(br_raw$invoiced_amount))
    br_raw$bonus_amount     <- suppressWarnings(as.numeric(br_raw$bonus_amount))

    # Keep only rows whose period falls in the selected interval
    br_filt <- br_raw[!is.na(br_raw$period_date) &
                        br_raw$period_date >= start_first &
                        br_raw$period_date <= end_first, , drop = FALSE]
  } else {
    br_filt <- data.frame()
  }

  if (nrow(br_filt) > 0) {
    # Uppgift map (not pre-built above): uppgift_name (uppgift_id) as label
    uppgift_map2 <- sanitize_nulls_df(wb[["Uppgift"]]) |>
      mutate(
        uppgift_id    = as.character(uppgift_id),
        uppgift_name  = as.character(uppgift_name),
        uppgift_label = ifelse(
          is.na(uppgift_name) | trimws(uppgift_name) == "",
          uppgift_id,
          paste0(uppgift_name, " (", uppgift_id, ")")
        )
      ) |>
      select(uppgift_id, uppgift_label)

    # rapporterande_name: fornamn + efternamn from the already-built kons_names map
    rap_map <- kons_names |>
      mutate(rapporterande_name = trimws(paste(fornamn, efternamn))) |>
      select(consultant_id, rapporterande_name)

    # Join all display-name lookups onto the filtered rows
    br_named <- merge(br_filt,   rap_map,      by.x = "rapporterande_id",
                                               by.y = "consultant_id", all.x = TRUE)
    br_named <- merge(br_named,  kund_map,     by = "customer_id",     all.x = TRUE)
    br_named <- merge(br_named,  uppdrag_map,  by = "uppdrag_id",      all.x = TRUE)
    br_named <- merge(br_named,  uppgift_map2, by = "uppgift_id",      all.x = TRUE)

    # Fall back to raw IDs wherever display names could not be resolved
    br_named$rapporterande_name[is.na(br_named$rapporterande_name)] <-
      br_named$rapporterande_id[is.na(br_named$rapporterande_name)]
    br_named$kundnamn[is.na(br_named$kundnamn)] <-
      br_named$customer_id[is.na(br_named$kundnamn)]
    br_named$uppdrag_label[is.na(br_named$uppdrag_label)] <-
      br_named$uppdrag_id[is.na(br_named$uppdrag_label)]
    br_named$uppgift_label[is.na(br_named$uppgift_label)] <-
      br_named$uppgift_id[is.na(br_named$uppgift_label)]

    # Map internal codes ("group_bonus" / "sales_bonus") to display labels
    br_named$bonus_type_lbl <- ifelse(
      br_named$bonus_type == "group_bonus", "Group", "Sales"
    )

    # Build the output data frame with the required column set
    br_out <- data.frame(
      period_date        = br_named$period_date,
      bonus_type         = br_named$bonus_type_lbl,
      rapporterande_name = br_named$rapporterande_name,
      customer_name      = br_named$kundnamn,
      uppdrag_label      = br_named$uppdrag_label,
      uppgift_label      = br_named$uppgift_label,
      bonus_procent      = br_named$bonus_procent,
      invoiced_amount    = br_named$invoiced_amount,
      bonus_amount       = br_named$bonus_amount,
      stringsAsFactors   = FALSE
    )
    br_out <- br_out[order(br_out$period_date, br_out$rapporterande_name), , drop = FALSE]

    group_bonus_summary <- br_out[br_out$bonus_type == "Group", , drop = FALSE]
    sales_bonus_summary <- br_out[br_out$bonus_type == "Sales", , drop = FALSE]
  } else {
    group_bonus_summary <- empty_bonus_summary()
    sales_bonus_summary <- empty_bonus_summary()
  }

  } # end consultant_filter else-branch
  # --------------------------------------------------------------------------

  # Safety filter: guarantee detail/summary/totals only contain the requested
  # consultants, regardless of whether the earlier tid filter fully propagated.
  if (!is.null(consultant_filter) && length(consultant_filter) > 0) {
    keep_ids <- as.character(consultant_filter)
    kf       <- kons_names[as.character(kons_names$consultant_id) %in% keep_ids,
                            , drop = FALSE]
    keep_key <- paste(as.character(kf$fornamn), as.character(kf$efternamn))
    filter_by_kons <- function(df) {
      if (nrow(df) == 0 || !all(c("fornamn", "efternamn") %in% names(df))) return(df)
      df[paste(as.character(df$fornamn), as.character(df$efternamn)) %in% keep_key,
         , drop = FALSE]
    }
    detail_all  <- filter_by_kons(detail_all)
    summary_all <- filter_by_kons(summary_all)
    totals      <- filter_by_kons(totals)
  }

  list(detail = detail_all, summary = summary_all, totals = totals,
       monthly_grand_summaries = monthly_grand_summaries,       # [+]
       arb_hours_by_month  = arb_hours_list,
       group_bonus_summary = group_bonus_summary,
       sales_bonus_summary = sales_bonus_summary)
}

# Wraps all output tables for write_xlsx.
interval_report_workbook_list <- function(report, include_bonus = TRUE) {
  out <- list(
    Detalj         = report$detail,
    Sammanfattning = report$summary,
    Totalt         = report$totals,
    `MånadsTotal`  = report$monthly_grand_summaries
  )
  if (include_bonus) {
    out$GroupBonusTotal <- report$group_bonus_summary
    out$SalesBonusTotal <- report$sales_bonus_summary
  }
  out
}
