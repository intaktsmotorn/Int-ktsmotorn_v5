# Dropdown refresh: make_refresh_selects(session, rv)
# Returns refresh_selects_by_keys -- a closure with session and rv bound explicitly.
# Sourced globally (app.r). Called from server() to create a session-scoped refresh function.

make_refresh_selects <- function(session, rv) {
  function(keys, selected_tid_kund = NULL) {
    req(rv$labels)
    for (key in unique(keys)) {
      if (identical(key, "upp_kund")) {
        updateSelectInput(session, "upp_kund", choices = rv$labels$kunder$customer_name)
      } else if (identical(key, "uppg_kund")) {
        updateSelectInput(session, "uppg_kund", choices = rv$labels$kunder$customer_name)
      } else if (identical(key, "uppg_kons")) {
        updateSelectInput(session, "uppg_kons", choices = rv$labels$kons$consultant_name)
      } else if (identical(key, "tid_kons")) {
        updateSelectInput(session, "tid_kons", choices = rv$labels$kons$consultant_name)
      } else if (identical(key, "tid_kund")) {
        if (is.null(selected_tid_kund)) {
          updateSelectInput(session, "tid_kund", choices = rv$labels$kunder$customer_name)
        } else {
          updateSelectInput(session, "tid_kund", choices = rv$labels$kunder$customer_name, selected = selected_tid_kund)
        }
      } else if (identical(key, "tid_upp")) {
        updateSelectInput(session, "tid_upp", choices = rv$labels$upp$uppdrag_label)
      } else if (identical(key, "fm_koppling")) {
        updateSelectInput(session, "fm_koppling", choices = rv$labels$kunder$customer_name)
      } else if (identical(key, "kund_fm_maklare")) {
        updateSelectInput(session, "kund_fm_maklare", choices = rv$labels$makl$maklare_name)
      }
    }
    }
  }
