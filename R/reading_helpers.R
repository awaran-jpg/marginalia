
library(tidyverse)

source("R/localization_helpers.R")

# ==================================================
# MARGINALIA — READING HELPERS
# English / Français québécois
# ==================================================


# --------------------------------------------------
# 1. Calculate reading progress across books
# --------------------------------------------------

calculate_book_progress <- function(books, reading_log) {

  reading_log |>
    left_join(
      books |>
        select(book_id, title, baseline_page),
      by = "book_id"
    ) |>
    group_by(book_id) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      previous_page = coalesce(
        lag(page_reached),
        as.integer(baseline_page)
      ),
      pages_read = page_reached - previous_page
    ) |>
    ungroup()
}


# --------------------------------------------------
# 2. Retrieve an individual book
# --------------------------------------------------

get_book <- function(books, id) {

  book <- books |>
    filter(book_id == id)

  if (nrow(book) != 1) {
    stop("Expected exactly one matching book.")
  }

  return(book)
}


# --------------------------------------------------
# 3. Retrieve an individual book's reading history
# --------------------------------------------------

get_book_history <- function(books, reading_log, id) {

  get_book(books, id)

  calculate_book_progress(
    books,
    reading_log
  ) |>
    filter(book_id == id) |>
    arrange(date)
}


# --------------------------------------------------
# 4. Generate a localized book information panel
# --------------------------------------------------

book_header <- function(
    books,
    reading_log,
    id,
    lang = "en"
) {

  lang <- match.arg(lang, c("en", "fr"))

  book <- get_book(books, id)

  history <- get_book_history(
    books,
    reading_log,
    id
  )

  # ------------------------------------------------
  # Format metadata
  # ------------------------------------------------

  display_date <- function(x) {

    if (is.na(x)) {
      return(m_text("not_recorded", lang))
    }

    m_date(x, lang, style = "long")
  }

  display_length <- function(x) {

    if (is.na(x)) {
      return(m_text("not_recorded", lang))
    }

    paste(
      m_number(x, lang),
      if (x == 1) "page" else "pages"
    )
  }

  started_label <- if (lang == "fr") {
    "Début de lecture"
  } else {
    "Started"
  }

  finished_label <- if (lang == "fr") {
    "Fin de lecture"
  } else {
    "Finished"
  }

  # ------------------------------------------------
  # Prepare reading details
  # ------------------------------------------------

  details <- list(

    list(
      label = m_text("status", lang),
      value = m_status(
        book$status[[1]],
        lang
      )
    ),

    list(
      label = m_text("length", lang),
      value = display_length(
        book$total_pages[[1]]
      )
    ),

    list(
      label = started_label,
      value = display_date(
        book$date_started[[1]]
      )
    ),

    list(
      label = finished_label,
      value = display_date(
        book$date_finished[[1]]
      )
    )

  )

  # ------------------------------------------------
  # Include recorded reading progress
  # ------------------------------------------------

  if (nrow(history) > 0) {

    current_page <- dplyr::last(
      history$page_reached
    )

    details <- append(
      details,
      list(
        list(
          label = m_text("last_page", lang),
          value = m_number(
            current_page,
            lang
          )
        )
      )
    )

    if (
      !is.na(current_page) &&
      !is.na(book$total_pages[[1]]) &&
      book$total_pages[[1]] > 0
    ) {

      progress <- round(
        100 * current_page /
          book$total_pages[[1]],
        1
      )

      details <- append(
        details,
        list(
          list(
            label = m_text("progress", lang),
            value = m_percent(progress, lang)
          )
        )
      )

    }
  }

  # ------------------------------------------------
  # Resolve book cover
  # ------------------------------------------------

  local_cover <- if (
    "cover_file" %in% names(book)
  ) {
    book$cover_file[[1]]
  } else {
    NA_character_
  }

  cover_url <- m_cover_url(
    cover_id = book$cover_id[[1]],
    cover_file = local_cover,
    prefix = "../",
    size = "L"
  )

  cover <- if (!is.na(cover_url)) {

    htmltools::tags$img(
      src = cover_url,
      alt = paste(
        m_text("cover_of", lang),
        book$title[[1]]
      ),
      class = "book-detail-cover",
      loading = "lazy"
    )

  } else {

    htmltools::tags$div(
      class = "book-cover-placeholder",
      m_text("cover_unavailable", lang)
    )

  }

  # ------------------------------------------------
  # Generate metadata fields
  # ------------------------------------------------

  make_detail <- function(item) {

    htmltools::tags$div(
      class = "book-detail-field",

      htmltools::tags$dt(
        item$label
      ),

      htmltools::tags$dd(
        item$value
      )
    )
  }

  detail_fields <- lapply(
    details,
    make_detail
  )

  # ------------------------------------------------
  # Generate complete information panel
  # ------------------------------------------------

  htmltools::tags$div(

    class = "book-detail",

    htmltools::tags$div(
      class = "book-detail-image",
      cover
    ),

    htmltools::tags$div(

      class = "book-detail-info",

      htmltools::tags$p(
        class = "book-detail-eyebrow",
        toupper(
          m_text("reading_details", lang)
        )
      ),

      do.call(
        htmltools::tags$dl,
        c(
          list(class = "book-detail-grid"),
          detail_fields
        )
      )

    )
  )
}


# --------------------------------------------------
# 5. Generate localized reading history
# --------------------------------------------------

book_history_table <- function(
    books,
    reading_log,
    id,
    lang = "en"
) {

  lang <- match.arg(lang, c("en", "fr"))

  history <- get_book_history(
    books,
    reading_log,
    id
  )

  # Retain our existing empty-history CSS behavior
  if (nrow(history) == 0) {

    return(
      htmltools::tags$span(
        class = "reading-history-empty-marker",
        `aria-hidden` = "true"
      )
    )

  }

  # Prepare localized table
  table <- history |>
    transmute(
      date = m_date(
        date,
        lang,
        style = "iso"
      ),
      pages_read = m_number(
        pages_read,
        lang
      ),
      page_reached = m_number(
        page_reached,
        lang
      )
    ) |>
    knitr::kable(
      format = "html",
      col.names = m_text(
        c(
          "date",
          "pages_read",
          "page_reached"
        ),
        lang
      ),
      align = c("l", "r", "r")
    )

  htmltools::HTML(
    paste(table, collapse = "\n")
  )
}
