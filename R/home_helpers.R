library(tidyverse)
library(htmltools)

# ==================================================
# MARGINALIA — HOMEPAGE HELPERS
# English / Français québécois
# ==================================================


# --------------------------------------------------
# 1. Prepare shared homepage data
# --------------------------------------------------

prepare_home <- function() {

  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  ) |>
    mutate(
      date_started = as.Date(date_started),
      date_finished = as.Date(date_finished)
    )

  # Support older catalogs
  if (!"cover_file" %in% names(books)) {
    books$cover_file <- NA_character_
  }

  reading_log <- read_csv(
    "data/reading_log.csv",
    col_types = cols(
      book_id = col_character(),
      date = col_date(),
      page_reached = col_integer()
    )
  )

  # Retrieve the latest recorded reading progress
  latest_progress <- reading_log |>
    group_by(book_id) |>
    arrange(date, .by_group = TRUE) |>
    slice_tail(n = 1) |>
    ungroup() |>
    select(book_id, page_reached)

  home_books <- books |>
    left_join(latest_progress, by = "book_id") |>
    mutate(
      current_page = coalesce(
        page_reached,
        baseline_page
      )
    )

  # Calculate homepage summary statistics
  books_finished <- sum(
    books$status == "Finished",
    na.rm = TRUE
  )

  books_reading <- sum(
    books$status == "Reading",
    na.rm = TRUE
  )

  pages_logged <- calculate_book_progress(
    books,
    reading_log
  ) |>
    summarise(
      total = sum(pages_read, na.rm = TRUE)
    ) |>
    pull(total)

  # Currently reading
  currently_reading <- home_books |>
    filter(status == "Reading") |>
    arrange(desc(date_started))

  # Five most recently completed books
  recently_completed <- home_books |>
    filter(status == "Finished") |>
    arrange(desc(date_finished)) |>
    slice_head(n = 5)

  list(
    books = home_books,
    books_reading = books_reading,
    books_finished = books_finished,
    pages_logged = pages_logged,
    currently_reading = currently_reading,
    recently_completed = recently_completed
  )
}


# --------------------------------------------------
# 2. Localize homepage summary labels
# --------------------------------------------------

home_summary_labels <- function(
    data,
    lang = "en"
) {

  lang <- match.arg(lang, c("en", "fr"))

  if (lang == "en") {

    return(list(
      reading = "Currently reading",
      finished = "Books completed",
      pages = "Pages logged"
    ))

  }

  list(

    reading = if (data$books_reading == 1L) {
      "Livre en cours"
    } else {
      "Livres en cours"
    },

    finished = if (data$books_finished == 1L) {
      "Livre terminé"
    } else {
      "Livres terminés"
    },

    pages = if (data$pages_logged == 1L) {
      "Page consignée"
    } else {
      "Pages consignées"
    }

  )
}


# --------------------------------------------------
# 3. Generate an individual book card
# --------------------------------------------------

home_book_card <- function(
    b,
    lang = "en",
    variant = c("compact", "featured")
) {

  lang <- match.arg(lang, c("en", "fr"))
  variant <- match.arg(variant)

  book_title <- b$title[[1]]
  author <- b$author[[1]]
  status <- b$status[[1]]
  total_pages <- b$total_pages[[1]]
  current_page <- b$current_page[[1]]

  # Link to the corresponding book page
  link <- m_book_link(
    b$book_page[[1]]
  )

  make_link <- function(content) {

    if (is.na(link)) {
      content
    } else {
      tags$a(href = link, content)
    }

  }

  # ------------------------------------------------
  # Resolve the book cover
  # ------------------------------------------------

  local_cover <- if (
    "cover_file" %in% names(b)
  ) {
    b$cover_file[[1]]
  } else {
    NA_character_
  }

  cover_url <- m_cover_url(
    cover_id = b$cover_id[[1]],
    cover_file = local_cover,
    size = "L"
  )

  cover <- if (!is.na(cover_url)) {

    tags$img(
      src = cover_url,
      alt = paste(
        m_text("cover_of", lang),
        book_title
      ),
      class = "book-cover",
      loading = "lazy"
    )

  } else {

    tags$div(
      class = "book-cover-placeholder",
      m_text("cover_unavailable", lang)
    )

  }

  # ------------------------------------------------
  # Generate reading progress information
  # ------------------------------------------------

  details <- NULL

  if (
    status == "Reading" &&
    !is.na(current_page) &&
    !is.na(total_pages) &&
    total_pages > 0
  ) {

    progress <- round(
      100 * current_page / total_pages,
      1
    )

    progress <- max(0, min(100, progress))

    # Use localized visible labels
    progress_label <- paste0(
      "Page ",
      m_number(current_page, lang),

      if (lang == "fr") " sur " else " of ",

      m_number(total_pages, lang),
      " · ",
      m_percent(progress, lang)
    )

    aria_label <- paste(
      if (lang == "fr") {
        "Progression de la lecture de"
      } else {
        "Reading progress for"
      },
      book_title
    )

    # CSS and accessibility attributes require
    # decimal points, regardless of display language.
    progress_value <- format(
      progress,
      decimal.mark = ".",
      scientific = FALSE,
      trim = TRUE
    )

    details <- tags$div(

      class = "book-progress",

      tags$div(

        class = "book-progress-track",

        role = "progressbar",

        `aria-label` = aria_label,

        `aria-valuenow` = progress_value,

        `aria-valuemin` = "0",

        `aria-valuemax` = "100",

        tags$div(
          class = "book-progress-fill",

          style = paste0(
            "width: ",
            progress_value,
            "%;"
          )
        )

      ),

      tags$p(
        class = "book-progress-label",
        progress_label
      )

    )

  } else if (status == "Reading") {

    details <- tags$p(
      class = "book-card-meta",

      if (lang == "fr") {
        "Progression non consignée"
      } else {
        "Reading progress not yet logged"
      }
    )

  } else if (status == "Finished") {

    finished <- b$date_finished[[1]]

    finished_text <- if (is.na(finished)) {

      if (lang == "fr") {
        "Date de fin non consignée"
      } else {
        "Completion date not recorded"
      }

    } else {

      paste(
        if (lang == "fr") {
          "Lecture terminée le"
        } else {
          "Finished"
        },

        m_date(
          finished,
          lang,
          style = "long"
        )
      )

    }

    details <- tags$p(
      class = "book-card-meta",
      finished_text
    )

  } else {

    details <- tags$p(
      class = "book-card-meta",
      m_status(status, lang)
    )

  }

  # ------------------------------------------------
  # Localize the small status heading
  # ------------------------------------------------

  eyebrow <- if (status == "Reading") {

    if (lang == "fr") {
      "EN COURS DE LECTURE"
    } else {
      "CURRENTLY READING"
    }

  } else if (status == "Finished") {

    if (lang == "fr") {
      "LECTURE TERMINÉE"
    } else {
      "COMPLETED"
    }

  } else {

    toupper(m_status(status, lang))

  }

  # ------------------------------------------------
  # Assemble the complete book card
  # ------------------------------------------------

  tags$article(

    class = paste(
      "book-card",
      paste0("book-card--", variant)
    ),

    tags$div(
      class = "book-card-media",
      make_link(cover)
    ),

    tags$div(

      class = "book-card-content",

      tags$p(
        class = "book-card-eyebrow",
        eyebrow
      ),

      tags$h3(
        make_link(book_title)
      ),

      tags$p(
        class = "book-card-author",
        author
      ),

      details

    )
  )
}


# --------------------------------------------------
# 4. Display a collection of book cards
# --------------------------------------------------

show_home_books <- function(
    data,
    lang = "en",
    variant = c("compact", "featured")
) {

  lang <- match.arg(lang, c("en", "fr"))
  variant <- match.arg(variant)

  if (nrow(data) == 0) {

    return(
      tags$p(
        class = "empty-state",

        if (lang == "fr") {
          "Aucun livre pour le moment."
        } else {
          "No books to display yet."
        }
      )
    )

  }

  cards <- lapply(
    seq_len(nrow(data)),

    function(i) {

      home_book_card(
        data[i, ],
        lang = lang,
        variant = variant
      )

    }
  )

  container_class <- if (variant == "featured") {
    "featured-book-list"
  } else {
    "recent-book-grid"
  }

  do.call(
    tags$div,
    c(
      list(class = container_class),
      cards
    )
  )
}
