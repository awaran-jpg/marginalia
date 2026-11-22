library(tidyverse)

# --------------------------------------------------
# 1. Calculate reading progress across books
# --------------------------------------------------

calculate_book_progress <- function(
    books,
    reading_log
) {

  reading_log |>
    left_join(
      books |>
        select(
          book_id,
          title,
          baseline_page
        ),
      by = "book_id"
    ) |>
    group_by(book_id) |>
    arrange(
      date,
      .by_group = TRUE
    ) |>
    mutate(
      previous_page = coalesce(
        lag(page_reached),
        as.integer(baseline_page)
      ),
      pages_read = (
        page_reached - previous_page
      )
    ) |>
    ungroup()
}


# --------------------------------------------------
# 2. Retrieve an individual book
# --------------------------------------------------

get_book <- function(
    books,
    id
) {

  book <- books |>
    filter(book_id == id)

  if (nrow(book) != 1) {
    stop(
      "Expected exactly one matching book."
    )
  }

  return(book)
}


# --------------------------------------------------
# 3. Retrieve an individual book's reading history
# --------------------------------------------------

get_book_history <- function(
    books,
    reading_log,
    id
) {

  get_book(books, id)

  calculate_book_progress(
    books,
    reading_log
  ) |>
    filter(book_id == id) |>
    arrange(date)
}


# --------------------------------------------------
# 4. Generate the book information panel
# --------------------------------------------------

book_header <- function(
    books,
    reading_log,
    id
) {

  book <- get_book(
    books,
    id
  )

  history <- get_book_history(
    books,
    reading_log,
    id
  )

  # Format missing dates
  display_date <- function(x) {

    if (is.na(x)) {
      "Not recorded"
    } else {
      format(
        x,
        "%B %d, %Y"
      )
    }

  }

  # Format missing page counts
  display_pages <- function(x) {

    if (is.na(x)) {
      "Not recorded"
    } else {
      as.character(x)
    }

  }

  # --------------------------------------------------
  # Prepare book metadata
  # --------------------------------------------------

  details <- list(
    list(
      label = "Status",
      value = book$status[[1]]
    ),
    list(
      label = "Length",
      value = paste(
        display_pages(
          book$total_pages[[1]]
        ),
        "pages"
      )
    ),
    list(
      label = "Started",
      value = display_date(
        book$date_started[[1]]
      )
    ),
    list(
      label = "Finished",
      value = display_date(
        book$date_finished[[1]]
      )
    )
  )

  # Include reading progress when available
  if (nrow(history) > 0) {

    current_page <- dplyr::last(
      history$page_reached
    )

    details <- append(
      details,
      list(
        list(
          label = "Last recorded page",
          value = as.character(
            current_page
          )
        )
      )
    )

    if (
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
            label = "Reading progress",
            value = paste0(
              progress,
              "%"
            )
          )
        )
      )

    }
  }

  # --------------------------------------------------
  # Generate the book cover
  # --------------------------------------------------

  # Retrieve the optional local-cover path
  local_cover <- if (
    "cover_file" %in% names(book)
  ) {
    book$cover_file[[1]]
  } else {
    NA_character_
  }

  # Prefer local covers to Open Library
  cover_url <- if (
    !is.na(local_cover) &&
    nzchar(local_cover)
  ) {

    # Individual book pages live under books/,
    # so go up one directory to reach images/.
    paste0(
      "../",
      local_cover
    )

  } else if (
    !is.na(book$cover_id[[1]])
  ) {

    paste0(
      "https://covers.openlibrary.org/b/id/",
      book$cover_id[[1]],
      "-L.jpg"
    )

  } else {

    NA_character_

  }

  # Generate either the cover or placeholder
  cover <- if (!is.na(cover_url)) {

    htmltools::tags$img(
      src = cover_url,
      alt = paste(
        "Cover of",
        book$title[[1]]
      ),
      class = "book-detail-cover",
      loading = "lazy"
    )

  } else {

    htmltools::tags$div(
      class = "book-cover-placeholder",
      "Cover unavailable"
    )

  }

  # --------------------------------------------------
  # Generate individual metadata fields
  # --------------------------------------------------

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

  # --------------------------------------------------
  # Generate complete book information panel
  # --------------------------------------------------

  htmltools::tags$div(
    class = "book-detail",

    # Left column: cover
    htmltools::tags$div(
      class = "book-detail-image",
      cover
    ),

    # Right column: reading metadata
    htmltools::tags$div(
      class = "book-detail-info",

      htmltools::tags$p(
        class = "book-detail-eyebrow",
        "READING DETAILS"
      ),

      do.call(
        htmltools::tags$dl,
        c(
          list(
            class = "book-detail-grid"
          ),
          detail_fields
        )
      )

    )
  )
}


# --------------------------------------------------
# 5. Generate a book's reading history table
# --------------------------------------------------

book_history_table <- function(
    books,
    reading_log,
    id
) {

  history <- get_book_history(
    books,
    reading_log,
    id
  )

  # Mark empty histories so CSS can hide them
  if (nrow(history) == 0) {

    return(
      htmltools::tags$span(
        class = "reading-history-empty-marker",
        `aria-hidden` = "true"
      )
    )

  }

  table <- history |>
    select(
      date,
      pages_read,
      page_reached
    ) |>
    knitr::kable(
      format = "html",
      col.names = c(
        "Date",
        "Pages Read",
        "Page Reached"
      ),
      align = c(
        "l",
        "r",
        "r"
      )
    )

  htmltools::HTML(
    paste(
      table,
      collapse = "\n"
    )
  )
}
