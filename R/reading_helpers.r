library(tidyverse)

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

  # Verify that the book exists
  get_book(books, id)

  history <- calculate_book_progress(
    books,
    reading_log
  ) |>
    filter(book_id == id) |>
    arrange(date)

  return(history)

}


# --------------------------------------------------
# 4. Generate a reusable book information layout
# --------------------------------------------------

book_header <- function(books, reading_log, id) {

  # Retrieve book information and reading history
  book <- get_book(books, id)

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
      format(x, "%B %d, %Y")
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

  # Prepare book information
  details <- c(
    "Author" = book$author[[1]],
    "Status" = book$status[[1]],
    "Total pages" = display_pages(book$total_pages[[1]]),
    "Date started" = display_date(book$date_started[[1]]),
    "Date finished" = display_date(book$date_finished[[1]])
  )

  # Include current reading progress when available
  if (nrow(history) > 0) {

    current_page <- dplyr::last(
      history$page_reached
    )

    details["Current page"] <- as.character(
      current_page
    )

    if (
      !is.na(book$total_pages[[1]]) &&
      book$total_pages[[1]] > 0
    ) {

      progress_percent <- round(
        100 * current_page / book$total_pages[[1]],
        1
      )

      details["Progress"] <- paste0(
        progress_percent,
        "%"
      )

    }
  }

  # Construct cover URL using Open Library
  cover_url <- if (!is.na(book$cover_id[[1]])) {

    paste0(
      "https://covers.openlibrary.org/b/id/",
      book$cover_id[[1]],
      "-L.jpg"
    )

  } else {
    NA_character_
  }

  # Generate book cover
  cover <- if (!is.na(cover_url)) {

    htmltools::tags$img(
      src = cover_url,
      alt = paste(
        "Cover of",
        book$title[[1]]
      ),
      class = "book-detail-cover"
    )

  } else {

    htmltools::tags$p(
      "Cover not available"
    )

  }

  # Generate consistent two-column layout
  htmltools::tags$div(
    class = "book-detail",

    # Left column: cover
    htmltools::tags$div(
      class = "book-detail-image",
      cover
    ),

    # Right column: book information
    htmltools::tags$div(
      class = "book-detail-info",

      htmltools::tags$h2(
        "Book Information"
      ),

      htmltools::tagList(
        lapply(names(details), function(name) {

          htmltools::tags$p(
            htmltools::tags$strong(
              paste0(name, ": ")
            ),
            details[[name]]
          )

        })
      )
    )
  )

}


# --------------------------------------------------
# 5. Generate an individual book's reading history
# --------------------------------------------------

book_history_table <- function(books, reading_log, id) {

  # Retrieve the book's reading records
  history <- get_book_history(
    books,
    reading_log,
    id
  )

  # Handle books without daily reading records
  if (nrow(history) == 0) {
    return(
      htmltools::tags$p(
        "No daily reading records are available for this book."
      )
    )
  }

  # Prepare the reading history table
  table <- history |>
    select(date, pages_read, page_reached) |>
    knitr::kable(
      format = "html",
      col.names = c(
        "Date",
        "Pages Read",
        "Page Reached"
      )
    )

  # Return the table as HTML
  htmltools::HTML(
    paste(table, collapse = "\n")
  )
}
