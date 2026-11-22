
library(tidyverse)

record_progress <- function(id, page, day = Sys.Date()) {

  # Check that the page number is valid
  if (length(page) != 1 || !is.numeric(page) ||
      is.na(page) || page < 0 || page %% 1 != 0) {
    stop("Page must be a non-negative whole number.")
  }

  day <- as.Date(day)

  # Check that the book exists
  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )

  if (!id %in% books$book_id) {
    stop("Book ID not found.")
  }

  # Check against the book's total page count
  total <- books$total_pages[match(id, books$book_id)]

  if (!is.na(total) && page > total) {
    stop("Page exceeds the book's total length.")
  }

  # Import previous progress records
  reading_log <- read_csv(
    "data/reading_log.csv",
    col_types = cols(
      book_id = col_character(),
      date = col_date(),
      page_reached = col_integer()
    )
  )

  # Remove an existing entry for the same book and date
  reading_log <- reading_log |>
    filter(!(book_id == id & date == day))

  # Add the new progress entry
  new_entry <- tibble(
    book_id = id,
    date = day,
    page_reached = as.integer(page)
  )

  updated_log <- reading_log |>
    bind_rows(new_entry) |>
    arrange(book_id, date)

  # Make sure progress does not move backwards
  book_progress <- updated_log |>
    filter(book_id == id) |>
    pull(page_reached)

  if (any(diff(book_progress) < 0)) {
    stop("Page counts cannot decrease over time.")
  }

  # Save the updated log
  write_csv(updated_log, "data/reading_log.csv")

  message("Reading progress saved.")
}
