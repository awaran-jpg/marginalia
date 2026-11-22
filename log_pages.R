library(tidyverse)

source("R/reading_helpers.R")

# --------------------------------------------------
# 1. Record daily reading progress
# --------------------------------------------------

record_progress <- function(
    id,
    page,
    day = Sys.Date(),
    finish = FALSE
) {

  # Load existing data
  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )

  reading_log <- read_csv(
    "data/reading_log.csv",
    col_types = cols(
      book_id = col_character(),
      date = col_date(),
      page_reached = col_integer()
    )
  )

  # Validate the date
  day <- as.Date(day)

  if (length(day) != 1 || is.na(day)) {
    stop("Please provide a valid date.")
  }

  # Validate the page number
  if (
    length(page) != 1 ||
    !is.numeric(page) ||
    is.na(page) ||
    page < 0 ||
    page %% 1 != 0
  ) {
    stop("Page must be a nonnegative whole number.")
  }

  page <- as.integer(page)

  # Validate the finish option
  if (
    length(finish) != 1 ||
    !is.logical(finish) ||
    is.na(finish)
  ) {
    stop("finish must be TRUE or FALSE.")
  }

  # Retrieve the book
  book <- books |>
    filter(book_id == id)

  if (nrow(book) != 1) {
    stop("Book ID not found or not unique.")
  }

  # Check that the page is within the book
  if (
    !is.na(book$total_pages[[1]]) &&
    page > book$total_pages[[1]]
  ) {
    stop("Page reached exceeds the book's total pages.")
  }

  # Retrieve the book's existing reading history
  history <- reading_log |>
    filter(book_id == id) |>
    arrange(date)

  # Find the preceding entry
  previous <- history |>
    filter(date < day) |>
    slice_tail(n = 1)

  previous_page <- if (nrow(previous) > 0) {
    previous$page_reached[[1]]
  } else {
    book$baseline_page[[1]]
  }

  # Prevent progress from going backward
  if (
    !is.na(previous_page) &&
    page < previous_page
  ) {
    stop("Page reached cannot be less than the previous entry.")
  }

  # Check any later recorded entries
  next_entry <- history |>
    filter(date > day) |>
    slice_head(n = 1)

  if (
    nrow(next_entry) > 0 &&
    page > next_entry$page_reached[[1]]
  ) {
    stop("Page reached cannot exceed a later recorded entry.")
  }

  # --------------------------------------------------
  # Validate a request to finish the book
  # --------------------------------------------------

  if (finish) {

    # Require a known total page count
    if (is.na(book$total_pages[[1]])) {
      stop("Cannot finish a book without a total page count.")
    }

    # Require the final page
    if (page != book$total_pages[[1]]) {
      stop("To finish the book, record its final page.")
    }

    # The finishing date cannot precede the starting date
    if (
      !is.na(book$date_started[[1]]) &&
      day < book$date_started[[1]]
    ) {
      stop("Finishing date cannot precede starting date.")
    }

    # A completion record cannot precede later reading entries
    if (nrow(next_entry) > 0) {
      stop("Cannot mark a book finished before a later reading entry.")
    }

    # Protect an existing completion date
    if (
      !is.na(book$date_finished[[1]]) &&
      book$date_finished[[1]] != day
    ) {
      stop(
        "This book already has a different finishing date. ",
        "Review its existing record before changing it."
      )
    }
  }

  # --------------------------------------------------
  # Update the reading log
  # --------------------------------------------------

  # Remove any existing entry for the same book and date
  reading_log <- reading_log |>
    filter(!(book_id == id & date == day)) |>
    bind_rows(
      tibble(
        book_id = id,
        date = day,
        page_reached = page
      )
    ) |>
    arrange(date, book_id)

  # Calculate pages since the previous entry
  pages_read <- if (is.na(previous_page)) {
    NA_integer_
  } else {
    page - previous_page
  }

  # Save the updated reading log
  write_csv(
    reading_log,
    "data/reading_log.csv"
  )

  # --------------------------------------------------
  # Optionally mark the book as finished
  # --------------------------------------------------

  if (finish) {

    books <- books |>
      mutate(
        status = if_else(
          book_id == id,
          "Finished",
          status
        ),
        date_finished = if_else(
          book_id == id,
          day,
          as.Date(date_finished)
        )
      )

    write_csv(
      books,
      "data/books.csv"
    )
  }

  # --------------------------------------------------
  # Confirmation
  # --------------------------------------------------

  message(
    "Progress saved for ", book$title[[1]],
    ": page ", page, " on ", day
  )

  if (!is.na(pages_read)) {
    message("Pages since previous entry: ", pages_read)
  }

  if (finish) {
    message("Book marked as finished!")
  }

  invisible(reading_log)
}


# --------------------------------------------------
# 2. Display books currently being read
# --------------------------------------------------

show_current_books <- function() {

  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )

  reading_log <- read_csv(
    "data/reading_log.csv",
    col_types = cols(
      book_id = col_character(),
      date = col_date(),
      page_reached = col_integer()
    )
  )

  # Find the most recent record for each book
  latest_progress <- reading_log |>
    group_by(book_id) |>
    arrange(date, .by_group = TRUE) |>
    slice_tail(n = 1) |>
    ungroup() |>
    transmute(
      book_id,
      current_page = page_reached,
      last_logged = date
    )

  # Display currently reading books
  books |>
    filter(status == "Reading") |>
    left_join(
      latest_progress,
      by = "book_id"
    ) |>
    mutate(
      current_page = coalesce(
        current_page,
        baseline_page
      )
    ) |>
    select(
      book_id,
      title,
      current_page,
      total_pages,
      last_logged
    )
}


# --------------------------------------------------
# 3. Record progress using a book's title
# --------------------------------------------------

record_progress_by_title <- function(
    title,
    page,
    day = Sys.Date(),
    finish = FALSE
) {

  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )

  # Standardize the requested title
  requested_title <- str_to_lower(
    str_squish(title)
  )

  # Find matching books
  matches <- books |>
    filter(
      str_to_lower(str_squish(.data$title)) ==
        requested_title
    )

  if (nrow(matches) == 0) {
    stop("No book with that title was found.")
  }

  if (nrow(matches) > 1) {
    stop("Multiple books have this title. Use the book ID instead.")
  }

  # Use our existing record_progress function
  record_progress(
    id = matches$book_id[[1]],
    page = page,
    day = day,
    finish = finish
  )
}


# --------------------------------------------------
# 4. Display recent reading sessions
# --------------------------------------------------

show_recent_reading <- function(n = 10L) {

  # Validate the number of entries requested
  if (
    length(n) != 1 ||
    !is.numeric(n) ||
    is.na(n) ||
    n <= 0 ||
    n %% 1 != 0
  ) {
    stop("n must be a positive whole number.")
  }

  # Load the book catalog
  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )

  # Load the reading log
  reading_log <- read_csv(
    "data/reading_log.csv",
    col_types = cols(
      book_id = col_character(),
      date = col_date(),
      page_reached = col_integer()
    )
  )

  # Calculate pages read using our shared helper
  recent_reading <- calculate_book_progress(
    books,
    reading_log
  ) |>
    arrange(desc(date), book_id) |>
    select(
      Date = date,
      Book = title,
      `Pages Read` = pages_read,
      `Page Reached` = page_reached
    )

  # Return the most recent entries
  head(recent_reading, n)
}

