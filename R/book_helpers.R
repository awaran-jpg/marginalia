library(tidyverse)

# Add a new book to the Marginalia catalog
add_book <- function(
    title,
    author,
    total_pages = NA_integer_,
    status = "Want to Read",
    date_started = as.Date(NA),
    date_finished = as.Date(NA),
    cover_id = NA_integer_,
    baseline_page = NA_integer_,
    save = FALSE
) {

  # Load the existing catalog
  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )


  # Standardize titles and authors for comparison
  normalize_text <- function(x) {
    str_to_lower(str_squish(x))
  }

  # Check whether the book already exists
  duplicate <- (
    normalize_text(books$title) == normalize_text(title) &
      normalize_text(books$author) == normalize_text(author)
  )

  if (any(duplicate, na.rm = TRUE)) {

    if (save) {
      stop("This book is already in the catalog. Nothing was saved.")
    } else {
      warning("This book is already in the catalog. Preview only.")
    }

  }


  # Validate reading status
  valid_statuses <- c(
    "Want to Read", "Reading",
    "Finished", "Abandoned"
  )

  if (!status %in% valid_statuses) {
    stop("Invalid reading status.")
  }

  # Validate page count
  if (!is.na(total_pages) &&
      (total_pages <= 0 || total_pages %% 1 != 0)) {
    stop("Total pages must be a positive whole number.")
  }

  # Convert dates
  date_started <- as.Date(date_started)
  date_finished <- as.Date(date_finished)

  if (!is.na(date_started) &&
      !is.na(date_finished) &&
      date_finished < date_started) {
    stop("Finishing date cannot precede starting date.")
  }

  # Generate the next available book ID
  existing_ids <- as.integer(
    stringr::str_remove(books$book_id, "^B")
  )

  next_number <- if (length(existing_ids) == 0) {
    1L
  } else {
    max(existing_ids) + 1L
  }

  new_id <- sprintf("B%03d", next_number)

  # Construct the new book record
  new_book <- tibble(
    book_id = new_id,
    title = title,
    author = author,
    total_pages = as.integer(total_pages),
    status = status,
    date_started = date_started,
    date_finished = date_finished,
    book_page = paste0("books/", new_id, ".qmd"),
    cover_id = as.integer(cover_id),
    baseline_page = as.integer(baseline_page)
  )

  # Save only when explicitly requested
  if (save) {

    # Check that the page does not already exist
    if (file.exists(new_book$book_page[[1]])) {
      stop("A book page with this ID already exists.")
    }

    # Generate the individual book page
    create_book_page(new_book)

    # Add the book to the catalog
    books <- bind_rows(books, new_book)

    write_csv(books, "data/books.csv")

    message("Book added: ", new_id)
  }


  return(new_book)
}


# Search Open Library for book information
search_open_library <- function(title, author = NULL) {

  # Construct the API request
  url <- paste0(
    "https://openlibrary.org/search.json?title=",
    URLencode(title, reserved = TRUE)
  )

  # Add an author filter if supplied
  if (!is.null(author)) {
    url <- paste0(
      url,
      "&author=",
      URLencode(author, reserved = TRUE)
    )
  }

  # Request only the fields we need
  url <- paste0(
    url,
    "&fields=key,title,author_name,",
    "first_publish_year,cover_i",
    "&limit=10"
  )

  # Retrieve and parse the JSON response
  results <- jsonlite::fromJSON(url)

  # Handle searches with no matches
  if (nrow(results$docs) == 0) {
    message("No matching books found.")
    return(tibble())
  }

  # Return a tidy table of possible matches
  results$docs |>
    as_tibble() |>
    select(any_of(c(
      "title",
      "author_name",
      "first_publish_year",
      "cover_i",
      "key"
    )))
}


# Create a book entry from an Open Library search result

add_book_from_search <- function(
    results,
    row,
    ...,
    save = FALSE
) {

  # Check that the selected row exists
  if (length(row) != 1 ||
      is.na(row) ||
      row < 1 ||
      row > nrow(results) ||
      row %% 1 != 0) {
    stop("Invalid search result selection.")
  }

  # Retrieve the selected book
  selected <- results[row, ]

  # Extract its author
  author <- selected$author_name[[1]][1]

  # Extract its cover identifier
  cover_id <- if ("cover_i" %in% names(selected)) {
    as.integer(selected$cover_i[[1]])
  } else {
    NA_integer_
  }

  # Use our existing function
  add_book(
    title = selected$title[[1]],
    author = author,
    cover_id = cover_id,
    ...,
    save = save
  )
}


# Automatically generate a Quarto page for a new book

create_book_page <- function(book) {

  # Retrieve the book's information
  id <- book$book_id[[1]]
  title <- book$title[[1]]
  author <- book$author[[1]]
  path <- book$book_page[[1]]

  # Prevent accidental overwriting
  if (file.exists(path)) {
    stop("A page already exists at: ", path)
  }

  # Ensure the books directory exists
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Construct the Quarto document
  content <- c(
    "---",
    paste0(
      "title: ",
      jsonlite::toJSON(title, auto_unbox = TRUE)
    ),
    paste0(
      "subtitle: ",
      jsonlite::toJSON(author, auto_unbox = TRUE)
    ),
    "---",
    "",
    "```{r}",
    "#| include: false",
    "#| message: false",
    "",
    "library(tidyverse)",
    'source("R/reading_helpers.R")',
    "",
    'books <- read_csv("data/books.csv", show_col_types = FALSE)',
    "",
    "reading_log <- read_csv(",
    '  "data/reading_log.csv",',
    "  col_types = cols(",
    "    book_id = col_character(),",
    "    date = col_date(),",
    "    page_reached = col_integer()",
    "  )",
    ")",
    "",
    paste0('book_id <- "', id, '"'),
    "```",
    "",
    "```{r}",
    "#| echo: false",
    "book_header(books, reading_log, book_id)",
    "```",
    "",
    "## My Reflections",
    "",
    "Write your reflections here.",
    "",
    "## Reading History",
    "",
    "```{r}",
    "#| echo: false",
    "book_history_table(books, reading_log, book_id)",
    "```"
  )

  # Write the Quarto file
  writeLines(content, path, useBytes = TRUE)

  message("Created book page: ", path)

  invisible(path)
}
