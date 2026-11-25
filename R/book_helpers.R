library(tidyverse)

source("R/book_page_helpers.R")
source("R/reflection_helpers.R")
# --------------------------------------------------
# 1. Add a new book to the catalog
# --------------------------------------------------

add_book <- function(
    title,
    author,
    total_pages = NA_integer_,
    status = "Want to Read",
    date_started = as.Date(NA),
    date_finished = as.Date(NA),
    cover_id = NA_integer_,
    baseline_page = NA_integer_,
    save = FALSE,
    cover_file = NA_character_
) {

  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )

  # Support catalogs created before local covers
  if (!"cover_file" %in% names(books)) {
    books$cover_file <- NA_character_
  }

  normalize_text <- function(x) {
    str_to_lower(str_squish(x))
  }

  # Prevent duplicate books
  duplicate <- (
    normalize_text(books$title) == normalize_text(title) &
      normalize_text(books$author) == normalize_text(author)
  )

  if (any(duplicate, na.rm = TRUE)) {

    if (save) {
      stop(
        "This book is already in the catalog. Nothing was saved."
      )
    } else {
      warning(
        "This book is already in the catalog. Preview only."
      )
    }

  }

  # Validate status
  valid_statuses <- c(
    "Want to Read",
    "Reading",
    "Finished",
    "Abandoned"
  )

  if (!status %in% valid_statuses) {
    stop("Invalid reading status.")
  }

  # Validate page count
  if (
    !is.na(total_pages) &&
    (
      total_pages <= 0 ||
      total_pages %% 1 != 0
    )
  ) {
    stop("Total pages must be a positive whole number.")
  }

  # Validate local cover
  if (
    length(cover_file) != 1L ||
    !is.character(cover_file) ||
    (
      !is.na(cover_file) &&
      !file.exists(cover_file)
    )
  ) {
    stop(
      "Cover file must be NA or an existing file."
    )
  }

  # Convert dates
  date_started <- as.Date(date_started)
  date_finished <- as.Date(date_finished)

  if (
    !is.na(date_started) &&
    !is.na(date_finished) &&
    date_finished < date_started
  ) {
    stop(
      "Finishing date cannot precede starting date."
    )
  }

  # Generate next book ID
  existing_ids <- as.integer(
    str_remove(books$book_id, "^B")
  )

  next_number <- if (length(existing_ids) == 0) {
    1L
  } else {
    max(existing_ids) + 1L
  }

  new_id <- sprintf("B%03d", next_number)

  # Construct book record
  new_book <- tibble(
    book_id = new_id,
    title = title,
    author = author,
    total_pages = as.integer(total_pages),
    status = status,
    date_started = date_started,
    date_finished = date_finished,
    book_page = paste0(
      "books/",
      new_id,
      ".qmd"
    ),
    cover_id = as.integer(cover_id),
    baseline_page = as.integer(baseline_page),
    cover_file = cover_file
  )



  # Save only when requested
  if (save) {

    en_path <- new_book$book_page[[1]]
    fr_path <- french_book_path(en_path)
    ref_path <- reflection_path(new_id)

    # Protect all existing files
    new_paths <- c(
      en_path,
      fr_path,
      ref_path
    )

    if (any(file.exists(new_paths))) {
      stop(
        "A book page or reflection file ",
        "with this ID already exists."
      )
    }

    # Create both pages and the shared reflection
    tryCatch(
      {

        create_book_page(new_book)

        create_french_book_page(
          new_book,
          save = TRUE
        )

        create_reflection_file(
          new_id,
          save = TRUE
        )

      },

      error = function(e) {

        created_paths <- new_paths[
          file.exists(new_paths)
        ]

        if (length(created_paths) > 0) {
          unlink(created_paths)
        }

        stop(
          "Could not create book files: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    # Update the shared catalog
    books <- bind_rows(
      books,
      new_book
    )

    write_csv(
      books,
      "data/books.csv"
    )

    message(
      "Book added: ",
      new_id,
      " (bilingual pages and shared reflection created)"
    )

  }

  return(new_book)
}


# --------------------------------------------------
# 2. Search Open Library
# --------------------------------------------------

search_open_library <- function(
    title,
    author = NULL
) {

  url <- paste0(
    "https://openlibrary.org/search.json?title=",
    URLencode(title, reserved = TRUE)
  )

  if (!is.null(author)) {

    url <- paste0(
      url,
      "&author=",
      URLencode(author, reserved = TRUE)
    )

  }

  url <- paste0(
    url,
    "&fields=key,title,author_name,",
    "first_publish_year,cover_i",
    "&limit=10"
  )

  results <- jsonlite::fromJSON(url)

  if (nrow(results$docs) == 0) {

    message("No matching books found.")
    return(tibble())

  }

  results$docs |>
    as_tibble() |>
    select(
      any_of(
        c(
          "title",
          "author_name",
          "first_publish_year",
          "cover_i",
          "key"
        )
      )
    )
}


# --------------------------------------------------
# 3. Add a book from Open Library results
# --------------------------------------------------

add_book_from_search <- function(
    results,
    row,
    ...,
    save = FALSE
) {

  if (
    length(row) != 1 ||
    is.na(row) ||
    row < 1 ||
    row > nrow(results) ||
    row %% 1 != 0
  ) {
    stop("Invalid search result selection.")
  }

  selected <- results[row, ]

  author <- selected$author_name[[1]][1]

  cover_id <- if (
    "cover_i" %in% names(selected)
  ) {
    as.integer(
      selected$cover_i[[1]]
    )
  } else {
    NA_integer_
  }

  add_book(
    title = selected$title[[1]],
    author = author,
    cover_id = cover_id,
    ...,
    save = save
  )
}


# --------------------------------------------------
# 4. Generate an individual book page
# --------------------------------------------------

create_book_page <- function(book) {

  id <- book$book_id[[1]]
  title <- book$title[[1]]
  author <- book$author[[1]]
  path <- book$book_page[[1]]

  # Prevent overwriting existing reflections
  if (file.exists(path)) {
    stop(
      "A page already exists at: ",
      path
    )
  }

  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  content <- c(
    "---",
    paste0(
      "title: ",
      jsonlite::toJSON(
        title,
        auto_unbox = TRUE
      )
    ),
    paste0(
      "subtitle: ",
      jsonlite::toJSON(
        author,
        auto_unbox = TRUE
      )
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
    paste0(
      'book_id <- "',
      id,
      '"'
    ),
    "```",
    "",
    "```{r}",
    "#| echo: false",
    "book_header(books, reading_log, book_id)",
    "```",
    "",

    "## My Reflections",
    "",

    "```{r}",
    "#| echo: false",
    "#| results: asis",
    "",

    'source("R/reflection_helpers.R")',
    "",

    'book_reflections(book_id, lang = "en")',

    "```",
    "",

    "## Reading History",

    "",
    "```{r}",
    "#| echo: false",
    "book_history_table(books, reading_log, book_id)",
    "```"
  )

  writeLines(
    content,
    path,
    useBytes = TRUE
  )

  message(
    "Created book page: ",
    path
  )

  invisible(path)
}


# --------------------------------------------------
# 5. Edit an existing book
# --------------------------------------------------

edit_book <- function(
    id,
    title = NULL,
    author = NULL,
    total_pages = NULL,
    status = NULL,
    date_started = NULL,
    date_finished = NULL,
    cover_id = NULL,
    baseline_page = NULL,
    save = FALSE,
    cover_file = NULL
) {

  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )

  # Support older catalogs
  if (!"cover_file" %in% names(books)) {
    books$cover_file <- NA_character_
  }


  # Identify book by ID or title
  if (
    length(id) != 1L ||
    !is.character(id) ||
    is.na(id) ||
    !nzchar(str_squish(id))
  ) {
    stop("Please provide a valid book ID or title.")
  }

  normalize_lookup <- function(x) {
    str_to_lower(str_squish(x))
  }

  # First attempt to match the book ID
  index <- which(
    normalize_lookup(books$book_id) ==
      normalize_lookup(id)
  )

  # If no ID matches, search by title
  if (length(index) == 0L) {

    index <- which(
      normalize_lookup(books$title) ==
        normalize_lookup(id)
    )

  }

  # Handle missing or ambiguous matches
  if (length(index) == 0L) {
    stop("No book found matching: ", id)
  }

  if (length(index) > 1L) {
    stop(
      "Multiple books match this title. ",
      "Please use the unique book ID."
    )
  }

  # Retain the canonical ID for internal operations
  id <- books$book_id[[index]]

  book <- books[index, ]


  # Validate numerical fields
  check_number <- function(
    x,
    field,
    minimum
  ) {

    if (
      length(x) != 1 ||
      (
        !is.na(x) &&
        (
          !is.numeric(x) ||
          !is.finite(x) ||
          x < minimum ||
          x %% 1 != 0
        )
      )
    ) {
      stop(
        field,
        " must be a valid whole number."
      )
    }

    as.integer(x)
  }

  # Validate dates
  check_date <- function(
    x,
    field
  ) {

    if (length(x) != 1) {
      stop(
        field,
        " must contain one date."
      )
    }

    tryCatch(
      as.Date(x),
      error = function(e) {
        stop(
          "Invalid ",
          field,
          "."
        )
      }
    )
  }

  # --------------------------------------------------
  # Apply requested changes
  # --------------------------------------------------

  if (!is.null(title)) {

    if (
      length(title) != 1 ||
      !is.character(title) ||
      is.na(title) ||
      !nzchar(str_squish(title))
    ) {
      stop(
        "Title must be a nonempty string."
      )
    }

    book$title <- str_squish(title)
  }

  if (!is.null(author)) {

    if (
      length(author) != 1 ||
      !is.character(author) ||
      is.na(author) ||
      !nzchar(str_squish(author))
    ) {
      stop(
        "Author must be a nonempty string."
      )
    }

    book$author <- str_squish(author)
  }

  if (!is.null(total_pages)) {

    book$total_pages <- check_number(
      total_pages,
      "Total pages",
      1
    )

  }

  if (!is.null(status)) {

    valid_statuses <- c(
      "Want to Read",
      "Reading",
      "Finished",
      "Abandoned"
    )

    if (
      length(status) != 1 ||
      is.na(status) ||
      !status %in% valid_statuses
    ) {
      stop("Invalid reading status.")
    }

    book$status <- status
  }

  if (!is.null(date_started)) {

    book$date_started <- check_date(
      date_started,
      "starting date"
    )

  }

  if (!is.null(date_finished)) {

    book$date_finished <- check_date(
      date_finished,
      "finishing date"
    )

  }

  if (!is.null(cover_id)) {

    book$cover_id <- check_number(
      cover_id,
      "Cover ID",
      1
    )

  }

  if (!is.null(baseline_page)) {

    book$baseline_page <- check_number(
      baseline_page,
      "Baseline page",
      0
    )

  }

  # Local cover: NA removes the local override
  if (!is.null(cover_file)) {

    if (
      length(cover_file) != 1L ||
      !is.character(cover_file) ||
      (
        !is.na(cover_file) &&
        !file.exists(cover_file)
      )
    ) {
      stop(
        "Cover file must be NA or an existing file."
      )
    }

    book$cover_file <- cover_file
  }

  # --------------------------------------------------
  # Validate the updated record
  # --------------------------------------------------

  normalize <- function(x) {
    str_to_lower(str_squish(x))
  }

  other_books <- books[-index, ]

  duplicate <- (
    normalize(other_books$title) ==
      normalize(book$title[[1]]) &
      normalize(other_books$author) ==
      normalize(book$author[[1]])
  )

  if (any(duplicate, na.rm = TRUE)) {
    stop(
      "Another book already has this title and author."
    )
  }

  # Check date order
  if (
    !is.na(book$date_started[[1]]) &&
    !is.na(book$date_finished[[1]]) &&
    book$date_finished[[1]] <
    book$date_started[[1]]
  ) {
    stop(
      "Finishing date cannot precede starting date."
    )
  }

  # Check completion status
  if (
    book$status[[1]] == "Finished" &&
    is.na(book$date_finished[[1]])
  ) {
    stop(
      "Finished books must have a finishing date."
    )
  }

  if (
    book$status[[1]] != "Finished" &&
    !is.na(book$date_finished[[1]])
  ) {
    stop(
      "Only finished books can have a finishing date."
    )
  }

  # Check baseline against total pages
  if (
    !is.na(book$baseline_page[[1]]) &&
    !is.na(book$total_pages[[1]]) &&
    book$baseline_page[[1]] >
    book$total_pages[[1]]
  ) {
    stop(
      "Baseline page exceeds total pages."
    )
  }

  # --------------------------------------------------
  # Protect existing reading history
  # --------------------------------------------------

  reading_log <- read_csv(
    "data/reading_log.csv",
    col_types = cols(
      book_id = col_character(),
      date = col_date(),
      page_reached = col_integer()
    )
  )

  history <- reading_log |>
    filter(book_id == id)

  if (nrow(history) > 0) {

    if (
      !is.na(book$total_pages[[1]]) &&
      any(
        history$page_reached >
        book$total_pages[[1]],
        na.rm = TRUE
      )
    ) {
      stop(
        "Total pages cannot be below recorded progress."
      )
    }

    if (
      !is.na(book$baseline_page[[1]]) &&
      any(
        history$page_reached <
        book$baseline_page[[1]],
        na.rm = TRUE
      )
    ) {
      stop(
        "Baseline page cannot exceed recorded progress."
      )
    }

    if (
      !is.na(book$date_started[[1]]) &&
      any(
        history$date <
        book$date_started[[1]],
        na.rm = TRUE
      )
    ) {
      stop(
        "Starting date conflicts with reading history."
      )
    }

    if (
      !is.na(book$date_finished[[1]]) &&
      any(
        history$date >
        book$date_finished[[1]],
        na.rm = TRUE
      )
    ) {
      stop(
        "Finishing date conflicts with reading history."
      )
    }

  }

  # --------------------------------------------------
  # Save changes
  # --------------------------------------------------

  if (save) {

    metadata_changed <- (
      book$title[[1]] !=
        books$title[[index]] ||
        book$author[[1]] !=
        books$author[[index]]
    )

    if (metadata_changed) {
      update_book_page_metadata(book)
    }

    books[index, ] <- book

    write_csv(
      books,
      "data/books.csv"
    )

    message(
      "Book updated: ",
      id
    )

  } else {

    message(
      "Preview only. No changes saved."
    )

  }

  return(book)
}



# --------------------------------------------------
# 6. Update bilingual book page metadata
# --------------------------------------------------

update_book_page_metadata <- function(book) {

  en_path <- book$book_page[[1]]
  fr_path <- french_book_path(en_path)

  paths <- c(
    en = en_path,
    fr = fr_path
  )

  # Verify that both pages exist
  if (
    anyNA(paths) ||
    !all(file.exists(paths))
  ) {
    stop(
      "One or both book pages could not be found."
    )
  }

  # Prepare updated metadata
  prepare_metadata <- function(path) {

    lines <- readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    )

    if (
      length(lines) < 3 ||
      trimws(lines[1]) != "---"
    ) {
      stop(
        "Invalid YAML front matter in: ",
        path
      )
    }

    end_yaml <- which(
      trimws(lines) == "---"
    )

    end_yaml <- end_yaml[
      end_yaml > 1
    ][1]

    if (
      is.na(end_yaml) ||
      end_yaml <= 2L
    ) {
      stop(
        "Could not identify YAML metadata in: ",
        path
      )
    }

    yaml_lines <- seq.int(
      2,
      end_yaml - 1
    )

    title_line <- yaml_lines[
      grepl(
        "^title\\s*:",
        lines[yaml_lines]
      )
    ]

    subtitle_line <- yaml_lines[
      grepl(
        "^subtitle\\s*:",
        lines[yaml_lines]
      )
    ]

    if (
      length(title_line) != 1 ||
      length(subtitle_line) != 1
    ) {
      stop(
        "Expected one title and one subtitle in: ",
        path
      )
    }

    # Replace only title and subtitle
    lines[title_line] <- paste0(
      "title: ",
      jsonlite::toJSON(
        book$title[[1]],
        auto_unbox = TRUE
      )
    )

    lines[subtitle_line] <- paste0(
      "subtitle: ",
      jsonlite::toJSON(
        book$author[[1]],
        auto_unbox = TRUE
      )
    )

    lines
  }

  # Validate both pages before writing
  updated_pages <- lapply(
    paths,
    prepare_metadata
  )

  # Save updated pages
  for (i in seq_along(paths)) {

    writeLines(
      enc2utf8(updated_pages[[i]]),
      paths[[i]],
      useBytes = TRUE
    )

    message(
      "Updated book page metadata: ",
      paths[[i]]
    )

  }

  invisible(paths)
}
