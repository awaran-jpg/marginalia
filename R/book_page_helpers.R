library(tidyverse)

# ==================================================
# MARGINALIA — BILINGUAL BOOK PAGE HELPERS
# ==================================================


# --------------------------------------------------
# 1. Resolve the French counterpart of a book page
# --------------------------------------------------

french_book_path <- function(path) {

  sub(
    "\\.qmd$",
    ".fr.qmd",
    path
  )
}


# --------------------------------------------------
# 2. Identify temporary French placeholders
# --------------------------------------------------

is_french_placeholder <- function(path) {

  if (!file.exists(path)) {
    return(FALSE)
  }

  lines <- readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  if (
    length(lines) < 3 ||
    trimws(lines[1]) != "---"
  ) {
    return(FALSE)
  }

  yaml_end <- which(trimws(lines) == "---")
  yaml_end <- yaml_end[yaml_end > 1][1]

  if (
    is.na(yaml_end) ||
    yaml_end >= length(lines)
  ) {
    return(FALSE)
  }

  body <- lines[
    seq.int(yaml_end + 1, length(lines))
  ]

  body <- trimws(body)
  body <- body[nzchar(body)]

  expected <- c(
    "Cette page est en cours de préparation.",
    "La version anglaise est accessible à partir du menu des langues."
  )

  identical(body, expected)
}


# --------------------------------------------------
# 3. Construct a functional French book page
# --------------------------------------------------

french_book_content <- function(book) {

  id <- book$book_id[[1]]
  title <- book$title[[1]]
  author <- book$author[[1]]

  c(

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

    'book_header(books, reading_log, book_id, lang = "fr")',

    "```",
    "",


    "## Mes réflexions",
    "",

    "```{r}",
    "#| echo: false",
    "#| results: asis",
    "",

    'source("R/reflection_helpers.R")',
    "",

    'book_reflections(book_id, lang = "fr")',

    "```",
    "",

    "## Historique de lecture {#reading-history}",

    "",

    "```{r}",
    "#| echo: false",

    'book_history_table(books, reading_log, book_id, lang = "fr")',

    "```"
  )
}


# --------------------------------------------------
# 4. Create or replace a French placeholder page
# --------------------------------------------------

create_french_book_page <- function(
    book,
    save = FALSE
) {

  original_path <- book$book_page[[1]]

  if (
    is.na(original_path) ||
    !grepl("\\.qmd$", original_path)
  ) {
    stop("Invalid book-page path.")
  }

  path <- french_book_path(
    original_path
  )

  action <- if (!file.exists(path)) {

    "Create"

  } else if (is_french_placeholder(path)) {

    "Replace placeholder"

  } else {

    "Preserve existing page"

  }

  # Never overwrite an independently edited page
  if (
    save &&
    action != "Preserve existing page"
  ) {

    dir.create(
      dirname(path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    writeLines(
      french_book_content(book),
      path,
      useBytes = TRUE
    )

    message(action, ": ", path)
  }

  tibble(
    book_id = book$book_id[[1]],
    title = book$title[[1]],
    path = path,
    action = action
  )
}


# --------------------------------------------------
# 5. Prepare French pages for the entire catalog
# --------------------------------------------------

create_all_french_book_pages <- function(
    save = FALSE
) {

  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  )

  stopifnot(
    all(!is.na(books$book_page)),
    all(file.exists(books$book_page))
  )

  map_dfr(
    seq_len(nrow(books)),

    function(i) {

      create_french_book_page(
        books[i, ],
        save = save
      )

    }
  )
}
