library(tidyverse)
library(DT)
library(htmltools)


# ==================================================
# MARGINALIA — LIBRARY HELPERS
# ==================================================


# --------------------------------------------------
# 1. Generate book cells with cover thumbnails
# --------------------------------------------------

library_book_cell <- function(
    title,
    page,
    cover_id,
    cover_file,
    lang = "en"
) {

  cover_url <- m_cover_url(
    cover_id = cover_id,
    cover_file = cover_file,
    size = "M"
  )

  cover <- if (!is.na(cover_url)) {

    tags$img(
      src = cover_url,
      alt = paste(
        m_text("cover_of", lang),
        title
      ),
      class = "library-cover",
      loading = "lazy"
    )

  } else {

    tags$span(
      class = "library-cover-fallback",
      m_text("no_cover", lang)
    )

  }

  title_element <- tags$span(
    class = "library-book-title",
    title
  )

  page_url <- m_book_link(page)

  cell <- if (!is.na(page_url)) {

    tags$a(
      href = page_url,
      class = "library-book-link",
      cover,
      title_element
    )

  } else {

    tags$div(
      class = "library-book-link",
      cover,
      title_element
    )

  }

  as.character(cell)
}


# --------------------------------------------------
# 2. Generate reading-status labels
# --------------------------------------------------

library_status <- function(
    status,
    lang = "en"
) {

  # CSS classes use the original English status
  status_class <- str_to_lower(status) |>
    str_replace_all("[^a-z]+", "-")

  as.character(
    tags$span(
      class = paste(
        "library-status",
        paste0(
          "library-status--",
          status_class
        )
      ),
      m_status(status, lang)
    )
  )
}


# --------------------------------------------------
# 3. Prepare both Library tables
# --------------------------------------------------

prepare_library <- function(lang = "en") {

  lang <- match.arg(lang, c("en", "fr"))

  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  ) |>
    mutate(
      date_started = as.Date(date_started),
      date_finished = as.Date(date_finished)
    )

  # Compatibility with older catalogs
  if (!"cover_file" %in% names(books)) {
    books$cover_file <- NA_character_
  }

  # Reading Library: active books first
  reading_books <- books |>
    filter(status != "Want to Read") |>
    mutate(

      status_order = case_when(
        status == "Reading" ~ 1L,
        status == "Finished" ~ 2L,
        TRUE ~ 3L
      ),

      sorting_date = if_else(
        status == "Finished",
        date_finished,
        date_started
      )

    ) |>
    arrange(
      status_order,
      desc(sorting_date),
      title
    )

  # Wishlist: alphabetical order
  wishlist_books <- books |>
    filter(status == "Want to Read") |>
    arrange(title)

  # Prepare the main Reading Library
  reading_table <- reading_books |>
    mutate(

      book_display = pmap_chr(
        list(
          title,
          book_page,
          cover_id,
          cover_file
        ),

        function(title, page, cover_id, cover_file) {

          library_book_cell(
            title,
            page,
            cover_id,
            cover_file,
            lang
          )

        }
      ),

      status_display = map_chr(
        status,
        function(x) library_status(x, lang)
      )

    ) |>
    transmute(
      Book = book_display,
      Author = author,
      Pages = total_pages,
      Status = status_display,
      `Date Started` = m_date(
        date_started,
        lang,
        style = "iso"
      ),
      `Date Finished` = m_date(
        date_finished,
        lang,
        style = "iso"
      )
    )

  # Prepare the wishlist without book-page links
  wishlist_table <- wishlist_books |>
    mutate(

      book_display = pmap_chr(
        list(
          title,
          rep(NA_character_, n()),
          cover_id,
          cover_file
        ),

        function(title, page, cover_id, cover_file) {

          library_book_cell(
            title,
            page,
            cover_id,
            cover_file,
            lang
          )

        }
      )

    ) |>
    transmute(
      Book = book_display,
      Author = author
    )

  list(
    reading = reading_table,
    wishlist = wishlist_table
  )
}


# --------------------------------------------------
# 4. Render either Library table
# --------------------------------------------------

render_library_table <- function(
    data,
    lang = "en",
    type = c("reading", "wishlist")
) {

  lang <- match.arg(lang, c("en", "fr"))

  type <- match.arg(type)

  if (type == "reading") {

    column_keys <- c(
      "book",
      "author",
      "pages",
      "status",
      "date_started",
      "date_finished"
    )

    escape_columns <- c(2, 3, 5, 6)

    search_key <- "search_books"
    empty_key <- "empty_books"

  } else {

    column_keys <- c(
      "book",
      "author"
    )

    escape_columns <- 2

    search_key <- "search_wishlist"
    empty_key <- "empty_wishlist"

  }

  DT::datatable(

    data,

    colnames = m_text(column_keys, lang),

    rownames = FALSE,

    escape = escape_columns,

    class = "hover",

    options = list(

      pageLength = 10,

      lengthChange = TRUE,

      lengthMenu = m_length_menu(lang),

      order = list(),

      searching = TRUE,

      ordering = TRUE,

      autoWidth = FALSE,

      scrollX = TRUE,

      language = m_dt_language(
        lang,
        search_key,
        empty_key
      )

    )
  )
}
