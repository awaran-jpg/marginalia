library(tidyverse)
library(DT)
library(htmltools)

# ==================================================
# MARGINALIA — PAGE LOG HELPERS
# ==================================================


# --------------------------------------------------
# 1. Prepare shared reading-log data
# --------------------------------------------------

prepare_page_log <- function() {

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

  # Calculate recorded reading progress
  book_progress <- calculate_book_progress(
    books,
    reading_log
  )

  logged_entries <- book_progress |>
    left_join(
      books |>
        select(book_id, book_page),
      by = "book_id"
    ) |>
    transmute(
      date,
      title,
      pages_read,
      page_reached,
      book_page,
      unrecorded = FALSE
    )

  # Identify days without reading records
  if (nrow(book_progress) > 0) {

    empty_days <- tibble(
      date = seq.Date(
        min(book_progress$date),
        max(book_progress$date),
        by = "day"
      )
    ) |>
      anti_join(
        book_progress |> distinct(date),
        by = "date"
      ) |>
      mutate(
        title = NA_character_,
        pages_read = NA_integer_,
        page_reached = NA_integer_,
        book_page = NA_character_,
        unrecorded = TRUE
      )

  } else {

    empty_days <- tibble(
      date = as.Date(character()),
      title = character(),
      pages_read = integer(),
      page_reached = integer(),
      book_page = character(),
      unrecorded = logical()
    )

  }

  # Combine and sort all entries
  bind_rows(
    logged_entries,
    empty_days
  ) |>
    arrange(
      desc(date),
      title
    )
}


# --------------------------------------------------
# 2. Generate clickable book titles
# --------------------------------------------------

page_log_book_cell <- function(
    title,
    page,
    unrecorded,
    lang = "en"
) {

  # Distinguish missing records from zero reading
  if (unrecorded) {

    return(
      as.character(
        tags$span(
          class = "log-unrecorded",
          m_text("no_entry", lang)
        )
      )
    )

  }

  # Retrieve the corresponding book-page link
  page_url <- m_book_link(page)

  if (is.na(page_url)) {

    return(
      as.character(
        htmlEscape(title)
      )
    )

  }

  as.character(
    tags$a(
      href = page_url,
      class = "log-book-link",
      title
    )
  )
}


# --------------------------------------------------
# 3. Prepare localized display data
# --------------------------------------------------

localize_page_log <- function(
    data,
    lang = "en"
) {

  lang <- match.arg(
    lang,
    c("en", "fr")
  )

  data |>
    mutate(

      book_display = pmap_chr(
        list(
          title,
          book_page,
          unrecorded
        ),

        function(title, page, unrecorded) {

          page_log_book_cell(
            title,
            page,
            unrecorded,
            lang
          )

        }
      )

    ) |>
    transmute(
      Date = m_date(
        date,
        lang,
        style = "iso"
      ),
      Book = book_display,
      `Pages Read` = pages_read,
      `Page Reached` = page_reached
    )
}


# --------------------------------------------------
# 4. Render the interactive reading-log table
# --------------------------------------------------

render_page_log <- function(
    data,
    lang = "en"
) {

  lang <- match.arg(
    lang,
    c("en", "fr")
  )

  display_log <- localize_page_log(
    data,
    lang
  )

  column_keys <- c(
    "date",
    "book",
    "pages_read",
    "page_reached"
  )

  DT::datatable(

    display_log,

    colnames = m_text(
      column_keys,
      lang
    ),

    rownames = FALSE,

    # Permit HTML links in the Book column
    escape = c(1, 3, 4),

    class = "hover",

    options = list(

      pageLength = 15,

      lengthChange = TRUE,

      lengthMenu = m_length_menu(lang),

      searching = TRUE,

      ordering = TRUE,

      # Most recent dates first
      order = list(
        list(0, "desc")
      ),

      autoWidth = FALSE,

      scrollX = TRUE,

      language = m_dt_language(
        lang,
        search_key = "search_log",
        empty_key = "empty_log"
      )

    )
  )
}
