# ==================================================
# MARGINALIA — LOCALIZATION HELPERS
# English / Français québécois
# ==================================================


# --------------------------------------------------
# 1. Shared interface vocabulary
# --------------------------------------------------

marginalia_labels <- list(

  en = c(
    book = "Book",
    author = "Author",
    pages = "Pages",
    status = "Status",
    date = "Date",
    date_started = "Date Started",
    date_finished = "Date Finished",

    reading = "Reading",
    finished = "Finished",
    abandoned = "Abandoned",
    wishlist = "Want to Read",

    pages_read = "Pages Read",
    page_reached = "Page Reached",
    pages_logged = "Pages logged",
    books_completed = "Books completed",

    reading_details = "Reading Details",
    reading_history = "Reading History",
    reflections = "My Reflections",
    last_page = "Last recorded page",
    progress = "Reading progress",
    length = "Length",

    no_entry = "No entry recorded",
    not_recorded = "Not recorded",
    no_cover = "No cover",
    cover_unavailable = "Cover unavailable",
    cover_of = "Cover of",

    search_books = "Search books:",
    search_wishlist = "Search wishlist:",
    search_log = "Search log:",

    empty_books = "No books recorded yet.",
    empty_wishlist = "No books on the wishlist yet.",
    empty_log = "No reading entries yet.",
    empty_chart = "No records are available for this chart.",

    all = "All"
  ),

  fr = c(
    book = "Livre",
    author = "Auteur ou autrice",
    pages = "Pages",
    status = "Statut",
    date = "Date",
    date_started = "Date de début",
    date_finished = "Date de fin",

    reading = "En cours",
    finished = "Terminé",
    abandoned = "Abandonné",
    wishlist = "À lire",

    pages_read = "Pages lues",
    page_reached = "Page atteinte",
    pages_logged = "Pages consignées",
    books_completed = "Livres terminés",

    reading_details = "Renseignements sur la lecture",
    reading_history = "Historique de lecture",
    reflections = "Mes réflexions",
    last_page = "Dernière page consignée",
    progress = "Progression de la lecture",
    length = "Nombre de pages",

    no_entry = "Aucune lecture consignée",
    not_recorded = "Non consigné",
    no_cover = "Sans couverture",
    cover_unavailable = "Couverture non disponible",
    cover_of = "Couverture de",

    search_books = "Rechercher un livre\u00a0:",
    search_wishlist = "Rechercher dans la liste\u00a0:",
    search_log = "Rechercher dans le carnet\u00a0:",

    empty_books = "Aucun livre pour le moment.",
    empty_wishlist = "Aucun livre dans la liste pour le moment.",
    empty_log = "Aucune lecture consignée pour le moment.",
    empty_chart = "Aucune donnée disponible pour ce graphique.",

    all = "Tout"
  )

)


# --------------------------------------------------
# 2. Retrieve translated text
# --------------------------------------------------

m_text <- function(key, lang = "en") {

  lang <- match.arg(lang, c("en", "fr"))

  result <- marginalia_labels[[lang]][key]

  if (anyNA(result)) {
    stop(
      "Unknown localization key: ",
      paste(key[is.na(result)], collapse = ", ")
    )
  }

  unname(result)
}


# --------------------------------------------------
# 3. Translate reading statuses for display
# --------------------------------------------------

m_status <- function(status, lang = "en") {

  status_keys <- c(
    "Reading" = "reading",
    "Finished" = "finished",
    "Abandoned" = "abandoned",
    "Want to Read" = "wishlist"
  )

  keys <- unname(status_keys[status])

  result <- as.character(status)

  valid <- !is.na(keys)

  result[valid] <- m_text(keys[valid], lang)

  result
}


# --------------------------------------------------
# 4. Format numbers
# --------------------------------------------------

m_number <- function(x, lang = "en") {

  lang <- match.arg(lang, c("en", "fr"))

  vapply(
    x,
    function(value) {

      if (is.na(value)) {
        return("—")
      }

      format(
        value,
        big.mark = if (lang == "fr") "\u00a0" else ",",
        decimal.mark = if (lang == "fr") "," else ".",
        scientific = FALSE,
        trim = TRUE
      )

    },
    character(1),
    USE.NAMES = FALSE
  )
}


# --------------------------------------------------
# 5. Format percentages
# --------------------------------------------------

m_percent <- function(x, lang = "en") {

  lang <- match.arg(lang, c("en", "fr"))

  suffix <- if (lang == "fr") {
    "\u00a0%"
  } else {
    "%"
  }

  ifelse(
    is.na(x),
    "—",
    paste0(m_number(x, lang), suffix)
  )
}


# --------------------------------------------------
# 6. Format dates and month names
# --------------------------------------------------

m_month_names <- function(
    lang = "en",
    abbreviated = FALSE
) {

  lang <- match.arg(lang, c("en", "fr"))

  if (lang == "en") {

    if (abbreviated) {
      return(month.abb)
    }

    return(month.name)
  }

  if (abbreviated) {

    return(c(
      "janv.", "févr.", "mars", "avr.",
      "mai", "juin", "juill.", "août",
      "sept.", "oct.", "nov.", "déc."
    ))

  }

  c(
    "janvier", "février", "mars",
    "avril", "mai", "juin",
    "juillet", "août", "septembre",
    "octobre", "novembre", "décembre"
  )
}


m_date <- function(
    x,
    lang = "en",
    style = c("long", "iso", "short", "month")
) {

  lang <- match.arg(lang, c("en", "fr"))
  style <- match.arg(style)

  x <- as.Date(x)

  vapply(
    seq_along(x),

    function(i) {

      value <- x[i]

      if (is.na(value)) {
        return("—")
      }

      if (style == "iso") {
        return(format(value, "%Y-%m-%d"))
      }

      day <- as.integer(format(value, "%d"))
      month <- as.integer(format(value, "%m"))
      year <- format(value, "%Y")

      month_text <- m_month_names(
        lang,
        abbreviated = style != "long"
      )[month]

      if (style == "month") {
        return(paste(month_text, year))
      }

      if (lang == "fr") {

        day_text <- if (
          day == 1L && style == "long"
        ) {
          "1er"
        } else {
          as.character(day)
        }

        if (style == "short") {
          return(paste(day_text, month_text))
        }

        return(
          paste(day_text, month_text, year)
        )

      }

      if (style == "short") {
        return(paste(month_text, day))
      }

      paste0(month_text, " ", day, ", ", year)

    },

    character(1),
    USE.NAMES = FALSE
  )
}


# --------------------------------------------------
# 7. Resolve local and Open Library covers
# --------------------------------------------------

m_cover_url <- function(
    cover_id = NA_integer_,
    cover_file = NA_character_,
    prefix = "",
    size = c("M", "L", "S")
) {

  size <- match.arg(size)

  # Prefer a valid local cover
  if (
    length(cover_file) == 1L &&
    !is.na(cover_file) &&
    nzchar(cover_file) &&
    file.exists(cover_file)
  ) {

    return(
      paste0(prefix, cover_file)
    )

  }

  # Otherwise use Open Library
  if (
    length(cover_id) == 1L &&
    !is.na(cover_id) &&
    cover_id > 0
  ) {

    return(
      paste0(
        "https://covers.openlibrary.org/b/id/",
        cover_id,
        "-",
        size,
        ".jpg"
      )
    )

  }

  NA_character_
}


# --------------------------------------------------
# 8. Resolve book-page links
# --------------------------------------------------

m_book_link <- function(page) {

  if (
    length(page) != 1L ||
    is.na(page) ||
    !nzchar(page)
  ) {

    return(NA_character_)

  }

  sub("\\.qmd$", ".html", page)
}


# --------------------------------------------------
# 9. Localize DataTables controls
# --------------------------------------------------

m_dt_language <- function(
    lang = "en",
    search_key = "search_books",
    empty_key = "empty_books"
) {

  lang <- match.arg(lang, c("en", "fr"))

  if (lang == "en") {

    return(list(
      search = m_text(search_key, lang),
      emptyTable = m_text(empty_key, lang)
    ))

  }

  list(
    decimal = ",",
    thousands = "\u00a0",

    search = m_text(search_key, lang),

    emptyTable = m_text(empty_key, lang),

    zeroRecords = "Aucun résultat correspondant.",

    lengthMenu = "Afficher _MENU_ entrées",

    info = "_START_ à _END_ sur _TOTAL_ entrées",

    infoEmpty = "Aucune entrée à afficher",

    infoFiltered = "(sur _MAX_ entrées au total)",

    loadingRecords = "Chargement...",

    processing = "Traitement en cours...",

    paginate = list(
      first = "Première",
      last = "Dernière",
      "next" = "Suivante",
      previous = "Précédente"
    ),

    aria = list(
      sortAscending = ": trier par ordre croissant",
      sortDescending = ": trier par ordre décroissant"
    )
  )
}


# --------------------------------------------------
# 10. Table-length selector
# --------------------------------------------------

m_length_menu <- function(lang = "en") {

  list(
    c(10, 15, 25, 50, 100, -1),
    c(
      "10", "15", "25", "50", "100",
      m_text("all", lang)
    )
  )
}
