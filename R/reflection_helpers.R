# ==================================================
# MARGINALIA — REFLECTION HELPERS
# English / Français québécois
# ==================================================


# --------------------------------------------------
# 1. Locate a book's shared reflection file
# --------------------------------------------------

reflection_path <- function(id) {

  if (
    length(id) != 1L ||
    is.na(id) ||
    !grepl("^B[0-9]+$", id)
  ) {
    stop("Invalid book ID.")
  }

  file.path(
    "_reflections",
    paste0(id, ".md")
  )
}


# --------------------------------------------------
# 2. Read both language sections
# --------------------------------------------------

read_book_reflections <- function(id) {

  path <- reflection_path(id)

  if (!file.exists(path)) {
    stop(
      "Reflection file not found: ",
      path
    )
  }

  lines <- readLines(
    path,
    encoding = "UTF-8",
    warn = FALSE
  )

  # Identify the language markers
  en_marker <- which(
    trimws(lines) == "<!-- lang: en -->"
  )

  fr_marker <- which(
    trimws(lines) == "<!-- lang: fr -->"
  )

  # Require exactly one marker for each language
  if (
    length(en_marker) != 1L ||
    length(fr_marker) != 1L
  ) {
    stop(
      "Expected exactly one English and one French ",
      "language marker in: ",
      path
    )
  }

  # Require English before French
  if (en_marker >= fr_marker) {
    stop(
      "The English section must precede ",
      "the French section in: ",
      path
    )
  }

  # Safely extract lines, including empty sections
  extract_lines <- function(from, to) {

    if (from > to) {
      return(character())
    }

    lines[seq.int(from, to)]
  }

  # Remove blank lines surrounding each reflection
  trim_blank_lines <- function(x) {

    nonempty <- which(
      nzchar(trimws(x))
    )

    if (length(nonempty) == 0L) {
      return(character())
    }

    x[seq.int(
      min(nonempty),
      max(nonempty)
    )]
  }

  english <- extract_lines(
    en_marker + 1L,
    fr_marker - 1L
  )

  french <- extract_lines(
    fr_marker + 1L,
    length(lines)
  )

  list(
    en = trim_blank_lines(english),
    fr = trim_blank_lines(french)
  )
}


# --------------------------------------------------
# 3. Retrieve the requested language
# --------------------------------------------------

get_book_reflection <- function(
    id,
    lang = "en"
) {

  lang <- match.arg(
    lang,
    c("en", "fr")
  )

  reflections <- read_book_reflections(id)

  paste(
    reflections[[lang]],
    collapse = "\n"
  )
}


# --------------------------------------------------
# 4. Display a book's reflection
# --------------------------------------------------

book_reflections <- function(
    id,
    lang = "en"
) {

  lang <- match.arg(
    lang,
    c("en", "fr")
  )

  reflection <- get_book_reflection(
    id,
    lang
  )

  # Display a message if no reflection is available
  if (!nzchar(trimws(reflection))) {

    reflection <- if (lang == "fr") {
      "*Mes réflexions suivront.*"
    } else {
      "*My reflections are forthcoming.*"
    }

  }

  cat(
    "\n",
    reflection,
    "\n\n",
    sep = ""
  )

  invisible(NULL)
}


# --------------------------------------------------
# 5. Generate a shared reflection file
# --------------------------------------------------

create_reflection_file <- function(
    id,
    en = "",
    fr = "",
    save = FALSE
) {

  path <- reflection_path(id)

  content <- c(
    "<!-- lang: en -->",
    "",
    en,
    "",
    "<!-- lang: fr -->",
    "",
    fr,
    ""
  )

  if (save) {

    # Never overwrite existing reflections
    if (file.exists(path)) {
      stop(
        "Reflection file already exists: ",
        path
      )
    }

    dir.create(
      dirname(path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    writeLines(
      enc2utf8(content),
      path,
      useBytes = TRUE
    )

    message(
      "Created reflection file: ",
      path
    )
  }

  invisible(content)
}
