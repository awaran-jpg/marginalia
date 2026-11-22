
library(tidyverse)

# Create an empty book catalog
books <- tibble(
  book_id = character(),
  title = character(),
  author = character(),
  total_pages = integer(),
  status = character(),
  date_started = as.Date(character()),
  date_finished = as.Date(character()),
  rating = double(),
  thoughts = character()
)

# Create the CSV only if it does not already exist
if (!file.exists("data/books.csv")) {
  write_csv(books, "data/books.csv")
}


new_book <- tibble(
  book_id = "B001",
  title = "L'avalée des avalés",
  author = "Réjean Ducharme",
  total_pages = 343L,
  status = "Reading",
  date_started = as.Date("2026-11-19"),
  date_finished = as.Date(NA),
  rating = NA_real_,
  thoughts = ""
)


if (!new_book$book_id %in% books$book_id) {
  books <- bind_rows(books, new_book)
  write_csv(books, "data/books.csv")
}

glimpse(books)


# Create an empty reading log
reading_log <- tibble(
  book_id = character(),
  date = as.Date(character()),
  page_reached = integer()
)

# Save only if the file does not already exist
if (!file.exists("data/reading_log.csv")) {
  write_csv(reading_log, "data/reading_log.csv")
}

