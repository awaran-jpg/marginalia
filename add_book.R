
library(tidyverse)

# Import our existing catalog
books <- read_csv("data/books.csv", show_col_types = FALSE)

# Remove columns that we no longer need
books <- books |>
  select(-any_of(c("rating", "thoughts")))

# Add a column for each book's individual page
if (!"book_page" %in% names(books)) {
  books$book_page <- NA_character_
}

# Create the new book entry
completed_book <- tibble(
  book_id = "B002",
  title = "Volkswagen Blues",
  author = "Jacques Poulin",
  total_pages = NA_integer_,
  status = "Finished",
  date_started = as.Date(NA),
  date_finished = as.Date(NA),
  book_page = "books/volkswagen-blues.qmd"
)

# Add the book only if it is not already present
if (!completed_book$book_id %in% books$book_id) {
  books <- bind_rows(books, completed_book)
}

# Ensure that the book is linked to its page
books <- books |>
  mutate(
    book_page = if_else(
      book_id == "B002",
      "books/volkswagen-blues.qmd",
      book_page
    )
  )

# Save our updated catalog
write_csv(books, "data/books.csv")

# Inspect the result
glimpse(books)
