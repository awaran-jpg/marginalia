
library(tidyverse)
library(jsonlite)

# Search Open Library
url <- paste0(
  "https://openlibrary.org/search.json?",
  "title=", URLencode("L'avalée des avalés", reserved = TRUE),
  "&author=", URLencode("Réjean Ducharme", reserved = TRUE),
  "&fields=title,cover_i,first_publish_year",
  "&limit=20"
)

results <- fromJSON(url)

# Display possible covers
results$docs |>
  as_tibble() |>
  filter(!is.na(cover_i)) |>
  mutate(
    cover_url = paste0(
      "https://covers.openlibrary.org/b/id/",
      cover_i,
      "-M.jpg"
    )
  ) |>
  select(title, cover_i, first_publish_year, cover_url)
