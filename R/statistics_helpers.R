library(tidyverse)

# ==================================================
# MARGINALIA — STATISTICS HELPERS
# ==================================================


# --------------------------------------------------
# 1. Prepare shared reading statistics
# --------------------------------------------------

prepare_statistics <- function() {

  books <- read_csv(
    "data/books.csv",
    show_col_types = FALSE
  ) |>
    mutate(
      date_started = as.Date(date_started),
      date_finished = as.Date(date_finished)
    )

  reading_log <- read_csv(
    "data/reading_log.csv",
    col_types = cols(
      book_id = col_character(),
      date = col_date(),
      page_reached = col_integer()
    )
  )

  book_progress <- calculate_book_progress(
    books,
    reading_log
  )

  # ------------------------------------------------
  # Daily reading totals
  # ------------------------------------------------

  if (nrow(book_progress) > 0) {

    daily_totals <- book_progress |>
      group_by(date) |>
      summarise(
        pages_read = if (anyNA(pages_read)) {
          NA_integer_
        } else {
          sum(pages_read)
        },
        .groups = "drop"
      )

    daily_reading <- daily_totals |>
      complete(
        date = seq.Date(
          min(daily_totals$date),
          max(daily_totals$date),
          by = "day"
        ),
        fill = list(pages_read = 0L),
        explicit = FALSE
      ) |>
      arrange(date)

  } else {

    daily_reading <- tibble(
      date = as.Date(character()),
      pages_read = integer()
    )

  }

  # ------------------------------------------------
  # Summary statistics
  # ------------------------------------------------

  books_reading <- sum(
    books$status == "Reading",
    na.rm = TRUE
  )

  books_finished <- sum(
    books$status == "Finished",
    na.rm = TRUE
  )

  completed_pages <- books |>
    filter(
      status == "Finished",
      !is.na(total_pages)
    ) |>
    summarise(
      total = sum(total_pages),
      .groups = "drop"
    ) |>
    pull(total)

  pages_logged <- sum(
    daily_reading$pages_read,
    na.rm = TRUE
  )

  reading_days <- sum(
    daily_reading$pages_read > 0,
    na.rm = TRUE
  )

  calendar_days <- nrow(daily_reading)

  average_daily <- if (calendar_days > 0) {
    round(pages_logged / calendar_days, 1)
  } else {
    NA_real_
  }

  average_reading_day <- if (reading_days > 0) {
    round(pages_logged / reading_days, 1)
  } else {
    NA_real_
  }

  # ------------------------------------------------
  # Recent daily reading activity
  # ------------------------------------------------

  recent_reading <- if (nrow(daily_reading) > 0) {

    daily_reading |>
      filter(date >= max(date) - 29)

  } else {

    daily_reading

  }

  date_breaks <- if (nrow(recent_reading) == 0) {

    as.Date(character())

  } else if (nrow(recent_reading) <= 7) {

    recent_reading$date

  } else {

    seq.Date(
      from = min(recent_reading$date),
      to = max(recent_reading$date),
      by = "1 week"
    )

  }

  # ------------------------------------------------
  # Monthly reading activity
  # ------------------------------------------------

  recorded_dates <- c(
    reading_log$date,
    books$date_finished
  )

  recorded_dates <- recorded_dates[
    !is.na(recorded_dates)
  ]

  month_sequence <- if (length(recorded_dates) > 0) {

    seq.Date(
      from = lubridate::floor_date(
        min(recorded_dates),
        "month"
      ),
      to = lubridate::floor_date(
        max(recorded_dates),
        "month"
      ),
      by = "month"
    ) |>
      tail(12)

  } else {

    as.Date(character())

  }

  month_breaks <- if (length(month_sequence) > 0) {

    unique(c(
      month_sequence[
        seq(
          1,
          length(month_sequence),
          by = if (
            length(month_sequence) > 6
          ) 2 else 1
        )
      ],
      tail(month_sequence, 1)
    ))

  } else {

    as.Date(character())

  }

  # Monthly pages recorded
  monthly_reading <- tibble(
    month = month_sequence
  ) |>
    left_join(
      daily_reading |>
        mutate(
          month = lubridate::floor_date(
            date,
            "month"
          )
        ) |>
        group_by(month) |>
        summarise(
          pages_read = sum(
            pages_read,
            na.rm = TRUE
          ),
          .groups = "drop"
        ),
      by = "month"
    ) |>
    mutate(
      pages_read = replace_na(
        pages_read,
        0L
      )
    )

  # Books completed each month
  completed_by_month <- tibble(
    month = month_sequence
  ) |>
    left_join(
      books |>
        filter(
          status == "Finished",
          !is.na(date_finished)
        ) |>
        mutate(
          month = lubridate::floor_date(
            date_finished,
            "month"
          )
        ) |>
        count(
          month,
          name = "books_finished"
        ),
      by = "month"
    ) |>
    mutate(
      books_finished = replace_na(
        books_finished,
        0L
      )
    )

  # ------------------------------------------------
  # Return all shared statistics
  # ------------------------------------------------

  list(

    books_reading = books_reading,
    books_finished = books_finished,

    completed_pages = completed_pages,
    pages_logged = pages_logged,

    reading_days = reading_days,
    calendar_days = calendar_days,

    average_daily = average_daily,
    average_reading_day = average_reading_day,

    daily_reading = daily_reading,
    recent_reading = recent_reading,

    monthly_reading = monthly_reading,
    completed_by_month = completed_by_month,

    date_breaks = date_breaks,
    month_breaks = month_breaks

  )
}


# --------------------------------------------------
# 2. Shared chart color palettes
# --------------------------------------------------

stats_palette <- function(dark = FALSE) {

  if (dark) {

    list(
      background = "#181d1b",
      text = "#f0efe9",
      muted = "#b2bbb3",
      grid = "#39443c",
      accent = "#a8c1aa"
    )

  } else {

    list(
      background = "#f8f6f2",
      text = "#2d342f",
      muted = "#657069",
      grid = "#e5dfd6",
      accent = "#4f6b59"
    )

  }
}


# --------------------------------------------------
# 3. Shared chart theme
# --------------------------------------------------

stats_theme <- function(dark = FALSE) {

  palette <- stats_palette(dark)

  theme_minimal(
    base_size = 12,
    base_family = "sans"
  ) +
    theme(

      plot.background = element_rect(
        fill = palette$background,
        color = NA
      ),

      panel.background = element_rect(
        fill = palette$background,
        color = NA
      ),

      panel.grid.major.x = element_blank(),

      panel.grid.major.y = element_line(
        color = palette$grid,
        linewidth = 0.4
      ),

      panel.grid.minor = element_blank(),

      axis.text = element_text(
        color = palette$muted,
        size = 10
      ),

      axis.title.y = element_text(
        color = palette$text,
        size = 11,
        margin = margin(r = 12)
      ),

      axis.title.x = element_blank(),

      plot.margin = margin(
        t = 12,
        r = 18,
        b = 12,
        l = 8
      )

    )
}


# --------------------------------------------------
# 4. Recent daily reading chart
# --------------------------------------------------

plot_daily_reading <- function(
    data,
    lang = "en",
    dark = FALSE
) {

  if (nrow(data$recent_reading) == 0) {
    return(NULL)
  }

  palette <- stats_palette(dark)

  ggplot(
    data$recent_reading,
    aes(x = date, y = pages_read)
  ) +

    geom_col(
      fill = palette$accent,
      width = 0.72
    ) +

    scale_x_date(
      breaks = data$date_breaks,
      labels = function(x) {
        m_date(x, lang, style = "short")
      }
    ) +

    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 4),
      labels = function(x) {
        m_number(x, lang)
      },
      expand = expansion(
        mult = c(0, 0.13)
      )
    ) +

    labs(
      x = NULL,
      y = if (lang == "fr") {
        "Pages lues"
      } else {
        "Pages read"
      }
    ) +

    stats_theme(dark)
}


# --------------------------------------------------
# 5. Monthly reading chart
# --------------------------------------------------

plot_monthly_reading <- function(
    data,
    lang = "en",
    dark = FALSE
) {

  if (nrow(data$monthly_reading) == 0) {
    return(NULL)
  }

  palette <- stats_palette(dark)

  ggplot(
    data$monthly_reading,
    aes(x = month, y = pages_read)
  ) +

    geom_col(
      fill = palette$accent,
      width = 20
    ) +

    scale_x_date(
      breaks = data$month_breaks,
      labels = function(x) {
        m_date(x, lang, style = "month")
      },
      expand = expansion(
        add = c(17, 17)
      )
    ) +

    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 4),
      labels = function(x) {
        m_number(x, lang)
      },
      expand = expansion(
        mult = c(0, 0.13)
      )
    ) +

    labs(
      x = NULL,
      y = m_text("pages_logged", lang)
    ) +

    stats_theme(dark) +

    theme(
      axis.text.x = element_text(
        angle = 25,
        hjust = 1
      )
    )
}


# --------------------------------------------------
# 6. Monthly book completions chart
# --------------------------------------------------

plot_monthly_completions <- function(
    data,
    lang = "en",
    dark = FALSE
) {

  if (nrow(data$completed_by_month) == 0) {
    return(NULL)
  }

  palette <- stats_palette(dark)

  max_completed <- max(
    1L,
    data$completed_by_month$books_finished,
    na.rm = TRUE
  )

  ggplot(
    data$completed_by_month,
    aes(x = month, y = books_finished)
  ) +

    geom_col(
      fill = palette$accent,
      width = 20
    ) +

    scale_x_date(
      breaks = data$month_breaks,
      labels = function(x) {
        m_date(x, lang, style = "month")
      },
      expand = expansion(
        add = c(17, 17)
      )
    ) +

    scale_y_continuous(
      breaks = seq.int(
        0L,
        max_completed
      ),
      labels = function(x) {
        m_number(x, lang)
      },
      expand = expansion(
        mult = c(0, 0.15)
      )
    ) +

    labs(
      x = NULL,
      y = m_text("books_completed", lang)
    ) +

    stats_theme(dark) +

    theme(
      axis.text.x = element_text(
        angle = 25,
        hjust = 1
      )
    )
}


# --------------------------------------------------
# 7. Display charts or localized empty-state message
# --------------------------------------------------

show_stats_plot <- function(
    plot,
    lang = "en"
) {

  if (is.null(plot)) {

    cat(
      '\n<p class="stats-empty">',
      m_text("empty_chart", lang),
      '</p>\n\n',
      sep = ""
    )

  } else {

    print(plot)

  }
}
