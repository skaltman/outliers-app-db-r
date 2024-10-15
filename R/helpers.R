initialize_database <- function(con, source_db, table) {
  source_con <- dbConnect(duckdb::duckdb(), dbdir = source_db)
  ozone <- dbReadTable(source_con, table)
  dbDisconnect(source_con)

  dbWriteTable(con, table, ozone)
}

create_card <- function(header_text, ...) {
  card(
    card_header(
      markdown(
        glue::glue("{bsicons::bs_icon('info-circle')} {header_text}")),
      class = "bg-light"
    ),
    ...
  )
}

create_outliers <- function(data) {
  iqr_bound <- 1.5 * IQR(data |> pull(ppm))
  q1 <- quantile(data |> pull(ppm), 0.25)
  q3 <- quantile(data |> pull(ppm), 0.75)

  data |>
    filter(
      ppm > (q3 + iqr_bound) |
        ppm < (q1 - iqr_bound)
    )
}

create_editable_table <- function(data) {
  datatable(
    data |>
      select(
        state_name,
        date_local,
        ppm,
        aqi,
        flag
      ) |>
      mutate(ppm = round(ppm, 2)),
    colnames = c("State", "Date", "PPM", "AQI", "Flag"),
    options =
      list(
        searching = FALSE,
        lengthChange = FALSE,
        info = FALSE,
        paging = FALSE,
        ordering = FALSE
      ),
    editable = list(target = "cell", disable = list(columns = c(0:3))),
    rownames = FALSE
  )
}

plot_ozone <- function(input, ozone, outliers, plotly_event, choices) {
  x <- sym(input$plot_x)
  y <- sym(input$plot_y)

  p <-
    ggplot(mapping = aes(!!x, !!y)) +
    geom_point(
      data =
        ozone |>
        anti_join(outliers, by = join_by("id")) |>
        mutate(flag = as.factor(flag)),
      alpha = 0.4,
      size = 2
    ) +
    geom_point(
      aes(color = flag),
      data = outliers |> mutate(flag = as.factor(flag)),
      alpha = 0.7,
      size = 2
    ) +
    scale_color_manual(values = c("#6ea0ff", "#dc3545")) +
    guides(color = "none") +
    theme_minimal() +
    labs(
      x = choices[choices == input$plot_x] |> names(),
      y = choices[choices == input$plot_y] |> names()
    )

  ggplotly(p) |>
    event_register(plotly_event) |>
    config(displayModeBar = FALSE)
}

