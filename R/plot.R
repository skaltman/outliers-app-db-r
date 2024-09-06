choices <-
  c(
    "Date" = "date_local",
    "PPM" = "ppm",
    "AQI" = "aqi"
  )

plot_ozone <- function(input, ozone, outliers, plotly_event) {
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

