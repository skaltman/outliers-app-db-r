create_outliers <- function(data) {
  iqr_bound <- 1.5 * IQR(data$ppm)
  q1 <- quantile(data$ppm, 0.25)
  q3 <- quantile(data$ppm, 0.75)

  data |>
    filter(
      ppm > (q3 + iqr_bound) |
        ppm < (q1 - iqr_bound)
    )
}

