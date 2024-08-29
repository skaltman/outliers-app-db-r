library(tidyverse)
library(duckdb)

set.seed(456)

N_SAMPLE <- 500

path_ozone <- here::here("data-raw/ozone.csv")
cols_exclude <-
  c(
    "parameter_code",
    "parameter_name",
    "pollutant_standard",
    "units_of_measure"
  )

# ==============================================================================

ozone <-
  read_csv(path_ozone) |>
  sample_n(size = N_SAMPLE) |>
  select(-all_of(cols_exclude)) |>
  mutate(
    arithmetic_mean =
      if_else(
        rnorm(length(arithmetic_mean)) > 2.5,
        arithmetic_mean + 0.05,
        arithmetic_mean
      ),
    id = row_number(),
    flag = 0L
  ) |>
  select(id, everything())


con <- dbConnect(duckdb(), dbdir = "data/ozone.duckdb", read_only = FALSE)
DBI::dbWriteTable(con, "ozone", ozone, overwrite = TRUE)
dbDisconnect(con)
