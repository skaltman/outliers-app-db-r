library(shiny)
library(bslib)
library(ggplot2)
library(DBI)
library(DT)
library(plotly)
library(plu)
library(dplyr)

source("R/helpers.R")
includeCSS("www/styles.css")

choices <-
  c(
    "Date" = "date_local",
    "PPM" = "ppm",
    "AQI" = "aqi"
  )

ui <- page_fluid(
  markdown("## Identify suspicious values in air quality data"),
  layout_column_wrap(
    create_card(
      "Click on a suspicious value (in blue) to highlight the point in the table.",
      layout_column_wrap(
        selectInput(
          "plot_x",
          "X-axis variable:",
          choices = choices
        ),
        selectInput(
          "plot_y",
          "Y-axis variable:",
          choices = choices[choices != "date_local"]
        )
      ),
      plotlyOutput("plot")
    ),
    layout_column_wrap(
      width = 1,
      create_card(
        "Change `Flag` to `1` to flag a value as an error. Flagged points will appear red in the plot.",
        DTOutput("table"),
        actionButton("write_data", "Write to database", width = "50%")
      ),
      card(
        card_header("About this app", class = "bg-light"),
        markdown(
          "This app uses ozone data from the [EPA](https://www.epa.gov/outdoor-air-quality-data).
          The values shown in blue represent rows where `PPM` (ozone level in parts-per-million) was an outlier, identified using the [IQR method](https://en.wikipedia.org/wiki/Interquartile_range#Outliers).
          Some of these values are real, but some are errors, created for the purposes of this app. \n

          The app reads from and writes to an in-memory DuckDB database.
          When you refresh the page, the database will be regenerated from scratch, so you will not see your changes.
          "
        )
      ),
      heights_equal = "row"
    )
  )
)

server <- function(input, output, session) {

  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

  initialize_database(con, "data/ozone.duckdb", table = "ozone")

  ozone <-
    dplyr::tbl(con, "ozone") |>
    collect() |>
    rename(ppm = arithmetic_mean)

  outliers <- create_outliers(ozone)

  selectedRow <- reactiveVal(NULL)

  # Reactive value to store the edited data
  table_data <- reactiveVal(outliers)
  plot_data <- reactiveVal(outliers)

  # Capture edits made in the DataTable so that plot can be updated
  observeEvent(
    input$table_cell_edit,
    {
      edited_cell <- input$table_cell_edit
      new_plot_data <- plot_data()

      new_flag_value <- as.integer(edited_cell$value)
      if (new_flag_value > 1 || new_flag_value < 0) {
        showNotification(
          markdown("Error: `flag` must be either `0` or `1`."),
          type = "error"
        )
      }

      else {
        new_plot_data[edited_cell$row, "flag"] <- new_flag_value
        plot_data(new_plot_data)
      }
    }
  )

  # Plot data
  output$plot <-
    renderPlotly({
      plot_ozone(
        input, ozone, plot_data(), plotly_event = "plotly_click", choices
      )
    })

  # Editable datatable
  output$table <- renderDT({
    create_editable_table(table_data())
  })

  observeEvent(event_data("plotly_click"), {

    data_click <- event_data("plotly_click")
    if (is.null(data_click)) return(NULL)

    point_clicked <- data_click$pointNumber + 1
    selectedRow(point_clicked)
  })

  observe({
    selected <- selectedRow()
    if (is.null(selected)) return(NULL)
    proxy <- dataTableProxy("table")
    selectRows(proxy, selected)
  })

  # Write data to database on button click
  observeEvent(
    input$write_data, {
      tryCatch(
        {
          dbBegin(con)

          # Update table data reactive with edited values
          table_data(plot_data())

          rows_changed <-
            outliers |>
            left_join(
              table_data(),
              by = join_by("id"),
              suffix = c("_old", "_new")
            ) |>
            filter(flag_old != flag_new) |>
            select(id, flag_new)


          for (id in rows_changed$id) {
            dbExecute(
              con,
              "UPDATE ozone SET flag = ? WHERE id = ?",
              params = list(rows_changed$flag_new[rows_changed$id == id], id)
            )
          }

          n_changes <- nrow(rows_changed)

          if (n_changes > 0) {
            showNotification(
              markdown(
                glue::glue(
                  "{n_changes} `Flag` {plu::ral('value', n_changes[n_changes == 1])} successfully updated in database."
                )
              ),
              type = "message"
            )
          }

          dbCommit(con)
        },
        error = function(e) {
          dbRollback(con)
          showNotification("Error: Failed to update database.", type = "error")
        }
      )
    }
  )

  # Disconnect from DuckDB when the app stops
  onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)
