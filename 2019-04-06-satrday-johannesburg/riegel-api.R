library(dplyr)
library(plotly)

#* @get /riegel
riegel <- function(time, exponent = 1.06) {
  # API inputs are character.
  time = as.numeric(time)
  exponent = as.numeric(exponent)
  # Fixed distances.
  distance = 5
  goal = 42.2
  # Calculate time for goal distance.
  time * (goal / distance) ** exponent
}

#* @get /riegel-plot
#* @serializer htmlwidget
riegel_plot <- function(time, exponent = 1.06) {
  time = as.numeric(time)
  exponent = as.numeric(exponent)
  
  times <- data.frame(
    distance = seq(5, 100, 1)
  ) %>%
    mutate(
      time = time * (distance / 5) ** exponent,
      pace = time / distance,
      time_text = sprintf("%d km - %.1f min", distance, time),
      pace_text = sprintf("%d km - %.1f min/km", distance, pace)
    )
  
  plot_ly(times, x = ~distance) %>%
    add_trace(y = ~time, name = "time", mode = "lines",
              text = ~time_text,
              hoverinfo = "text") %>%
    add_trace(y = ~pace, name = "pace", mode = "lines", yaxis = "y2",
              text = ~pace_text,
              hoverinfo = "text") %>%
    layout(
      title = "Predictions from Riegel's Formula",
      legend = list(x = 1.05, y = 1.0),
      xaxis = list(
        title = "distance [km]",
        showgrid = FALSE,
        range = c(0, max(times$distance)),
        zeroline = FALSE
      ),
      yaxis = list(
        title = "time [min]",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        title = "pace [min/km]",
        tickformat = "0.1f",
        showgrid = FALSE
      )
    )
}

# To run:
#
if (FALSE) {
  library(plumber)
  plumb("riegel-api.R")$run(port=8000)
}
