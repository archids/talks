library(rvest)
library(dplyr)
library(stringr)

#' Are the lights on?
#'
#' Retrieve load shedding status from http://loadshedding.eskom.co.za/.
#'
#' @return A Boolean indicating whether or not load shedding is applied.
#'
#' @examples
#' if (load_shedding()) buy_generator()
load_shedding <- function() {
  # Retrieve page content.
  html <- read_html("http://loadshedding.eskom.co.za/")
  # Extract relevant snippet of content.
  status <- html %>% html_node("#lsstatus") %>% html_text()
  # Normalise.
  status <- status %>% str_squish() %>% str_to_lower()
  
  status != "not load shedding"
}
