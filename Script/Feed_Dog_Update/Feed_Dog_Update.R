
# Packages ---------------

library(tidyverse)
library(calendR)
library(ggplot2)
library(fs)
library(glue)

# Parameters & Events ----------------------- 

this_year <- year(today())
month <- 12
month_name <- month.name[month]

custom_events <- calendR(year = this_year,
                         month = month)

add_custom_text <- "This is some text"

events <- custom_events$data |> 
  mutate(rows = row_number(),
         date = case_when(
           rows == 12 ~ "Heartworm - Kya & Marvin",
           .default = NA)
         ) |> 
  pull(date)



## working within ggplot means working with a coordinate system
custom_cal <- calendR(year = this_year,
                      month = month,
                      title = glue("{month_name} {this_year}: Fed Dog?"),
                      
                      special.days = events,
                      special.col = c("lightblue"),
                      legend.pos = "top",
                      
                      text.size = 3,
                      margin = 1,
                      weeknames = c("Mo", "Tu",  # Week names
                                    "We", "Th",
                                    "Fr", "Sa",
                                    "Su"),
                      day.size = 2.25
                      )

# Horizontal ------------------

# Reversed Diagonal -----------------------

annotate_AmPm <- function(y) {
  annotate(geom = "text",
           x = -1,
           y = y,
           label = "AM\nPM")
}

custom_cal + 
  geom_segment(aes(x = (dow) - 0.5, xend = (dow) + 0.5,
                   y = y + 0.5, yend = y - 0.5), col = "gray60") +
  annotate(geom = "text",
           # x = .5, y = 0,
           label = glue("{add_custom_text}"),
           hjust = 0,vjust = 1) +
  annotate_AmPm(1:5)

if(dir_exists(glue("Output/{this_year}"))){

ggsave(filename = glue("Output/{this_year}/FeedDog_calendar_{this_year}_{month}.pdf"),
       device = cairo_pdf,
       units = "in",
       height = 8.5,
       width = 11)
} else {
  dir_create(glue("Output/{this_year}"))
  
  ggsave(filename = glue("Output/{this_year}/FeedDog_calendar_{this_year}_{month}.pdf"),
         device = cairo_pdf,
         units = "in",
         height = 8.5,
         width = 11)
}



