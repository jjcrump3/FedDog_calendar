
library(tidyverse)
library(ggtext)

theme_home <- theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 20, lineheight = 1.2),
    axis.text = element_markdown(size = 12, lineheight = 1.2),
    axis.title = element_markdown(size = 12, lineheight = 1.2),
    strip.text = element_markdown(size = 12, lineheight = 1.2),
  )



theme_set(theme_home)


