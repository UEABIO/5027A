# book-specific code to include on every page

library(ggplot2)

my_theme <- theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),  # Removes gridlines
    axis.line = element_line(color = "black")  # Adds x and y axis lines
  )

theme_set(my_theme)
