library(ggplot2)
library(showtext)

setwd("/home/johmo870/Desktop/LinearRegression-1/")

# Enable Calibri font
showtext_auto()
font_add(family = "calibri", regular = "calibri.ttf")
font_add(family = "georgia", regular = "georgia.ttf")

# Custom theme
custom_theme <- theme_minimal() +
  theme(
    text = element_text(family = "calibri"),
    plot.background = element_rect(fill = "#d2eef9"),
    #panel.background = element_rect(fill = "#d2eef9"),
    #panel.grid.major = element_line(color = "#687f91"),
    axis.title = element_text(color = "#ff6442"),
    axis.text = element_text(color = "#8781d3"),
    plot.title = element_text(color = "#fcf05f", hjust = 0.5)
  )

theme_custom <- theme_grey() + 
  theme(text = element_text(family = "calibri"),
        panel.background = element_rect(fill = "#bde6f6", color = NA),
        plot.background = element_rect(fill = "#d2eef9", color = NA),
        legend.background = element_rect(fill = "#bde6f6", color = NA)
        )

# Example plot
ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#ff6442", "#8781d3", "#fcf05f", "#687f91")) +
  ggtitle("My title") +
  theme_custom


# Use grDevices to adjust the color
lighter_bg_color <- grDevices::adjustcolor("#0cc7d3", alpha.f = 0.8) # 20% towards white

# Update the theme with the new color
custom_theme <- theme_minimal() +
  theme(
    text = element_text(family = "calibri"),
    plot.background = element_rect(fill = lighter_bg_color),
    panel.background = element_rect(fill = lighter_bg_color),
    panel.grid.major = element_line(color = "#687f91"),
    axis.title = element_text(color = "#ff6442"),
    axis.text = element_text(color = "#8781d3"),
    plot.title = element_text(color = "#fcf05f", hjust = 0.5)
  )

# Example plot
ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#ff6442", "#8781d3", "#fcf05f", "#687f91")) +
  custom_theme
