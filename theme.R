#' LiU Theme and Color Palette with Custom Font
#'
#' A custom theme and color palette based on the LiU style, with the 'calibri' font.
#' This function applies both a custom ggplot theme and a manual color scale.
#'
#' @return A list containing the custom theme, color scale, and font settings.
#' @export
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg, color = as.factor(cyl))) +
#' geom_point() +
#' LiU_theme_and_colors()

LiU_theme_and_colors <- function() {
  # Enable showtext for custom fonts
  showtext_auto()
  
  # Add the 'calibri' font from the package
  font_add(family = "calibri", regular = system.file("fonts", "calibri.ttf", package = "LiUTheme"))
  
  list(
    theme_grey() + 
      theme(text = element_text(family = "calibri"),
            panel.background = element_rect(fill = "#bde6f6", color = NA),
            plot.background = element_rect(fill = "#d2eef9", color = NA),
            legend.background = element_rect(fill = "#bde6f6", color = NA)
      ),
    scale_color_manual(values = c("#ff6442", "#8781d3", "#fcf05f", "#687f91"))
  )
}