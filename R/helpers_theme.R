# helpers_theme.R -----------------------------------------------------
library(ggplot2)

theme_gg_dashboard <- function() {
  theme_minimal(base_family = "system-ui") +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text        = element_text(color = "#333"),
      axis.title       = element_text(color = "#444"),
      plot.title       = element_text(face = "bold", size = 13),
      legend.position  = "bottom"
    )
}
