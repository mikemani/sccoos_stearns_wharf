suppressMessages({
  library(shiny)
  library(cowplot)
  library(ggplot2)
  library(shinythemes)
})

# set config env to "default" if running app locally for development, and set it to
# production if running on shinyapps.io.



scaleFUN <- function(x) sprintf("%.2e", x)
theme_set(
  theme(text=element_text(family="Times"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        plot.title = element_text(size=12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(linewidth = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, hjust= 0),
        legend.position = 'top',
        # legend.key = element_rect(fill = NA,size = 0.25),
        legend.background = element_blank(),
        legend.key.width = unit(0.15, units = "cm"),
        legend.key.height = unit(0.15, units = "cm"),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        axis.text.x = element_text( angle=-45, hjust = 0.2, vjust = 1),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
)
