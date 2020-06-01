
# plot_figures_combined.R
#
# Author: Bob Verity
# Date: 2020-05-29
#
# Purpose:
# Read in plotting objects and produce combined plot.
#
# ------------------------------------------------------------------

library(ggplot2)
library(grid)
library(gridExtra)

# run individual scripts
source("plot_figure1.R")
source("plot_figure2.R")
source("plot_figure3.R")

# load grobs
plot1 <- readRDS("Output/plot1.rds")
plot2 <- readRDS("Output/plot2.rds")
plot3 <- readRDS("Output/plot3.rds")

# add panel titles
panel_hjust <- -0.3
panel_size <- 10
plot1 <- plot1 + ggtitle("A)") +
  theme(plot.title = element_text(size = panel_size, hjust = panel_hjust))
plot2 <- plot2 + ggtitle("B)") +
  theme(plot.title = element_text(size = panel_size, hjust = panel_hjust))
plot3 <- plot3 + ggtitle("C)") +
  theme(plot.title = element_text(size = panel_size, hjust = panel_hjust))

# add annotation to plot1
plot1b <- ggplot_gtable(ggplot_build(plot1))
plot1b$layout$clip[gt$layout$name == "panel"] <- "off"

# combine plots
plot_combined <- gridExtra::grid.arrange(plot1b, plot2, plot3, nrow = 1)

# render to file
ggsave(filename = "Figures/figures_combined.pdf", plot = plot_combined, width = 12, height = 5)
ggsave(filename = "Figures/figures_combined.png", plot = plot_combined, width = 12, height = 5)
