
# plot_figure3.R
#
# Author: Bob Verity
# Date: 2020-05-29
#
# Purpose:
# Read in processed data and plot seroprevalence vs mortality.
#
# ------------------------------------------------------------------

library(ggplot2)
library(grid)
library(gridExtra)

# read in processed data
sero <- readRDS("Output/sero_processed.rds")

# set colours
col_vec <- c("#D73027", "#FDC47D", "#C5E1EE", "#4575B4")
col_vec <- RColorBrewer::brewer.pal(4, "Set1")
names(col_vec) <- levels(sero$country)

# figure 3
plot3 <- ggplot(sero) + theme_bw() +
  geom_abline(slope = c(0.1, 0.5, 1, 2, 5)*100, color = grey(0.8), size = 0.3) +
  geom_point(aes(x = seroprevalence * 100, y = cumu_deaths_per_million, fill = country), shape = 21, size = 2.5, stroke = 0.2) +
  scale_fill_manual(values = col_vec, name = "country") +
  xlab("seroprevalence (%)\n") + ylab("deaths per million at time of\nseroprevalence study") +
  scale_x_continuous(limits = c(0,15), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1500), expand = c(0,0), n.breaks = 5) +
  theme(legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "cm"),
        plot.title = element_text(hjust = -0.3),
        legend.spacing.x = unit(0, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        legend.box.background = element_rect(colour = "black")) +
  guides(fill = guide_legend(nrow = 2))

plot3

# add percentages
plot3 <- plot3 + annotate("text", x = c(12, 11, 9, 6, 2),
                          y = c(170, 620, 1000, 1300, 1400),
                          label = sprintf("%s%%", c(0.1, 0.5, 1, 2, 5)),
                          hjust = 0,
                          size = 3,
                          col = grey(0.5))

plot3

# save grob to file
saveRDS(plot3, "Output/plot3.rds")

# render to file
ggsave(filename = "Figures/figure3.pdf", plot = plot3, width = 5, height = 5)
ggsave(filename = "Figures/figure3.png", plot = plot3, width = 5, height = 5)

