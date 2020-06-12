
# plot_figure2.R
#
# Author: Bob Verity
# Date: 2020-05-29
#
# Purpose:
# Read in processed data and plot correlation of deaths before vs. after lockdown.
#
# ------------------------------------------------------------------

library(ggplot2)
library(grid)
library(gridExtra)

# read in processed data
df_data <- readRDS("Output/deaths_processed.rds")

# get colours for each colour group
my_pal <- colorRampPalette(c("#D73027", "#FC8D59", "#FEE090", "#E0F3F8", "#91BFDB", "#4575B4"))
ncol <- nlevels(df_data$col_group)
col_vec <- rev(my_pal(ncol))
names(col_vec) <- levels(df_data$col_group)

# subset to cumulative deaths at time of suppression, and cumulative deaths six weeks later
df_data$deaths_per_million_at_suppression <- 1000 * df_data$deaths_at_suppression / df_data$pop_total
tmp <- subset(df_data, date == df_data$date_suppression + 6*7)
df_corplot <- data.frame(country = tmp$country,
                         deaths_before = tmp$deaths_per_million_at_suppression,
                         deaths_after = tmp$cumu_deaths_per_million - tmp$deaths_per_million_at_suppression,
                         col_group = tmp$col_group,
                         popsize = tmp$pop_total)

# produce plot
plot2 <- ggplot(df_corplot, aes(x = deaths_before, y = deaths_after)) + theme_bw() +
  geom_point(aes(fill = as.factor(col_group)), shape = 21, size = 2, stroke = 0.2) +
  scale_x_log10() +
  scale_y_log10(limits = c(1e-1, 1e3), expand = c(0,0)) +
  scale_fill_manual(values = col_vec, name = "deaths before\nlockdown") +
  xlab("deaths per million\nbefore lockdown") + ylab("deaths per million in six week\nperiod after lockdown") +
  theme(legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "cm"),
        plot.title = element_text(hjust = -0.3),
        legend.spacing.x = unit(0, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        legend.box.background = element_rect(colour = "black"))

plot2

# add linear model
plot2 <- plot2 + geom_smooth(method = "lm", formula = "y ~ x",
                             linetype = "dashed", color = grey(0.5), size = 0.5, se = FALSE)

plot2

# save grob to file
saveRDS(plot2, "Output/plot2.rds")

# render to file
ggsave(filename = "Figures/figure2.pdf", plot = plot2, width = 5, height = 5)
ggsave(filename = "Figures/figure2.png", plot = plot2, width = 5, height = 5)


