
# figures.R
#
# Author: Bob Verity
# Date: 2020-05-29
#
# Purpose:
# Read in the raw data, clean up and filter, and produce two figures:
#  1. curves of cumulative deaths
#  2. correlation plot of deaths before vs. after suppression
#
# ------------------------------------------------------------------

library(ggplot2)
library(grid)
library(gridExtra)

# read in processed data
df_data <- readRDS("Output/deaths_processed.rds")

# get max date
max_date <- df_data$max_date[1]

# get colours for each colour group
my_pal <- colorRampPalette(c("#D73027", "#FC8D59", "#FEE090", "#E0F3F8", "#91BFDB", "#4575B4"))
ncol <- nlevels(df_data$col_group)
col_vec <- rev(my_pal(ncol))
names(col_vec) <- levels(df_data$col_group)

# produce plot
plot1 <- ggplot(df_data) + theme_bw() +
  geom_line(aes(x = date, y = cumu_deaths_per_million, color = col_group, group = country)) +
  scale_x_date(limits = c(as.Date("2020-03-01"), max_date), expand = c(0,1)) +
  scale_y_continuous(expand = c(0,10)) +
  xlab("date\n") + ylab("cumulative deaths\nper million") +
  scale_color_manual(values = col_vec, name = "deaths before\nlockdown") +
  theme(legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3),
        plot.margin = unit(c(1, 2.5, 0.5, 0.5), "cm"),
        legend.spacing.x = unit(0, 'mm'),
        legend.spacing.y = unit(0, 'mm'),
        legend.box.background = element_rect(colour = "black"))

plot1
#plot1 + scale_y_continuous(trans = "log10")

# add points for some countries
focal_countries <- subset(df_data, date == max_date &
                            (cumu_deaths_per_million > 200 | country %in% c("Switzerland", "Denmark")))

plot1 <- plot1 + 
  geom_point(aes(x = date, y = cumu_deaths_per_million, color = as.factor(col_group)), data = focal_countries)

# offset country labels
focal_countries$text_pos <- focal_countries$cumu_deaths_per_million
w <- which(focal_countries$country == "Italy")
focal_countries$text_pos[w] <- focal_countries$text_pos[w] + 12
w <- which(focal_countries$country == "United Kingdom")
focal_countries$text_pos[w] <- focal_countries$text_pos[w] - 12
w <- which(focal_countries$country == "Netherlands")
focal_countries$text_pos[w] <- focal_countries$text_pos[w] + 10
w <- which(focal_countries$country == "Ireland")
focal_countries$text_pos[w] <- focal_countries$text_pos[w] - 10

# add country labels
for (i in seq_len(nrow(focal_countries))) {
  plot1 <- plot1 +
    annotation_custom(grob = textGrob(label = focal_countries$country[i],
                                      hjust = 0, gp = gpar(fontsize = 8)),
                    xmin = focal_countries$date[i] + 2,
                    ymin = focal_countries$text_pos[i],
                    ymax = focal_countries$text_pos[i])
}

# save grob to file
saveRDS(plot1, "Output/plot1.rds")

# render to screen with annotation
gt <- ggplot_gtable(ggplot_build(plot1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

# render to file with annotation
ggsave(filename = "Figures/figure1.pdf", plot = gt, width = 6, height = 5)
ggsave(filename = "Figures/figure1.png", plot = gt, width = 6, height = 5)

