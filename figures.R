
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

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  }

# ----------------------------------------------------------------
# BASIC DATA CLEANING

# read in raw data
measures_raw <- read.csv("Data/Appendix_country_specific_suppression_measures.csv", stringsAsFactors = FALSE)
ecdc_raw <- read.csv("Data/daily_deaths_ECDC20200518.csv", stringsAsFactors = FALSE)
pop_raw <- read.csv("Data/WPP2019_TotalPopulationBySex.csv", stringsAsFactors = FALSE)
sero_raw <- read.csv("Data/summary_sero_vs_deaths.csv", stringsAsFactors = FALSE)

# clean up ECDC data
ecdc <- data.frame(country = gsub("_", " ", ecdc_raw$countriesAndTerritories),
                     date = as.Date(ecdc_raw$dateRep, "%d/%m/%Y"),
                     deaths = ecdc_raw$deaths,
                     stringsAsFactors = FALSE)
ecdc$country[ecdc$country == "United States of America"] <- "United States"

# clean up measures data
measures <- data.frame(country = measures_raw$Country,
                       date_suppression = as.Date(measures_raw$Date_suppression, "%d/%m/%Y"),
                       deaths_at_suppression = measures_raw$deaths_at_suppression,
                       stringsAsFactors = FALSE)

# clean up pop data
pop_sub <- subset(pop_raw, MidPeriod == "2019.5")
pop <- data.frame(country = pop_sub$Location,
                  pop_total = pop_sub$PopTotal,
                  stringsAsFactors = FALSE)
pop$country[pop$country == "United States of America"] <- "United States"

# merge data
df_data <- merge(ecdc, measures, by = "country")
df_data <- merge(df_data, pop, by = "country")

# ----------------------------------------------------------------
# PROCESS CUMULATIVE DEATHS

# exclude Belgium
df_data <- subset(df_data, country != "Belgium")

# focus on countries with complete suppression data
df_data <- subset(df_data, !is.na(deaths_at_suppression))

# focus on countries with >=1 death at time of suppression
df_data <- subset(df_data, deaths_at_suppression >= 1)

# get total deaths
tmp <- tapply(df_data$deaths, df_data$country, sum, na.rm = TRUE)
total_deaths <- data.frame(country = row.names(tmp),
                           deaths_total = as.vector(tmp),
                           stringsAsFactors = FALSE)

# focus on countries with >= 100 deaths total
df_data <- merge(df_data, total_deaths, by = "country")
df_data <- subset(df_data, deaths_total >= 10)

# set maximum date present in all countries (e.g. Spain)
max_date <- as.Date("2020-05-17")
df_data <- subset(df_data, date <= max_date)

# get cumulative deaths
countries <- unique(df_data$country)
df_data <- df_data[order(df_data$country, df_data$date), ]
df_data$cumu_deaths <- NA
for (i in seq_along(countries)) {
  w <- which(df_data$country == countries[i])
  df_data$cumu_deaths[w] <- cumsum(df_data$deaths[w])
}

# get deaths per million
df_data$cumu_deaths_per_million <- 1000 * df_data$cumu_deaths / df_data$pop_total

# get colour grouping
col_breaks <- c(0, 1, 2, 5, 10, 20, 50, 100, 200, Inf)
col_break_names <- c("0", "1", "2-4", "5-9", "10-19", "20-49", "50-99", "150-199", "200+")
df_data$col_group <- cut(df_data$deaths_at_suppression,
                                    breaks = col_breaks,
                                    right = FALSE)

# get colours
my_pal <- colorRampPalette(c("#D73027", "#FC8D59", "#FEE090", "#E0F3F8", "#91BFDB", "#4575B4"))
ncol <- length(col_breaks) - 1
col_vec <- rev(my_pal(ncol))

# ensure correct matching of colours
names(col_vec) <- levels(df_data$col_group)
names(col_break_names) <- levels(df_data$col_group) 

# ----------------------------------------------------------------
# FIGURE 1: CUMULATIVE DEATHS

# figure 1 plot
plot1 <- ggplot(df_data) + theme_bw() +
  geom_line(aes(x = date, y = cumu_deaths_per_million, color = col_group, group = country)) +
  scale_x_date(limits = c(as.Date("2020-03-01"), max_date), expand = c(0,1)) +
  scale_y_continuous(expand = c(0,10)) +
  xlab("date\n") + ylab("cumulative deaths\nper million") + ggtitle("A)") +
  scale_color_manual(values = col_vec, labels = col_break_names, name = "deaths before\nlockdown") +
  theme(legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3),
        plot.margin = unit(c(1, 2.5, 0.0, 0.5), "cm"),
        plot.title = element_text(hjust = -0.3))

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

# plot with annotation
gt <- ggplot_gtable(ggplot_build(plot1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

# save plot to file
ggsave(filename = "Figures/figure1.pdf", plot = gt, width = 6, height = 5)
ggsave(filename = "Figures/figure1.png", plot = gt, width = 6, height = 5)

# extract legend (store for later)
plot1 <- plot1 + guides(color = guide_legend(nrow = 1))
plot1_legend <- g_legend(plot1)
plot1 <- plot1 + theme(legend.position = "none")

# re-plot with annotation
gt <- ggplot_gtable(ggplot_build(plot1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"


# ----------------------------------------------------------------
# FIGURE 2: BEFORE VS. AFTER CORRELATION

# subset to cumulative deaths at time of suppression, and cumulative deaths six weeks later
df_data$deaths_per_million_at_suppression <- df_data$deaths_at_suppression / df_data$pop_total
tmp <- subset(df_data, date == df_data$date_suppression + 6*7)
df_corplot <- data.frame(country = tmp$country,
                         deaths_before = tmp$deaths_per_million_at_suppression,
                         deaths_after = tmp$cumu_deaths_per_million - tmp$deaths_per_million_at_suppression,
                         col_group = tmp$col_group,
                         popsize = tmp$pop_total)

# figure 2 plot
plot2 <- ggplot(df_corplot, aes(x = deaths_before, y = deaths_after)) + theme_bw() +
  geom_point(aes(fill = as.factor(col_group)), shape = 21, size = 2, stroke = 0.2) +
  scale_x_log10() +
  scale_y_log10(limits = c(1e-1, 1e3), expand = c(0,0)) +
  scale_fill_manual(values = col_vec, labels = col_break_names, name = "deaths before\nlockdown") +
  guides(fill = FALSE) +
  xlab("deaths per million\nbefore lockdown") + ylab("deaths per million in six week\nperiod after lockdown") +
  ggtitle("B)") +
  theme(legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3),
        plot.margin = unit(c(1, 1, 0.0, 0.5), "cm"),
        plot.title = element_text(hjust = -0.3))

# test correlation
ct <- cor.test(df_corplot$deaths_before, df_corplot$deaths_after)
ct$estimate
ct$p.value

# add linear model
plot2 <- plot2 + geom_smooth(method = "lm", formula = "y ~ x",
                             linetype = "dashed", color = grey(0.5), size = 0.5, se = FALSE)

plot2

# save plot to file
ggsave(filename = "Figures/figure2.pdf", plot = plot2, width = 5, height = 5)
ggsave(filename = "Figures/figure2.png", plot = plot2, width = 5, height = 5)

# ----------------------------------------------------------------
# FIGURE 3: SEROLOGY

# remove Iran
sero <- subset(sero_raw, country != "iran")

# format country names
sero$country <- tools::toTitleCase(sero$country)
sero_name <- c("Sweden", "Spain", "Switzerland", "Denmark")
sero$country <- factor(sero$country, levels = sero_name)

# get deaths per million population
sero$cumu_deaths_per_million <- sero$n_deaths / sero$population * 1e6

# set colours
sero_colvec <- my_pal(4)
names(sero_colvec) <- sero_names

# figure 3
plot3 <- ggplot(sero) + theme_bw() +
  geom_abline(slope = c(0.1, 0.5, 1, 2, 5)*100, color = grey(0.8), size = 0.3) +
  geom_point(aes(x = seroprevalence * 100, y = cumu_deaths_per_million, fill = country), shape = 21, size = 2.5, stroke = 0.2) +
  scale_fill_manual(values = sero_colvec, name = "country") +
  xlab("seroprevalence (%)\n") + ylab("deaths per million at time of\nseroprevalence study") + ggtitle("C)") +
  scale_x_continuous(limits = c(0,15), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1500), expand = c(0,0), n.breaks = 5) +
  theme(legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3),
        plot.margin = unit(c(1, 1, 0.0, 0.5), "cm"),
        plot.title = element_text(hjust = -0.3)) +
  guides(fill = guide_legend(nrow = 2))

# add percentages
plot3 <- plot3 + annotate("text", x = c(12, 11, 9, 6, 2),
                          y = c(170, 620, 1000, 1300, 1400),
                          label = sprintf("%s%%", c(0.1, 0.5, 1, 2, 5)),
                          hjust = 0,
                          size = 3,
                          col = grey(0.5))

plot3

# save plot to file
ggsave(filename = "Figures/figure3.pdf", plot = plot3, width = 5, height = 5)
ggsave(filename = "Figures/figure3.png", plot = plot3, width = 5, height = 5)

# extract legend (store for later)
plot3_legend <- g_legend(plot3)
plot3 <- plot3 + theme(legend.position = "none")


# ----------------------------------------------------------------
# COMBINED PLOT

# plot in grid
layout_mat <- rbind(matrix(1:3, nrow = 5, ncol = 3, byrow = TRUE),
                    c(4,4,5))
plot_combined <- gridExtra::grid.arrange(gt, plot2, plot3, plot1_legend, plot3_legend,
                                         layout_matrix = layout_mat)

# save to file
ggsave(filename = "Figures/figures_combined.pdf", plot = plot_combined, width = 12, height = 5)
ggsave(filename = "Figures/figures_combined.png", plot = plot_combined, width = 12, height = 5)
