
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

# ----------------------------------------------------------------

# read in raw data
measures_raw <- read.csv("Data/Appendix_country_specific_suppression_measures.csv", stringsAsFactors = FALSE)
ecdc_raw <- read.csv("Data/daily_deaths_ECDC20200518.csv", stringsAsFactors = FALSE)
pop_raw <- read.csv("Data/WPP2019_TotalPopulationBySex.csv", stringsAsFactors = FALSE)

# clean up ECDC data
ecdc <- data.frame(country = gsub("_", " ", ecdc_raw$countriesAndTerritories),
                   date = as.Date(ecdc_raw$dateRep, "%d/%m/%Y"),
                   deaths = ecdc_raw$deaths,
                   stringsAsFactors = FALSE)
ecdc$country[ecdc$country == "United States of America"] <- "United States"

# clean up measures data
measures <- data.frame(country = measures_raw$Country,
                       date_suppression = as.Date(measures_raw$Date.of.suppression, "%d/%m/%Y"),
                       deaths_at_suppression = measures_raw$Deaths.at.suppression,
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

# focus on countries with complete suppression data
df_data <- subset(df_data, !is.na(date_suppression) & !is.na(deaths_at_suppression))

# focus on countries with >=1 death at time of suppression
df_data <- subset(df_data, deaths_at_suppression >= 1)

# get total deaths
tmp <- tapply(df_data$deaths, df_data$country, sum, na.rm = TRUE)
total_deaths <- data.frame(country = row.names(tmp),
                           deaths_total = as.vector(tmp),
                           stringsAsFactors = FALSE)

# focus on countries with >= 100 deaths total
df_data <- merge(df_data, total_deaths, by = "country")
df_data <- subset(df_data, deaths_total >= 100)

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

# get deaths six weeks after suppresion
df_data$cumu_deaths_six_weeks = 0
for (i in 1:length(countries)) {
  sub = df_data[df_data$country==countries[i],]
  wh<- sub$date>=sub$date_suppression[1] & sub$date<=(sub$date_suppression[1]+(6*7))
  df_data$cumu_deaths_six_weeks[df_data$country==countries[i]] = sum(sub$deaths[wh])
}

# get deaths per million
df_data$cumu_deaths_per_million_six_weeks <- 1000 * df_data$cumu_deaths_six_weeks / df_data$pop_total


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


# figure 1 plot
plot1 <- ggplot(df_data) + theme_bw() +
  geom_line(aes(x = date, y = cumu_deaths_per_million, color = col_group, group = country)) +
  scale_x_date(limits = c(as.Date("2020-03-01"), max_date), expand = c(0,1)) +
  scale_y_continuous(expand = c(0,10)) +
  xlab("date") + ylab("cumulative deaths\nper million") +
  scale_color_manual(values = col_vec, labels = col_break_names, name = "deaths before\nlockdown") +
  theme(legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3),
        plot.margin = unit(c(1, 2.5, 0.5, 0.5), "cm"))

plot1

# add points for some countries
focal_countries <- subset(df_data, date == max_date & cumu_deaths_per_million_six_weeks > 200)
plot1 <- plot1 + 
  geom_point(aes(x = date, y = cumu_deaths_per_million_six_weeks, color = as.factor(col_group)), data = focal_countries)

# offset country labels
focal_countries$text_pos <- focal_countries$cumu_deaths_per_million_six_weeks
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

# ----------------------------------------------------------------

# subset to cumulative deaths at time of suppression, and cumulative deaths six weeks later
df_data$deaths_per_million_at_suppression <- df_data$deaths_at_suppression / df_data$pop_total
tmp <- subset(df_data, date == df_data$date_suppression)
df_corplot <- data.frame(country = tmp$country,
                         deaths_before = tmp$deaths_per_million_at_suppression,
                         deaths_after = tmp$cumu_deaths_per_million_six_weeks - tmp$deaths_per_million_at_suppression,
                         col_group = tmp$col_group)

# figure 2 plot
plot2 <- ggplot(df_corplot, aes(x = deaths_before, y = deaths_after)) + theme_bw() +
  geom_point(aes(fill = as.factor(col_group)), shape = 21, size = 2) +
  scale_x_log10() + scale_y_log10() +
  scale_fill_manual(values = col_vec, labels = col_break_names, name = "deaths before\nlockdown") +
  guides(fill = FALSE) +
  xlab("deaths per million\nbefore suppresion") + ylab("deaths per million\n six weeks after suppresion")

# test correlation
ct <- cor.test(df_corplot$deaths_before, df_corplot$deaths_after)
ct$estimate
ct$p.value
#plot2 <- plot2 + ggtitle(sprintf("correlation = %s\np < 0.001", format(signif(ct$estimate, 2), nsmall = 2))) +
#  theme(plot.title = element_text(size = 12))

# add linear model
plot2 <- plot2 + geom_smooth(method = "lm", formula = "y ~ x",
                             linetype = "dashed", color = grey(0.5), size = 0.5, se = FALSE)

plot2

# save plot to file
ggsave(filename = "Figures/figure2.pdf", plot = plot2, width = 5, height = 5)
ggsave(filename = "Figures/figure2.png", plot = plot2, width = 5, height = 5)
