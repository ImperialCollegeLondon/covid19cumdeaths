
# process_data.R
#
# Author: Bob Verity
# Date: 2020-05-29
#
# Purpose:
# Read in the raw data, clean up and filter, and save processed data to file.
#
# ----------------------------------------------------------------
# DEATHS

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

# exclude Belgium (not diagnostically confirmed)
df_data <- subset(df_data, country != "Belgium")

# focus on countries with complete suppression data
df_data <- subset(df_data, !is.na(deaths_at_suppression))

# focus on countries with >=1 death at time of suppression
df_data <- subset(df_data, deaths_at_suppression >= 1)

# set maximum date present in all countries (e.g. Spain)
df_data$max_date <- as.Date("2020-05-17")
df_data <- subset(df_data, date <= max_date)

# get total deaths up to maximum date and merge back
tmp <- tapply(df_data$deaths, df_data$country, sum, na.rm = TRUE)
total_deaths <- data.frame(country = row.names(tmp),
                           deaths_total = as.vector(tmp),
                           stringsAsFactors = FALSE)
df_data <- merge(df_data, total_deaths, by = "country")

# focus on countries with >= 100 deaths total
df_data <- subset(df_data, deaths_total >= 10)

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

# get colour grouping based on deaths at suppression
col_breaks <- c(0, 1, 2, 5, 10, 20, 50, 100, 200, Inf)
col_break_names <- c("0", "1", "2-4", "5-9", "10-19", "20-49", "50-99", "150-199", "200+")
death_cut <- as.numeric(cut(df_data$deaths_at_suppression,
                            breaks = col_breaks,
                            right = FALSE))
df_data$col_group <- factor(col_break_names[death_cut], levels = col_break_names)

# save to file
saveRDS(df_data, "Output/deaths_processed.rds")


# ----------------------------------------------------------------
# SEROLOGY

# read in raw data
sero_raw <- read.csv("Data/summary_sero_vs_deaths.csv", stringsAsFactors = FALSE)

# remove Iran
sero <- subset(sero_raw, country != "iran")

# format country names as factors
sero$country <- tools::toTitleCase(sero$country)
sero_name <- c("Spain", "Sweden", "Switzerland", "Denmark")
sero$country <- factor(sero$country, levels = sero_name)

# get deaths per million population
sero$cumu_deaths_per_million <- sero$n_deaths / sero$population * 1e6

# save to file
saveRDS(sero, "Output/sero_processed.rds")

