############
# Legislative Violence
# Combined scatter plots to illustrate how framework relates to findings
# Christopher Gandrud
# Updated 18 March 2015
############

# Set working directory. Change as needed.
setwd('/git_repositories/LegislativeViolence/')

# Load packages
library(ggplot2)
library(gridExtra)
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(dplyr)

# Load data
leg_cumulative <- import('Data/LegislativeViolenceMain.csv') %>%
                    filter(year > 1980)

leg_cumulative <- leg_cumulative[!duplicated(leg_cumulative[,
                                        c('iso2c', 'year', 'violence_y_cum')]), ]

leg_cumulative$violence <- factor(leg_cumulative$violence, 
                                  label = c("No Violence", "Violence"))
leg_cumulative <- subset(leg_cumulative, violence != "NA")

cols <- c("1" = "#EDDD00", "2" = "#ED6700", "3" = "#A04400", "0" = "#5E5E5E")

## Create scatterplot of DemAge, Disproportionality, and Violence
age.disp.scatter <- qplot(dem_age, disproportionality,
                          position = position_jitter(w = 10),
                          color = factor(violence_y_cum),
                          data = leg_cumulative) +
                    facet_grid(.~violence) +
                    scale_y_log10(breaks = c(1, 2.5, 5, 10, 20, 30),
                                  labels = c(1, 2.5,  5, 10, 20, 30)) +
                    scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) +
                    xlab("\n Age of Democracy") +
                    ylab("Disproportionality (log)\n") +
                    scale_colour_manual(values = cols, name = "Brawl/Year") +
                    theme_bw(base_size = 12)


print(age.disp.scatter)
