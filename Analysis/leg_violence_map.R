###############
# Create global map of legislative violence
# Christopher Gandrud
# 18 March 2015
###############

# Set working directory. Change as needed.
setwd('/git_repositories/LegislativeViolence/')

# Load packages
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(dplyr)
library(googleVis)

## Load data
leg <- import('Data/LegislativeViolenceMain.csv') %>%
            select(iso2c, violence)

# Get frequency count
incidence <- leg %>% group_by(iso2c) %>% summarise(`Leg Brawl` = sum(violence))

# Make Google Map
violence_map <- gvisGeoMap(incidence, locationvar = "iso2c", numvar = "Leg Brawl", 
                           options = list(
                             colors = "[0xffffff, 0xEDDD00, 0xED6700, 0xA04400]",
                             height="510",
                             width="750"
                             )
                           )

plot(violence_map)

print(violence_map, "chart", file = "~/Dropbox/Public/Graphs/leg.violence.map.html")
