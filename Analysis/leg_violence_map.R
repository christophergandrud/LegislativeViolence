###############
# Create global map of legislative violence
# Christopher Gandrud
# 6 September 2013
###############

library(googleVis)
library(gregmisc)
library(plyr)

## Load data
leg.raw <- read.csv("/Users/christophergandrud/Dropbox/Leg_Violence/Data/Legislative_Violence_Research_stata_ready.csv")

# Get frequency count
  incidence <- count(leg.raw, c("violence", "Country"))
  incidence <- subset(incidence, violence == 1)
  incidence <- incidence[,2:3]

# Clean up data
  incidence$Country <- gsub("Korea South", "South Korea", incidence$Country)
  incidence <- rename.vars(incidence, from ="freq", to = "Leg Brawl", info = FALSE)

# Make Google Map
violence.map <- gvisGeoMap(incidence, locationvar = "Country", numvar = "Leg Brawl", 
                           options = list(
                             colors = "[0xEDDD00, 0xED6700, 0xA04400]",
                             height="510",
                             width="750"
                             )
                           )

plot(violence.map)

print(violence.map, "chart", file = "/Users/christophergandrud/Dropbox/Public/Graphs/leg.violence.map.html")