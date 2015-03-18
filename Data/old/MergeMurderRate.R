##########
# Merge in UN Murder Rate Data (murders per 100,000) 
# Christopher Gandrud
# 6 September 2013
##########

# Load packages
library(countrycode)

#### Load murder data
Murder <- read.csv("/git_repositories/LegislativeViolence/Data/Others/UNdata_HomicideRate.csv", stringsAsFactors = FALSE)

# Clean
Murder$iso2c <- countrycode(Murder$Country.or.Area, origin = "country.name",
                              destination = "iso2c")
MurderSub <- Murder[, c("iso2c", "Year", "Rate")]

names(MurderSub) <- c("iso2c", "year", "UNMurderRate")

MurderSub <- MurderSub[order(MurderSub$iso2c, MurderSub$year), ]

#### Load main data
leg.raw <- read.csv("/git_repositories/LegislativeViolence/Data/LegViolenceMain.csv")

leg.raw$iso2c <- countrycode(leg.raw$country, origin = "country.name",
                            destination = "iso2c")

#### Merge together
leg <- merge(leg.raw, MurderSub, by = c("iso2c", "year"), all.x = TRUE)

write.csv(leg, "/git_repositories/LegislativeViolence/Data/LegViolenceMain.csv", 
          row.names = FALSE)
