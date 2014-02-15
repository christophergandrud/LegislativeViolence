##########
# Merge % women in parliament 
# Christopher Gandrud
# 16 October 2013
##########

## Merge % women in parliament 


# From 1997 data from Inter-Parliamentary Union via World Bank Development Indicators 
## WDI indicator ID: SG.GEN.PARL.ZS

library(WDI)

women <- WDI(indicator = "SG.GEN.PARL.ZS", start = 1997, extra = FALSE)
women <- women[, c("iso2c", "year", "SG.GEN.PARL.ZS")]
names(women) <- c("iso2c", "year", "WomenInParl")


# Data from before 1997 from ICPSR: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/24340
library(foreign)
library(reshape2)
library(countrycode)

womenOld <- read.dta("~/Dropbox/Parliamentary Brawls/ICPSR_24340/DS0001/24340-0001-Data.dta")

womenOld <- womenOld[, 5:64]

moltenWomen <- melt(womenOld, id.vars = "COUNTRYN")

moltenWomen$variable <- as.numeric(gsub("P", "", moltenWomen$variable))
names(moltenWomen) <- c("country", "year", "WomenInParl")
moltenWomen <- moltenWomen[order(moltenWomen$country, moltenWomen$year), ]

moltenWomen$iso2c <- countrycode(moltenWomen$country, origin = "country.name", destination = "iso2c")
moltenWomen <- moltenWomen[, c("iso2c", "year", "WomenInParl")]
moltenWomen <- subset(moltenWomen, year < 1997)
moltenWomen$WomenInParl[moltenWomen$WomenInParl < 0] <- NA

WomenComb <- rbind(women, moltenWomen)
WomenComb <- WomenComb[order(WomenComb$iso2c, WomenComb$year), ]

#### Load main data
leg.raw <- read.csv("/git_repositories/LegislativeViolence/Data/LegViolenceMain.csv")

#### Merge together
leg <- merge(leg.raw, WomenComb, by = c("iso2c", "year"), all.x = TRUE)

write.csv(leg, "/git_repositories/LegislativeViolence/Data/LegViolenceMain.csv", 
          row.names = FALSE)
