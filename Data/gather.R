# ---------------------------------------------------------------------------- #
# Gather/Clean/Merge Data for Two Sword Lengths Apart
# Christopher Gandrud
# 18 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Set working directory. Change as needed.
setwd('/git_repositories/LegislativeViolence/')

# Load packages
library(dplyr)
library(WDI)
library(countrycode)
library(psData)
library(DataCombine)
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio', ref = 'fread')
library(rio)

#### ----------------- Main Leg. Violence Data ------------------------------- #
main_violence <- import('data/raw/brawls_BG.csv') %>% select(iso2c, year)
main_violence$violence <- 1

main_violence <- main_violence %>% group_by(iso2c, year) %>%
                    mutate(violence_y_cum = sum(violence))

#### ------------------ World Bank Development Indicators -------------------- #
indicators_wdi <- c('SI.POV.GINI', 'NY.GDP.PCAP.KD', 'VC.IHR.PSRC.P5')
wdi <- WDI(indicator = indicators_wdi, start = 1975, end = 2014, extra = T) %>%
        dplyr::rename(gini = SI.POV.GINI,
                gdp_per_capita = NY.GDP.PCAP.KD,
                murder_rate = VC.IHR.PSRC.P5) %>%
        filter(region != 'Aggregates') %>%
        select(iso2c, year, gini, gdp_per_capita, murder_rate)

# GDP to thousands of dollars
wdi$gdp_per_capita <- wdi$gdp_per_capita / 1000

# Assume constant gini
wdi <- wdi %>% arrange(iso2c, year) %>% group_by(iso2c) %>%
        mutate(gini = FillDown(Var = gini)) %>%
        as.data.frame


#### --------------- Age of Democracy ---------------------------------------- #
polity <- PolityGet(url = 'http://www.systemicpeace.org/inscr/p4v2013.sav') %>%
            arrange(iso2c, year)

# Create democratic age variable
polity$democracy <- 0
polity$democracy[polity$polity2 > 5] <- 1

cum_nozero <- function(x){
    polity <- as.data.frame(polity)
    comb <- vector()
    for (u in unique(polity$iso2c)){
        temp <- subset(polity, iso2c == u)
        temp_cum <- rep(0, nrow(temp))
    
        for (i in 1:nrow(temp)){
            if (i == 1) temp_cum[1] <- temp[1, x]
            else if (i > 1) {
                if (temp[i-1, x] == 0 & temp[i, x] > 0) {
                    temp_cum[i] <- 1
                }
                else if (temp[i-1, x] > 0){
                     temp_cum[i] <- temp_cum[i-1] + 1
                }
            }
        }
        comb <- c(comb, temp_cum)
    }
    return(comb)
}

polity$dem_age <- cum_nozero(x = 'democracy')

polity <- polity %>% select(iso2c, year, polity2, dem_age, durable)

#### ---------------------- Database of Political Institutions --------------- #
dpi <- DpiGet() %>% select(iso2c, year, maj, govfrac, allhouse, pr, liec)

for (i in names(dpi)) dpi[, i][dpi[, i] == -999] <- NA

# Convert maj from a proportion to a percent
dpi$maj <- dpi$maj * 100

#### ----------------- Dispoportionality ------------------------------------- #
disprop <- import('http://bit.ly/Ss6zDO', format = 'csv') %>% 
            select(iso2c, year, disproportionality) %>% filter(year >= 1980) %>%
            filter(year <- 2012)

# Create disproportionality threshold variable where 1 < 6
disprop$high_prop <- 0
disprop$high_prop[disprop$disproportionality > 6] <- 1

#### Only countries with elected legislatures
dpi <- dpi %>% filter(liec > 5)

#### ----------------- Merge together ---------------------------------------- #
## Merge dpi with disproportionality
comb <- merge(dpi, disprop, by = c('iso2c', 'year'), all = T) %>%
        arrange(iso2c, year)

# Fill in disproportionality between election years
comb <- comb %>% group_by(iso2c) %>% 
            mutate(disproportionality = FillDown(Var = disproportionality))
comb <- comb %>% mutate(high_prop = FillDown(Var = high_prop)) %>%
            as.data.frame

## Merge in legislative violence variables
comb <- merge(main_violence, comb, by = c('iso2c', 'year'), all = T) %>%
        arrange(iso2c, year)

# Convert NAs to missing
for (i in c('violence', 'violence_y_cum')) comb[, i][is.na(comb[, i])] <- 0

## Merge in polity
comb <- merge(comb, polity, by = c('iso2c', 'year'), all = T) %>%
    arrange(iso2c, year)

## Merge wdi
comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)
