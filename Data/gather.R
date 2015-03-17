# ---------------------------------------------------------------------------- #
# Gather/Clean/Merge Data for Two Sword Lengths Apart
# Christopher Gandrud
# 17 March 2015
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


#### --------------- Age of Democracy ---------------------------------------- #
polity <- PolityGet(url = 'http://www.systemicpeace.org/inscr/p4v2013.sav') %>%
            arrange(iso2c, year)
# Create democratic age variable
polity$democracy <- 0
polity$democracy[polity$polity2 > 5] <- 1

cum_nozero <- function(data, x){
    data$cum <- 0
    for (i in 1:nrow(data)){
        if (i == 1) data[1, 'cum'] <- data[1, x]
        else if (i > 1) {
            if (data[i-1, x] == 0 & data[i, x] > 0) data[i, 'cum'] <- 1
            else if (data[i-1, x] > 0) data[i, 'cum'] <- data[i-1, 'cum'] + 1
        }
    }
    return(data)
}

test = cum_nozero(polity, x = 'democracy')

polity <- polity %>% select(iso2c, year, polity2, durable)

