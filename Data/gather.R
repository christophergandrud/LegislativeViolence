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
    devtools::install_github('leeper/rio')
library(rio)
library(foreign)
library(tidyr)
library(repmis)

#### ----------------- Main Leg. Violence Data ---------------------------- ####
main_violence <- import('data/raw/brawls_BG.csv') %>% select(iso2c, year)
main_violence$violence <- 1

main_violence <- main_violence %>% group_by(iso2c, year) %>%
                    mutate(violence_y_cum = sum(violence))

#### ------------------ World Bank Development Indicators ----------------- ####
indicators_wdi <- c('SI.POV.GINI', 'NY.GDP.PCAP.KD', 'VC.IHR.PSRC.P5')
wdi <- WDI(indicator = indicators_wdi, start = 1975, end = 2014, extra = T) %>%
        dplyr::rename(gini = SI.POV.GINI,
                gdp_per_capita = NY.GDP.PCAP.KD,
                murder_rate = VC.IHR.PSRC.P5) %>%
        filter(region != 'Aggregates') %>%
        select(iso2c, year, gini, gdp_per_capita, murder_rate)

# GDP to thousands of dollars
wdi$gdp_per_capita <- wdi$gdp_per_capita / 1000

wdi <- wdi[!duplicated(wdi[, c('iso2c', 'year')]),]

#### --------------- Women in Parliament ---------------------------------- ####
# From 1997 data from Inter-Parliamentary Union via World Bank Development 
# Indicators 
## WDI indicator ID: SG.GEN.PARL.ZS
women <- WDI(indicator = "SG.GEN.PARL.ZS", start = 1997) %>% 
            select(-country) %>% rename(women_in_parl = SG.GEN.PARL.ZS) %>%
            select(iso2c, year, women_in_parl)

# Data from before 1997 from ICPSR: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/24340
women_old <- read.dta('Data/raw/24340-0001-Data.dta')
women_old<- women_old[, 5:64]

women_old <- gather(women_old, year, women_in_parl, -COUNTRYN)
women_old$year <- women_old$year %>% gsub('P', '', .) %>% as.numeric

women_old$iso2c <- countrycode(women_old$COUNTRYN, origin = 'country.name',
                               destination = 'iso2c')

women_old <- women_old %>% select(iso2c, year, women_in_parl) %>%
                arrange(iso2c, year) %>% filter(year >= 1980) %>% 
                filter(year < 1997)

women <- rbind(women_old, women) %>% arrange(iso2c, year)

women <- women[!duplicated(women[, c('iso2c', 'year')]),]


#### --------------- Age of Democracy ------------------------------------- ####
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
polity <- polity[!duplicated(polity[, c('iso2c', 'year')]),]

#### ---------------------- Database of Political Institutions ------------ ####
tmpfile <- tempfile()
download.file('http://bit.ly/1jZ3nmM', tmpfile)
DpiData <- read.dta(tmpfile)
unlink(tmpfile)

# Correct South Africa
DpiData$countryname <- as.character(DpiData$countryname)
DpiData$countryname[DpiData$countryname == "S. Africa"] <- 'South Africa'
DpiData$iso2c <- countrycode(DpiData$countryname, origin = 'country.name', 
                             destination = 'iso2c') 

dpi <- DpiData %>% select(iso2c, year, maj, system, govfrac, pr, liec)

for (i in names(dpi)) dpi[, i][dpi[, i] == -999] <- NA

# Convert maj from a proportion to a percent
dpi$maj <- dpi$maj * 100

# Create single_party government variable if govfrac == 0
dpi$single_party[!is.na(dpi$govfrac)] <- 0
dpi$single_party[dpi$govfrac == 0] <- 1

#### Only countries with elected legislatures
dpi <- dpi %>% filter(liec > 5)
dpi <- dpi[!duplicated(dpi[, c('iso2c', 'year')]),]

#### ----------------- Dispoportionality ---------------------------------- ####
disprop <- import('http://bit.ly/Ss6zDO', format = 'csv') %>% 
            select(iso2c, year, disproportionality) %>% filter(year >= 1980) %>%
            filter(year <= 2012)

# Create disproportionality threshold variable where 1 < 6
disprop$high_prop <- 0
disprop$high_prop[disprop$disproportionality < 6] <- 1

disprop <- disprop[!duplicated(disprop[, c('iso2c', 'year')]),]

#### ----------------- Legislative Immunity ------------------------------- ####
immunity <- import('Data/raw/fish_k_immunity.csv') %>% select(-year)
immunity <- immunity[!duplicated(immunity[, 'iso2c']),]

#### ----------------- Ethnic Fractionalization---------------------------- ####
ethnic_frac <- 'http://www.anderson.ucla.edu/faculty_pages/romain.wacziarg/downloads/fractionalization.xls' %>%
                source_XlsxData(sheet = 1)
ethnic_frac <- ethnic_frac[3:217, c(1, 4)]
names(ethnic_frac) <- c('country', 'ethnic_alesina')
ethnic_frac$ethnic_alesina[ethnic_frac$ethnic_alesina == '.'] <- NA
ethnic_frac$ethnic_alesina <- ethnic_frac$ethnic_alesina %>% as.character %>%
                              as.numeric

ethnic_frac$iso2c <- countrycode(ethnic_frac$country, origin = 'country.name',
                                 destination = 'iso2c')
ethnic_frac <- select(ethnic_frac, iso2c, ethnic_alesina)
ethnic_frac <- ethnic_frac[!duplicated(ethnic_frac[, 'iso2c']),]

#### ------------------ World Values Survey ------------------------------- ####
wvs <- import('Data/raw/wvs.csv')
wvs <- wvs[!duplicated(wvs[, c('iso2c', 'year')]),]

#### ------------------ Federal ----------- ------------------------------- ####
federal <- import('Data/raw/federal.csv')
federal <- federal[!duplicated(federal[, c('iso2c', 'year')]),]

#### ------------------ Effective No. Parties------------------------------ ####
enpv_enps <- import('Data/raw/enpv_epns.csv')

#### ----------------- Merge together ------------------------------------- ####
## Merge dpi with disproportionality
comb <- merge(dpi, disprop, by = c('iso2c', 'year'), all = T) %>%
        arrange(iso2c, year)

## Merge in legislative violence variables
comb <- merge(main_violence, comb, by = c('iso2c', 'year'), all = T) %>%
        arrange(iso2c, year)

## Merge in polity
comb <- merge(comb, polity, by = c('iso2c', 'year'), all = T) %>%
    arrange(iso2c, year)

## Merge wdi
comb <- merge(comb, wdi, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)

## Merge in women
comb <- merge(comb, women, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)

## Merge in immunity
comb <- merge(comb, immunity, by = c('iso2c'), all.x = T) %>%
    arrange(iso2c, year)

## Merge ethic fractionalisation
comb <- merge(comb, ethnic_frac, by = c('iso2c'), all.x = T) %>%
    arrange(iso2c, year)

## Merge World Values survey and extend
comb <- merge(comb, wvs, by = c('iso2c', 'year'), all.x = T) %>%
            arrange(iso2c, year)
comb <- comb %>% group_by(iso2c) %>% mutate(higher_trust = 
                                        FillDown(Var = higher_trust))
comb <- comb %>% group_by(iso2c) %>% mutate(cw_surv_self_expr = 
                                        FillDown(Var = cw_surv_self_expr)) %>%
            as.data.frame

## Merge federal and extend
comb <- merge(comb, federal, by = c('iso2c', 'year'), all.x = T) %>%
            arrange(iso2c, year)
comb <- comb %>% group_by(iso2c) %>% mutate(federal = 
                                    FillDown(Var = federal)) %>% as.data.frame

## Merge enps/enpv
comb <- merge(comb, enpv_enps, by = c('iso2c', 'year'), all.x = T) %>%
    arrange(iso2c, year)
comb <- comb %>% group_by(iso2c) %>% mutate(enpv = FillDown(Var = enpv))
comb <- comb %>% group_by(iso2c) %>% mutate(enps = FillDown(Var = enps)) %>% 
            as.data.frame

# Fill in disproportionality between election years
comb <- comb %>% group_by(iso2c) %>% 
    mutate(disproportionality = FillDown(Var = disproportionality))
comb <- comb %>% mutate(high_prop = FillDown(Var = high_prop)) %>%
    as.data.frame

## Final clean 
comb <- DropNA(comb, 'iso2c')

violence_sub <- comb %>% filter(!is.na(violence))

# Convert NAs to missing
for (i in c('violence', 'violence_y_cum')) comb[, i][is.na(comb[, i])] <- 0

# Limit to 1980-2012
comb <- comb %>% filter(year >= 1980) %>% filter(year <= 2012)

# Add country names
comb$country <- countrycode(comb$iso2c, origin = 'iso2c', 
                            destination = 'country.name')
comb <- MoveFront(comb, c('country', 'iso2c', 'year'))

#### -------------------- Save -------------------------------------------- ####
export(comb, 'Data/LegislativeViolenceMain.csv')
