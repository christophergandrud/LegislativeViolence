################
# Explore legislative violence intensity (V1)
# Christopher Gandrud
# 11 March 2014
################

# Load packages
library(repmis)
library(psData)
library(DataCombine)
library(foreign)
library(countrycode)

# Set working directory
setwd('/git_repositories/LegislativeViolence/VioParties/')

#### Load and clean basic violence data ####
Base <- read.csv(file = 'data/LegislativeViolenceMainDataVersion2.csv', stringsAsFactors = FALSE, na.strings = c('NA', ''))

Base <- CountryID(Base, standardCountryName = T) # reminder in future to address Alabama
Base <- DropNA(Base, 'country')
Base$ID <- 1:nrow(Base)
Base <- MoveFront(Base, 'ID')

# Clean
Replace1 <- data.frame(from = c('2--4', '4--8'), to = c('3', '6')) # take median
Base <- FindReplace(Base, Var = 'No.start', replaceData = Replace1)
Base$No.start <- as.numeric(Base$No.start)

Replace2 <- data.frame(from = c('30+'), to = c('31')) 
Base <- FindReplace(Base, Var = 'No.join', replaceData = Replace2)
Base$No.join <- as.numeric(Base$No.join)

# Combine video and print (Criteria use print only when video is unavailable)
Second <- Base[, c('iso2c', 'year', 'R.involved')]
Second <- DropNA(Second, 'R.involved')
Base$Total <- as.character(Base$No.atPeak)
Base <- FillIn(Base, Second, Var1 = 'Total', Var2 = 'R.involved', allow.cartesian = TRUE)

Replace3 <- data.frame(from = c('10\\+', "\"Several\"", "-several", "17\\+", "3\\+", "12\\+", "Several", "~20", "~24",
                                "~5", "\"Multiple\"", "2\\+", "Multiple", "3 to 10", "\"Dozens\"", "\"Lawmakers\"",
                                "~12", "5 to 10", "~40", "70\\+", "Several ", '5\\+'), 
                       to = c('11', '5', '5', '18', '4', '13', '5', '20', '24', '5', '5', '3', '5', '6.5',
                              '24', '5', '12', '7.5', '40', '71', '5', '5'))
Base <- FindReplace(Base, Var = 'Total', replaceData = Replace3)
Base$Total <- as.numeric(Base$Total)
Base <- Base[!duplicated(Base$ID),]


#### Load democratic age data ####
Age <- read.dta('/Applications/CJ\'s Old Folders/PhD/Research/General Data/Polity_IV/democracy_age2012.dta')
Age$iso2c <- countrycode(Age$country, origin = 'country.name', destination = 'iso2c')
Age <- VarDrop(Age, 'country')

#### Load and clean Quality of Government data ####
## Main variables from Johnson & Wallack
URL <- 'http://www.qogdata.pol.gu.se/data/qog_std_ts_20dec13.csv'
FullQoG <- source_data(url = URL, cache = TRUE, sep = ';')

SubQoG <- FullQoG[, c('cname', 'year', 'jw_persr', 'jw_domr')]
SubQoG <- CountryID(SubQoG, countryVar = 'cname', timeVar = 'year', duplicates = 'drop')
SubQoG <- subset(SubQoG, year >= 1980)

SubQoGSub <- SubQoG[SubQoG$iso2c %in% unique(Base$iso2c), ]

SubQ3 <- subset(SubQoGSub, year > 2005)

Vars = c('jw_persr', 'jw_domr')

for (i in Vars){
  Sub <- subset(SubQoGSub, year == 2005)
  Sub <- Sub[, c('iso2c', i)]
  Sub <- DropNA(Sub, i)
  
  SubQ3 <- FillIn(SubQ3, Sub, Var1 = i, Var2 = i, 
               KeyVar = 'iso2c', allow.cartesian = TRUE)
}

SubQoG2005 <- subset(SubQoGSub, year <= 2005)
Spread <- rbind(SubQoG2005, SubQ3, all = TRUE)
Spread <- Spread[, -2]

# Merge with violence data
Comb <- merge(Spread, Age, by = c('iso2c', 'year'), all.y = TRUE)
Comb <- merge(Comb, Base, by = c('iso2c', 'year'), all.y = TRUE)
Comb <- MoveFront(Comb, 'country')

rmExcept('Comb')

Comb <- Comb[, -ncol(Comb)]
write.csv(Comb, file = '~/Dropbox/Parliamentary Brawls/DataEarlyMarch.csv', row.names = F)


#### Ploting ####
library(ggplot2)
library(gridExtra)

# Dominant personalistic vote incentives
PD1 <- ggplot(Comb, aes(jw_domr, No.start)) + geom_jitter() + theme_bw()
PD2 <- ggplot(Comb, aes(jw_domr, y = No.join)) + geom_jitter() + theme_bw()
PD3 <- ggplot(Comb, aes(jw_domr, y = No.atPeak)) + geom_jitter() + theme_bw()
PD4 <- ggplot(Comb, aes(jw_domr, y = PeakLength.sec)) + geom_jitter() + theme_bw()

grid.arrange(PD1, PD2, PD3, PD4)

# Democratic Age
CombSub1 <- subset(Comb, dem_age > 5)
CombSub2 <- subset(Comb, dem_age > 10)

PA1 <- ggplot(Comb, aes(jw_domr, y = Total, label = iso2c)) + geom_jitter() + 
  geom_text(angle = 30, vjust = -1) +
  ggtitle('All') + theme_bw()
PA2 <- ggplot(CombSub1, aes(jw_domr, y = Total, label = iso2c)) + geom_jitter() + 
  geom_text(angle = 30, vjust = -1) +
  ggtitle('Dem > 5') + theme_bw()
PA3 <- ggplot(CombSub2, aes(jw_domr, y = Total, label = iso2c)) + geom_jitter() + 
  geom_text(angle = 30, vjust = -1) +
  ggtitle('Dem > 10') + theme_bw()

grid.arrange(PA1, PA2, PA3)

