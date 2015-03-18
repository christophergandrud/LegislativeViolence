############
# Robustness test with negative binomial model
# Christopher Gandrud
# 18 March 2015
############

# Load package
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio', ref = 'fread')
library(rio)
library(Zelig)

# Load data
dem <- import('/git_repositories/LegislativeViolence/Data/LegislativeViolenceMain.csv')
dNew <- subset(dem, year > 1989)

#### Estimate Models ####
D1_nb <- zelig(violence ~ high_prop + dem_age, model = "negbinom", 
               data = dem.1.c, robust = list(method = "weave"), cite = FALSE)

DN1_nb <- zelig(violence ~ high_prop + dem_age, model = "negbinom", 
             data = dNew.1.c, robust = list(method = "weave"), cite = FALSE)
