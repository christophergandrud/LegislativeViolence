############
# Main Analysis 4 for Legislative Violence
# Christopher Gandrud
# 18 March 2015
############

# Set working directory. Change as needed.
setwd('/git_repositories/LegislativeViolence/')

# Load package
if (!('rio' %in% installed.packages()[, 1]))
    devtools::install_github('leeper/rio')
library(rio)
library(Zelig)

# Load data
dem <- import('Data/LegislativeViolenceMain.csv')
dNew <- subset(dem, year > 1989)

# tau's
tau_dem <- sum(dem$violence) / nrow(dem)
tau_dNew <- sum(dNew$violence) / nrow(dNew)

# Subset complete cases per model

vars.1 <- c("country", "year", "violence", "dem_age", "high_prop")
vars.2 <- c("country", "year", "violence", "dem_age", "high_prop", "maj",
            "immunity", "pr", "single_party")
vars.3 <- c("country", "year", "violence", "dem_age", "high_prop", "maj",
            "cw_surv_self_expr", "ethnic_alesina")
vars.4 <- c("country", "year", "violence", "dem_age", "high_prop", "maj",
            "women_in_parl")
vars.5 <- c("country", "year", "violence", "dem_age", "high_prop",
            "murder_rate")
vars.6 <- c("country", "year", "violence", "dem_age", "high_prop", "maj",
            "federal", "govfrac")
vars.7 <- c("country", "year", "violence", "dem_age", "high_prop", "maj",
            "enps", "federal")
vars.8 <- c("country", "year", "violence", "dem_age", "high_prop", "maj",
            "gdp_per_capita")

dem.1.c <- dem[complete.cases(dem[vars.1]),]
dNew.1.c <- dNew[complete.cases(dNew[vars.1]),]

dem.2.c <- dem[complete.cases(dem[vars.2]),]
dNew.2.c <- dNew[complete.cases(dNew[vars.2]),]

dem.3.c <- dem[complete.cases(dem[vars.3]),]
dNew.3.c <- dNew[complete.cases(dNew[vars.3]),]

dem.4.c <- dem[complete.cases(dem[vars.4]),]
dNew.4.c <- dNew[complete.cases(dNew[vars.4]),]

dem.5.c <- dem[complete.cases(dem[vars.5]),]
dNew.5.c <- dNew[complete.cases(dNew[vars.5]),]

dem.6.c <- dem[complete.cases(dem[vars.6]),]
dNew.6.c <- dNew[complete.cases(dNew[vars.6]),]

dem.7.c <- dem[complete.cases(dem[vars.7]),]
dNew.7.c <- dNew[complete.cases(dNew[vars.7]),]

dem.8.c <- dem[complete.cases(dem[vars.8]),]
dNew.8.c <- dNew[complete.cases(dNew[vars.8]),]

###########################
### Sample of countries with elected legislatures
D1 <- zelig(violence ~ high_prop + dem_age, model = "relogit", data = dem.1.c,
            tau = tau_dem, robust = list(method = "weave"), cite = FALSE)

D2 <- zelig(violence ~ high_prop + dem_age + maj + immunity + pr + single_party,
            model = "relogit", data = dem.2.c, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D3 <- zelig(violence ~ high_prop + dem_age + maj + cw_surv_self_expr +
                ethnic_alesina, model = "relogit", data = dem.3.c, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D4 <- zelig(violence ~ high_prop + dem_age + maj + women_in_parl,
            model = "relogit", data = dem.4.c, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D5 <- zelig(violence ~ high_prop + dem_age + maj + murder_rate,
            model = "relogit", data = dem.5.c, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D6 <- zelig(violence ~ high_prop + dem_age + maj + federal + govfrac,
            model = "relogit", data = dem.6.c, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D7 <- zelig(violence ~ high_prop + dem_age + maj + enps + federal,
            model = "relogit", data = dem.7.c, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

D8 <- zelig(violence ~ high_prop + dem_age + maj + gdp_per_capita,
            model = "relogit", data = dem.7.c, tau = tau_dem,
            robust = list(method = "weave"), cite = FALSE)

###########################
### Sample of countries with elected legislatures from 1990
DN1 <- zelig(violence ~ high_prop + dem_age, model = "relogit", data = dNew.1.c,
             tau = tau_dNew, robust = list(method = "weave"), cite = FALSE)

DN2 <- zelig(violence ~ high_prop + dem_age + maj + immunity + pr +
            single_party,
             model = "relogit", data = dNew.2.c, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN3 <- zelig(violence ~ high_prop + dem_age + maj + cw_surv_self_expr +
                 ethnic_alesina, model = "relogit", data = dNew.3.c,
             tau = tau_dem, robust = list(method = "weave"), cite = FALSE)

DN4 <- zelig(violence ~ high_prop + dem_age + maj + women_in_parl,
             model = "relogit", data = dNew.4.c, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN5 <- zelig(violence ~ high_prop + dem_age + maj + murder_rate,
             model = "relogit", data = dNew.5.c, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)

DN6 <- zelig(violence ~ high_prop + dem_age + maj + federal + govfrac,
             model = "relogit", data = dNew.6.c, tau = 69/3347,
             robust = list(method = "weave"), cite = FALSE)

DN7 <- zelig(violence ~ high_prop + dem_age + maj + enps + federal,
             model = "relogit", data = dNew.7.c, tau = 69/3347,
             robust = list(method = "weave"), cite = FALSE)

DN8 <- zelig(violence ~ high_prop + dem_age + maj + gdp_per_capita,
             model = "relogit", data = dNew.7.c, tau = tau_dem,
             robust = list(method = "weave"), cite = FALSE)