############
# Main Analysis 3 for Legislative Violence
# Christopher Gandrud
# 27 June 2013
############

# Load package
library(Zelig)

# Subset complete cases per model 

vars.1 <- c("country", "year", "violence", "DemAge", "HighProp")
vars.2 <- c("country", "year", "violence", "DemAge", "HighProp", "maj", "immunity", "pr", "singleParty")
vars.3 <- c("country", "year", "violence", "DemAge", "HighProp", "maj", "CWsurvSelfExpr", "ethnicAlesina")
vars.4 <- c("country", "year", "violence", "DemAge", "HighProp", "maj", "enps", "federal", "govfrac")
vars.5 <- c("country", "year", "violence", "DemAge", "HighProp", "maj", "gini", "GDPperCapita")
vars.6 <- c("country", "year", "violence", "DemAge", "VeryHighProp", "maj", "gini", "GDPperCapita")

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

###########################
### Sample of countries with elected legislatures
D1 <- zelig(violence ~ HighProp + DemAge, model = "relogit", data = dem.1.c, tau = 69/3347, robust = list(method = "weave"), cite = FALSE)

D2 <- zelig(violence ~ HighProp + DemAge + maj + immunity + pr + singleParty, model = "relogit", data = dem.2.c, tau = 69/3347, robust = list(method = "weave"), cite = FALSE)

D3 <- zelig(violence ~ HighProp + DemAge + maj + CWsurvSelfExpr + ethnicAlesina, model = "relogit", data = dem.3.c, tau = 69/3347, robust = list(method = "weave"), cite = FALSE)

D4 <- zelig(violence ~ HighProp + DemAge + maj + enps + federal + govfrac, model = "relogit", data = dem.4.c, tau = 69/3347, robust = list(method = "weave"), cite = FALSE)

D5 <- zelig(violence ~ HighProp + DemAge + maj + gini + GDPperCapita, model = "relogit", data = dem.5.c, tau = 69/3347, robust = list(method = "weave"), cite = FALSE)

#D6 <- zelig(violence ~ DemAge + VeryHighProp + maj + gini + GDPperCapita, model = "relogit", data = dem.6.c, tau = 69/3347, robust = list(method = "weave"), cite = FALSE)

#D7 <- zelig(violence ~ DemAge*VeryHighProp + maj + gini + GDPperCapita, model = "relogit", data = dem.6.c, tau = 69/3347, robust = list(method = "weave"), cite = FALSE)

###########################
### Sample of countries with elected legislatures from 1990
DN1 <- zelig(violence ~ HighProp + DemAge, model = "relogit", data = dNew.1.c, tau = 61/2631, robust = list(method = "weave"), cite = FALSE)

DN2 <- zelig(violence ~ HighProp + DemAge + maj + immunity + pr + singleParty, model = "relogit", data = dNew.2.c, tau = 61/2631, robust = list(method = "weave"), cite = FALSE)

DN3 <- zelig(violence ~ HighProp + DemAge + maj + CWsurvSelfExpr + ethnicAlesina, model = "relogit", data = dNew.3.c, tau = 61/2631, robust = list(method = "weave"), cite = FALSE)

DN4 <- zelig(violence ~ HighProp + DemAge + maj + enps + federal + govfrac, model = "relogit", data = dNew.4.c, tau = 61/2631, robust = list(method = "weave"), cite = FALSE)

DN5 <- zelig(violence ~ HighProp + DemAge + maj + gini + GDPperCapita, model = "relogit", data = dNew.5.c, tau = 61/2631, robust = list(method = "weave"), cite = FALSE)

#DN6 <- zelig(violence ~ DemAge + VeryHighProp + maj + gini + GDPperCapita, model = "relogit", data = dNew.6.c, tau = 61/2631, robust = list(method = "weave"), cite = FALSE)

#DN7 <- zelig(violence ~ DemAge*VeryHighProp + maj + gini + GDPperCapita, model = "relogit", data = dNew.6.c, tau = 61/2631, robust = list(method = "weave"), cite = FALSE)