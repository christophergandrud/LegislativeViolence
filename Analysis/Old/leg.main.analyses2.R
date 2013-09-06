######################
# Main Rare Logistic Regresssion Analyses for "When Legislators Attack"
# Christopher Gandrud
# Updated 7 May 2012
######################

library(Zelig)

## Subset data sets for complete cases in each analysis
# List of vars in each model
vars.1 <- c("country", "year", "violence", "DemAge")
vars.2 <- c("country", "year", "violence", "maj")
vars.3 <- c("country", "year", "violence", "maj", "UDS")
vars.4 <- c("country", "year", "violence", "DemAge", "immunity")
vars.5 <- c("country", "year", "violence", "DemAge", "pr")
vars.6 <- c("country", "year", "violence", "DemAge", "govfrac", "enps")
vars.7 <- c("country", "year", "violence", "DemAge", "singleParty")
vars.8 <- c("country", "year", "violence", "DemAge", "disproportionality")
vars.9 <- c("country", "year", "violence", "DemAge", "HighProp")
vars.10 <- c("country", "year", "violence", "DemAge", "system", "federal")
vars.11 <- c("country", "year", "violence", "DemAge", "tenshort")
vars.12 <- c("country", "year", "violence", "DemAge", "ethnicAlesina", "CWsurvSelfExpr", "gini", "GDPperCapita")
vars.13 <- c("country", "year", "violence", "CWtrust")
vars.14 <- c("country", "year", "violence", "DemAge", "maj", "HighProp", "tenshort")
vars.15 <- c("country", "year", "violence", "DemAge", "pr", "maj", "HighProp", "tenshort", "gini", "GDPperCapita")
vars.16 <- c("country", "year", "violence", "DemAge", "pr", "maj", "immunity", "govfrac", "enps", "HighProp", "system", "federal", "tenshort", "ethnicAlesina", "CWsurvSelfExpr", "gini", "GDPperCapita", "CWtrust")

leg.1.c <- leg[complete.cases(leg[vars.1]),]
dem.1.c <- dem[complete.cases(dem[vars.1]),]
dNew.1.c <- dNew[complete.cases(dNew[vars.1]),]

leg.2.c <- leg[complete.cases(leg[vars.2]),]
dem.2.c <- dem[complete.cases(dem[vars.2]),]
dNew.2.c <- dNew[complete.cases(dNew[vars.2]),]

#### Constrict Sample to Maj < 90 for robustness check
leg.3.c <- leg[complete.cases(leg[vars.3]),]
leg.3.c <- subset(leg.3.c, maj < 90)
dem.3.c <- dem[complete.cases(dem[vars.3]),]
dem.3.c <- subset(dem.3.c, maj < 90)
dNew.3.c <- dNew[complete.cases(dNew[vars.3]),]
dNew.3.c <- subset(dNew.3.c, maj < 90)

leg.4.c <- leg[complete.cases(leg[vars.4]),]
dem.4.c <- dem[complete.cases(dem[vars.4]),]
dNew.4.c <- dNew[complete.cases(dNew[vars.4]),]

leg.5.c <- leg[complete.cases(leg[vars.5]),]
dem.5.c <- dem[complete.cases(dem[vars.5]),]
dNew.5.c <- dNew[complete.cases(dNew[vars.5]),]

leg.6.c <- leg[complete.cases(leg[vars.6]),]
dem.6.c <- dem[complete.cases(dem[vars.6]),]
dNew.6.c <- dNew[complete.cases(dNew[vars.6]),]

leg.7.c <- leg[complete.cases(leg[vars.7]),]
dem.7.c <- dem[complete.cases(dem[vars.7]),]
dNew.7.c <- dNew[complete.cases(dNew[vars.7]),]

leg.8.c <- leg[complete.cases(leg[vars.8]),]
dem.8.c <- dem[complete.cases(dem[vars.8]),]
dNew.8.c <- dNew[complete.cases(dNew[vars.8]),]

leg.9.c <- leg[complete.cases(leg[vars.9]),]
dem.9.c <- dem[complete.cases(dem[vars.9]),]
dNew.9.c <- dNew[complete.cases(dNew[vars.9]),]

leg.10.c <- leg[complete.cases(leg[vars.10]),]
dem.10.c <- dem[complete.cases(dem[vars.10]),]
dNew.10.c <- dNew[complete.cases(dNew[vars.10]),]

leg.11.c <- leg[complete.cases(leg[vars.11]),]
dem.11.c <- dem[complete.cases(dem[vars.11]),]
dNew.11.c <- dNew[complete.cases(dNew[vars.11]),]

leg.12.c <- leg[complete.cases(leg[vars.12]),]
dem.12.c <- dem[complete.cases(dem[vars.12]),]
dNew.12.c <- dNew[complete.cases(dNew[vars.12]),]

leg.13.c <- leg[complete.cases(leg[vars.13]),]
dem.13.c <- dem[complete.cases(dem[vars.13]),]
dNew.13.c <- dNew[complete.cases(dNew[vars.13]),]

leg.14.c <- leg[complete.cases(leg[vars.14]),]
dem.14.c <- dem[complete.cases(dem[vars.14]),]
dNew.14.c <- dNew[complete.cases(dNew[vars.14]),]

leg.15.c <- leg[complete.cases(leg[vars.15]),]
dem.15.c <- dem[complete.cases(dem[vars.15]),]
dNew.15.c <- dNew[complete.cases(dNew[vars.15]),]

leg.16.c <- leg[complete.cases(leg[vars.16]),]
dem.16.c <- dem[complete.cases(dem[vars.16]),]
dNew.16.c <- dNew[complete.cases(dNew[vars.16]),]

########################## Models #####################################

### Full Sample of countries with legislatures
## Not shown in paper
F1 <- zelig(violence ~ DemAge, model = "relogit", data = leg.1.c, tau = 72/4224, robust = list(method = "weave"))

F2 <- zelig(violence ~ maj, model = "relogit", data = leg.2.c, tau = 72/4224, robust = list(method = "weave"))

F3 <- zelig(violence ~ maj + UDS, model = "relogit", data = leg.3.c, tau = 72/4224, robust = list(method = "weave"))

F4 <- zelig(violence ~ immunity + DemAge, model = "relogit", data = leg.4.c, tau = 72/4224, robust = list(method = "weave"))

F5 <- zelig(violence ~ pr + DemAge, model = "relogit", data = leg.5.c, tau = 72/4224, robust = list(method = "weave"))

F6 <- zelig(violence ~ govfrac + DemAge + enps, model = "relogit", data = leg.6.c, tau = 72/4224, robust = list(method = "weave"))

F7 <- zelig(violence ~ singleParty + DemAge, model = "relogit", data = leg.7.c, tau = 72/4224, robust = list(method = "weave"))

F8 <- zelig(violence ~ disproportionality + DemAge, model = "relogit", data = leg.8.c, tau = 72/4224, robust = list(method = "weave"))

F9 <- zelig(violence ~ HighProp + DemAge, model = "relogit", data = leg.9.c, tau = 72/4224, robust = list(method = "weave"))

F10 <- zelig(violence ~ system + federal + CWsurvSelfExpr + DemAge, model = "relogit", data = leg.10.c, tau = 72/4224, robust = list(method = "weave"))

F11 <- zelig(violence ~ tenshort + DemAge, model = "relogit", data = leg.11.c, tau = 72/4224, robust = list(method = "weave"))

F12 <- zelig(violence ~ DemAge + ethnicAlesina + CWsurvSelfExpr + gini + GDPperCapita, model = "relogit", data = leg.12.c, tau = 72/4224, robust = list(method = "weave"))

F13 <- zelig(violence ~ CWtrust, model = "relogit", data = leg.12.c, tau = 72/4224, robust = list(method = "weave"))

F14 <- zelig(violence ~ DemAge + pr + maj + HighProp + tenshort, model = "relogit", data = leg.14.c, tau = 72/4224, robust = list(method = "weave"))

F15 <- zelig(violence ~ DemAge + pr + maj + HighProp + tenshort + gini + GDPperCapita, model = "relogit", data = leg.15.c, tau = 72/4224, robust = list(method = "weave"))

F16 <- zelig(violence ~ DemAge + pr + maj + immunity + govfrac + enps + HighProp + system + federal + tenshort + ethnicAlesina + CWsurvSelfExpr + gini + GDPperCapita + CWtrust, model = "relogit", data = leg.16.c, tau = 72/4224, robust = list(method = "weave"))

###########################
### Sample of countries with elected legislatures
D1 <- zelig(violence ~ DemAge, model = "relogit", data = dem.1.c, tau = 72/4224, robust = list(method = "weave"))

D2 <- zelig(violence ~ maj, model = "relogit", data = dem.2.c, tau = 72/4224, robust = list(method = "weave"))

D3 <- zelig(violence ~ maj + UDS, model = "relogit", data = dem.3.c, tau = 72/4224, robust = list(method = "weave"))

D4 <- zelig(violence ~ immunity + DemAge, model = "relogit", data = dem.4.c, tau = 72/4224, robust = list(method = "weave"))

D5 <- zelig(violence ~ pr + DemAge, model = "relogit", data = dem.5.c, tau = 72/4224, robust = list(method = "weave"))

D6 <- zelig(violence ~ govfrac + DemAge + enps, model = "relogit", data = dem.6.c, tau = 72/4224, robust = list(method = "weave"))

D7 <- zelig(violence ~ singleParty + DemAge, model = "relogit", data = dem.7.c, tau = 72/4224, robust = list(method = "weave"))

D8 <- zelig(violence ~ disproportionality + DemAge, model = "relogit", data = dem.8.c, tau = 72/4224, robust = list(method = "weave"))

D9 <- zelig(violence ~ HighProp + DemAge, model = "relogit", data = dem.9.c, tau = 72/4224, robust = list(method = "weave"))

D10 <- zelig(violence ~ system + federal + CWsurvSelfExpr + DemAge, model = "relogit", data = dem.10.c, tau = 72/4224, robust = list(method = "weave"))

D11 <- zelig(violence ~ tenshort + DemAge, model = "relogit", data = dem.11.c, tau = 72/4224, robust = list(method = "weave"))

D12 <- zelig(violence ~ DemAge + ethnicAlesina + CWsurvSelfExpr + gini + GDPperCapita, model = "relogit", data = dem.12.c, tau = 72/4224, robust = list(method = "weave"))

D13 <- zelig(violence ~ CWtrust, model = "relogit", data = dem.13.c, tau = 72/4224, robust = list(method = "weave"))

D14 <- zelig(violence ~ DemAge + pr + maj + HighProp + tenshort, model = "relogit", data = dem.14.c, tau = 72/4224, robust = list(method = "weave"))

D15 <- zelig(violence ~ DemAge + pr + maj + HighProp + tenshort + gini + GDPperCapita, model = "relogit", data = dem.15.c, tau = 72/4224, robust = list(method = "weave"))

D16 <- zelig(violence ~ DemAge + pr + maj + immunity + govfrac + enps + HighProp + system + federal + tenshort + ethnicAlesina + CWsurvSelfExpr + gini + GDPperCapita + CWtrust, model = "relogit", data = dem.16.c, tau = 72/4224, robust = list(method = "weave"))

###########################
### Sample of countries with elected legislatures from 1990
DN1 <- zelig(violence ~ DemAge, model = "relogit", data = dNew.1.c, tau = 64/2278, robust = list(method = "weave"))

DN2 <- zelig(violence ~ maj, model = "relogit", data = dNew.2.c, tau = 64/2278, robust = list(method = "weave"))

DN3 <- zelig(violence ~ maj + UDS, model = "relogit", data = dNew.3.c, tau = 64/2278, robust = list(method = "weave"))

DN4 <- zelig(violence ~ immunity + DemAge, model = "relogit", data = dNew.4.c, tau = 64/2278, robust = list(method = "weave"))

DN5 <- zelig(violence ~ pr + DemAge, model = "relogit", data = dNew.5.c, tau = 64/2278, robust = list(method = "weave"))

DN6 <- zelig(violence ~ govfrac + DemAge + enps, model = "relogit", data = dNew.6.c, tau = 64/2278, robust = list(method = "weave"))

DN7 <- zelig(violence ~ singleParty + DemAge, model = "relogit", data = dNew.7.c, tau = 64/2278, robust = list(method = "weave"))

DN8 <- zelig(violence ~ disproportionality + DemAge, model = "relogit", data = dNew.8.c, tau = 64/2278, robust = list(method = "weave"))

DN9 <- zelig(violence ~ HighProp + DemAge, model = "relogit", data = dNew.9.c, tau = 64/2278, robust = list(method = "weave"))

DN10 <- zelig(violence ~ system + federal + CWsurvSelfExpr + DemAge, model = "relogit", data = dNew.10.c, tau = 64/2278, robust = list(method = "weave"))

DN11 <- zelig(violence ~ tenshort + DemAge, model = "relogit", data = dNew.11.c, tau = 64/2278, robust = list(method = "weave"))

DN12 <- zelig(violence ~ DemAge + ethnicAlesina + CWsurvSelfExpr + gini + GDPperCapita, model = "relogit", data = dNew.12.c, tau = 64/2278, robust = list(method = "weave"))

DN13 <- zelig(violence ~ CWtrust, model = "relogit", data = dNew.13.c, tau = 64/2278, robust = list(method = "weave"))

DN14 <- zelig(violence ~ DemAge + pr + maj + HighProp + tenshort, model = "relogit", data = dNew.14.c, tau = 64/2278, robust = list(method = "weave"))

DN15 <- zelig(violence ~ DemAge + pr + maj + HighProp + tenshort + gini + GDPperCapita, model = "relogit", data = dNew.15.c, tau = 64/2278, robust = list(method = "weave"))

DN16 <- zelig(violence ~ DemAge + pr + maj + immunity + govfrac + enps + HighProp + system + federal + tenshort + ethnicAlesina + CWsurvSelfExpr + gini + GDPperCapita + CWtrust, model = "relogit", data = dNew.16.c, tau = 64/2278, robust = list(method = "weave"))
