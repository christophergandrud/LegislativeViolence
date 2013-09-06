######################
# Main Rare Logistic Regresssion Analyses for "Two Sword Lengths"
# Christopher Gandrud
# Updated 4 November 2012
######################

# Load Libraries
library(RCurl)
library(MatchIt)
library(Zelig)

## Load data
URLMain <- "https://raw.github.com/christophergandrud/LegislativeViolence/master/Data/LegViolenceMain.csv"
Main <- getURL(URLMain)
leg.raw <- read.csv(textConnection(Main))

## Keep variables of interest
vars <- c("country", "year", "violence", "system", "DemAge", "maj", "MajCat", "govfrac", "singleParty", "pr", "tenshort", "UDS", "polity2", "ethnicAlesina", "CWtrust", "higherTrust", "CWsurvSelfExpr", "legislature", "elect_legislature", "disproportionality", "gini", "GDPperCapita", "enps", "enpv", "federal", "immunity")
leg.raw <- leg.raw[vars]

## Label MajCat factor categories
leg.raw$MajCat <- factor(leg.raw$MajCat, labels = c("Minority", "Regular Maj.", "Strong Maj."))

## Transform GDP/capita to thousands of USD
leg.raw$GDPperCapita <- leg.raw$GDPperCapita/1000 

## Create disproportionality threshold variable where 1 < 5
leg.raw$HighProp[leg.raw$disproportionality < 5] <- 1
leg.raw$HighProp[leg.raw$disproportionality >= 5] <- 0

## Transform majority variable from a proportion to a percentage
leg.raw$maj = leg.raw$maj*100

## Only Countries with Legislatures
leg <- subset(leg.raw, legislature == 1)

## Only Countries with Elected Legislatures
dem <- subset(leg.raw, elect_legislature == 1)

## Only Countries with Elected Legislatures after 1989 (new sample 199)
dNew <- subset(dem, year > 1989)

## Analyses
## Run the main analyses used for predicting quantities of interest and estimating regression coefficients.
DemMatched <- matchit(HighProp ~ )


M1 <- zelig(violence ~ DemAge + HighProp + tag(HighProp | country), data = dem, model = "logit.mixed")