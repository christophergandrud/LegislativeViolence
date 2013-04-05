#######################
# Legislative Violence Expected Value Graphs
# Christopher Gandrud
# Updated 18 October 2012
#######################

##require(ggplot2)
require(reshape2)
library(gridExtra)
library(plyr)

## Ranges of fitted values
    HighProp.r <- c(0, 1)
    dem.r <- 0:85
    maj.r <- c(20:100)

## Age of Democracy
    # Set fitted values  
    D15.DemAge <-setx(D15, DemAge = dem.r)

    # Simulate quantities of interest
    D15.DemSim <- sim(D15, x = D15.DemAge)
    
    # Extract expected values from simulations
    D15.demAge.e <- data.frame(simulation.matrix(D15.DemSim, "Expected Values: E(Y|X)"))
    D15.demAge.e <- melt(D15.demAge.e, measure = 1:86)
    
    # Remove "X" from variable
    D15.demAge.e$variable <- as.numeric(gsub("X", "", D15.demAge.e$variable))

    # Remove values outside of the 2.5% and 97.5% quantiles
    # Find 2.5% and 97.5% quantiles for HRCC
    D15.demAge.ePer <- ddply(D15.demAge.e, .(variable), transform, 
                             Lower = value < quantile(value, c(0.025)))

    D15.demAge.ePer <- ddply(D15.demAge.ePer, .(variable), transform, 
                             Upper = value > quantile(value, c(0.975)))

    # Remove variables outside of the middle 95%
    D15.demAge.ePer <- subset(D15.demAge.ePer, Lower == FALSE & Upper == FALSE)
    
    # Plot
    D15.demAge.p <- ggplot(D15.demAge.ePer, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth() +
                        scale_x_continuous(breaks = c(0, 21, 51, 85), 
                                           labels = c("0", "20", "50", "85")) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1, 0.15), 
                                           limits = c(0, 0.15)) +
                        xlab("\nAge of Democracy") + ylab("") +
                        theme_bw(base_size = 12)

## Disporportionality < 5 Dummy
    ## Set fitted values
    D15.HighProp <- setx(D15, HighProp = HighProp.r)

    # Simulate quantities of interest
    D15.HighPropSim <- sim(D15, x = D15.HighProp)

    # Extract expected values from simulations
    D15.HighProp.e <- data.frame(simulation.matrix(D15.HighPropSim, "Expected Values: E(Y|X)"))
    D15.HighProp.e <- melt(D15.HighProp.e, measure = 1:2)

    # Remove "X" from variable
    D15.HighProp.e$variable <- as.numeric(gsub("X", "", D15.HighProp.e$variable))

    # Remove values outside of the 2.5% and 97.5% quantiles
    # Find 2.5% and 97.5% quantiles for HRCC
    D15.HighProp.ePer <- ddply(D15.HighProp.e, .(variable), transform, 
                         Lower = value < quantile(value, c(0.025)))

    D15.HighProp.ePer <- ddply(D15.HighProp.ePer, .(variable), transform, 
                         Upper = value > quantile(value, c(0.975)))

    # Remove variables outside of the middle 95%
    D15.HighProp.ePer <- subset(D15.HighProp.ePer, Lower == FALSE & Upper == FALSE)

    # Plot
    D15.HighProp.p <- ggplot(D15.HighProp.ePer, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth(method = "lm", se = FALSE) +
                        scale_x_reverse(breaks = c(1, 2), labels = c("Higher", "Very Low")) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1, 0.15), 
                                           labels = c("", "", "", "", ""), limits = c(0, 0.15)) +
                        xlab("\nDisproportionality") + ylab("") +
                        theme_bw(base_size = 12)
## Majority
    # Set fitted values
    D15.maj1 <-setx(D15, maj = maj.r)

    # Simulate quantities of interest
    D15.majSim <- sim(D15, x = D15.maj1)
    
    # Extract expected values from simulations
    D15.maj.e <- data.frame(simulation.matrix(D15.majSim, "Expected Values: E(Y|X)"))
    D15.maj.e <- melt(D15.maj.e, measure = 1:81)
    
    # Remove "X" from variable
    D15.maj.e$variable <- as.numeric(gsub("X", "", D15.maj.e$variable))
    
    # Put in terms of the original variable percentage
    D15.maj.e$variable = D15.maj.e$variable + 19

    # Remove values outside of the 2.5% and 97.5% quantiles
    # Find 2.5% and 97.5% quantiles for HRCC
    D15.maj.ePer <- ddply(D15.maj.e, .(variable), transform, 
                         Lower = value < quantile(value, c(0.025)))

    D15.maj.ePer <- ddply(D15.maj.ePer, .(variable), transform, 
                         Upper = value > quantile(value, c(0.975)))

    # Remove variables outside of the middle 95%
    D15.maj.ePer <- subset(D15.maj.ePer, Lower == FALSE & Upper == FALSE)
    
    # Plot
    D15.maj.p <- ggplot(D15.maj.ePer, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth(method = "lm", se = FALSE) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1, 0.15), 
                                           labels = c("", "", "", "", ""), 
                                           limits = c(0, 0.15)) +
                        xlab("\nGovernment Majority") + ylab("") +
                        theme_bw()
    
#### Combibine plots
    predicted.combine <- grid.arrange(D15.demAge.p, D15.HighProp.p, D15.maj.p, ncol = 3, left = "Predicted Probability of Violence in a Year")
    print(predicted.combine)