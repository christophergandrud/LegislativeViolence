#######################
# Legislative Violence Expected Value Graphs
# Christopher Gandrud
# Updated 27 June 2013
#######################

##require(ggplot2)
require(reshape2)
library(gridExtra)
library(plyr)

## Ranges of fitted values
    dem.r <- seq(from = 0, to = 85, by = 2)
    HighProp.r <- c(0, 1)
    Trust <- seq(from = 1.26, to = 1.97, by = 0.01)

## Age of Democracy
    # Set fitted values  
    DN3.DemAge <-setx(DN3, DemAge = dem.r)

    # Simulate quantities of interest
    DN3.DemSim <- sim(DN3, x = DN3.DemAge)
    
    # Extract expected values from simulations
    DN3.demAge.e <- data.frame(simulation.matrix(DN3.DemSim, "Expected Values: E(Y|X)"))
    DN3.demAge.e <- melt(DN3.demAge.e, measure = 1:43)
    
    # Remove "X" from variable
    DN3.demAge.e$variable <- as.numeric(gsub("X", "", DN3.demAge.e$variable))

    # Convert back to original values
    DN3.demAge.e$variable <- (DN3.demAge.e$variable - 1)*2

    # Remove values outside of the 2.5% and 97.5% quantiles
    # Find 2.5% and 97.5% quantiles for HRCC
    DN3.demAge.ePer <- ddply(DN3.demAge.e, .(variable), transform, 
                             Lower = value < quantile(value, c(0.025)))

    DN3.demAge.ePer <- ddply(DN3.demAge.ePer, .(variable), transform, 
                             Upper = value > quantile(value, c(0.975)))

    # Remove variables outside of the middle 95%
    DN3.demAge.ePer <- subset(DN3.demAge.ePer, Lower == FALSE & Upper == FALSE)
    
    # Plot
    DN3.demAge.p <- ggplot(DN3.demAge.ePer, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth() +
                        scale_x_continuous(breaks = c(0, 21, 51, 85), 
                                           labels = c("0", "20", "50", "85")) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05), 
                                           limits = c(0, 0.05)) +
                        xlab("\nAge of Democracy") + ylab("") +
                        theme_bw(base_size = 12)

## Disporportionality < 6 Dummy
    ## Set fitted values
    DN3.HighProp <- setx(DN3, HighProp = HighProp.r)

    # Simulate quantities of interest
    DN3.HighPropSim <- sim(DN3, x = DN3.HighProp)

    # Extract expected values from simulations
    DN3.HighProp.e <- data.frame(simulation.matrix(DN3.HighPropSim, "Expected Values: E(Y|X)"))
    DN3.HighProp.e <- melt(DN3.HighProp.e, measure = 1:2)

    # Remove "X" from variable
    DN3.HighProp.e$variable <- as.numeric(gsub("X", "", DN3.HighProp.e$variable))

    # Remove values outside of the 2.5% and 97.5% quantiles
    # Find 2.5% and 97.5% quantiles for HRCC
    DN3.HighProp.ePer <- ddply(DN3.HighProp.e, .(variable), transform, 
                         Lower = value < quantile(value, c(0.025)))

    DN3.HighProp.ePer <- ddply(DN3.HighProp.ePer, .(variable), transform, 
                         Upper = value > quantile(value, c(0.975)))

    # Remove variables outside of the middle 95%
    DN3.HighProp.ePer <- subset(DN3.HighProp.ePer, Lower == FALSE & Upper == FALSE)

    # Plot
    DN3.HighProp.p <- ggplot(DN3.HighProp.ePer, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth(method = "lm", se = FALSE) +
                        scale_x_reverse(breaks = c(1, 2), labels = c("Higher", "Very Low")) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05), 
                                           labels = c("", "", ""), limits = c(0, 0.05)) +
                        xlab("\nDisproportionality") + ylab("") +
                        theme_bw(base_size = 12)
    
#### Combibine plots
    predicted.combine <- grid.arrange(DN3.HighProp.p, DN3.demAge.p, ncol = 2, left = "Predicted Probability of Violence in a Year")
    print(predicted.combine)