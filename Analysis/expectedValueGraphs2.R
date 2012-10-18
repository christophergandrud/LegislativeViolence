#######################
# Legislative Violence Expected Value Graphs
# Christopher Gandrud
# Updated 18 October 2012
#######################

##require(ggplot2)
require(reshape2)
library(gridExtra)

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
    D15.demAge.e <- (D15.DemSim$qi)
    D15.demAge.e <- data.frame(D15.demAge.e$ev)
    D15.demAge.e <- melt(D15.demAge.e, measure = 1:86)
    
    # Remove "X" from variable
    D15.demAge.e$variable <- as.numeric(gsub("X", "", D15.demAge.e$variable))
    
    # Plot
    D15.demAge.p <- ggplot(D15.demAge.e, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth() +
                        scale_x_continuous(breaks = c(0, 21, 51, 85), labels = c("0", "20", "50", "85")) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1, 0.15), limits = c(0, 0.15)) +
                        xlab("\nAge of Democracy") + ylab("") +
                        theme_bw(base_size = 12)

## Disporportionality < 5 Dummy
    ## Set fitted values
    D15.HighProp <- setx(D15, HighProp = HighProp.r)

    # Simulate quantities of interest
    D15.HighPropSim <- sim(D15, x = D15.HighProp)

    # Extract expected values from simulations
    D15.HighProp.e <- (D15.HighPropSim$qi)
    D15.HighProp.e <- data.frame(D15.HighProp.e$ev)
    D15.HighProp.e <- melt(D15.HighProp.e, measure = 1:2)

    # Remove "X" from variable
    D15.HighProp.e$variable <- as.numeric(gsub("X", "", D15.HighProp.e$variable))

    # Plot
    D15.HighProp.p <- ggplot(D15.HighProp.e, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth(method = "lm", se = FALSE) +
                        scale_x_reverse(breaks = c(1, 2), labels = c("Higher", "Very Low")) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1, 0.15), labels = c("", "", "", "", ""), limits = c(0, 0.15)) +
                        xlab("\nDisproportionality") + ylab("") +
                        theme_bw(base_size = 12)
## Majority
    # Set fitted values
    D15.maj1 <-setx(D15, maj = maj.r)

    # Simulate quantities of interest
    D15.majSim <- sim(D15, x = D15.maj1)
    
    # Extract expected values from simulations
    D15.maj.e <- (D15.majSim$qi)
    D15.maj.e <- data.frame(D15.maj.e$ev)
    D15.maj.e <- melt(D15.maj.e, measure = 1:81)
    
    # Remove "X" from variable
    D15.maj.e$variable <- as.numeric(gsub("X", "", D15.maj.e$variable))
    
    # Put in terms of the original variable percentage
    D15.maj.e$variable = D15.maj.e$variable + 19
    
    # Plot
    D15.maj.p <- ggplot(D15.maj.e, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth(method = "lm", se = FALSE) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1, 0.15), labels = c("", "", "", "", ""), limits = c(0, 0.15)) +
                        xlab("\nGovernment Majority") + ylab("") +
                        theme_bw()
    
#### Combibine plots
    predicted.combine <- grid.arrange(D15.demAge.p, D15.HighProp.p, D15.maj.p, ncol = 3, left = "Predicted Probability of Violence")
    print(predicted.combine)