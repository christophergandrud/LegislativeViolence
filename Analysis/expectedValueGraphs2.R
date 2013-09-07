#######################
# Legislative Violence Expected Value Graphs
# Christopher Gandrud
# Updated 7 September 2013
#######################

##require(ggplot2)
require(reshape2)
library(gridExtra)
library(plyr)

## Ranges of fitted values
    HighProp.r <- c(0, 1)
    dem.r <- seq(from = 0, to = 85, by = 2)
    maj.r <- seq(from = 20, to = 100, by = 2)

## Disporportionality < 5 Dummy
## Set fitted values
DN2.HighProp <- setx(DN2, HighProp = HighProp.r)

# Simulate quantities of interest
DN2.HighPropSim <- sim(DN2, x = DN2.HighProp)

# Extract expected values from simulations
DN2.HighProp.e <- data.frame(simulation.matrix(DN2.HighPropSim, "Expected Values: E(Y|X)"))
DN2.HighProp.e <- melt(DN2.HighProp.e, measure = 1:2)

# Remove "X" from variable
DN2.HighProp.e$variable <- as.numeric(gsub("X", "", DN2.HighProp.e$variable))

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
DN2.HighProp.ePer <- ddply(DN2.HighProp.e, .(variable), transform, 
                           Lower = value < quantile(value, c(0.025)))

DN2.HighProp.ePer <- ddply(DN2.HighProp.ePer, .(variable), transform, 
                           Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
DN2.HighProp.ePer <- subset(DN2.HighProp.ePer, Lower == FALSE & Upper == FALSE)

# Plot
DN2.HighProp.p <- ggplot(DN2.HighProp.ePer, aes(variable, value)) +
  geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
  stat_smooth(method = "lm", se = FALSE) +
  scale_x_reverse(breaks = c(1, 2), labels = c("Above", "Below Median")) +
  scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1), 
                     labels = c("0", "0.02", "0.05", "0.1"),
                     limits = c(0, 0.11)) +
  xlab("\nDisproportionality") + ylab("") +
  theme_bw(base_size = 12)

## Age of Democracy
    # Set fitted values  
    DN2.DemAge <-setx(DN2, DemAge = dem.r)

    # Simulate quantities of interest
    DN2.DemSim <- sim(DN2, x = DN2.DemAge)
    
    # Extract expected values from simulations
    DN2.demAge.e <- data.frame(simulation.matrix(DN2.DemSim, "Expected Values: E(Y|X)"))
    DN2.demAge.e <- melt(DN2.demAge.e, measure = 1:43)
    
    # Remove "X" from variable
    DN2.demAge.e$variable <- as.numeric(gsub("X", "", DN2.demAge.e$variable))

    # Convert back to original values
    DN2.demAge.e$variable <- (DN2.demAge.e$variable - 1)*2

    # Remove values outside of the 2.5% and 97.5% quantiles
    # Find 2.5% and 97.5% quantiles for HRCC
    DN2.demAge.ePer <- ddply(DN2.demAge.e, .(variable), transform, 
                             Lower = value < quantile(value, c(0.025)))

    DN2.demAge.ePer <- ddply(DN2.demAge.ePer, .(variable), transform, 
                             Upper = value > quantile(value, c(0.975)))

    # Remove variables outside of the middle 95%
    DN2.demAge.ePer <- subset(DN2.demAge.ePer, Lower == FALSE & Upper == FALSE)
    
    # Plot
    DN2.demAge.p <- ggplot(DN2.demAge.ePer, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth() +
                        scale_x_continuous(breaks = c(0, 21, 51, 85), 
                                           labels = c("0", "20", "50", "85")) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1), 
                                           labels = c("", "", "", ""), 
                                           limits = c(0, 0.11)) +
                        xlab("\nAge of Democracy") + ylab("") +
                        theme_bw(base_size = 12)
## Majority
    # Set fitted values
    DN2.maj1 <-setx(DN2, maj = maj.r)

    # Simulate quantities of interest
    DN2.majSim <- sim(DN2, x = DN2.maj1)
    
    # Extract expected values from simulations
    DN2.maj.e <- data.frame(simulation.matrix(DN2.majSim, "Expected Values: E(Y|X)"))
    DN2.maj.e <- melt(DN2.maj.e, measure = 1:41)
    
    # Remove "X" from variable
    DN2.maj.e$variable <- as.numeric(gsub("X", "", DN2.maj.e$variable))

    # Remove values outside of the 2.5% and 97.5% quantiles
    # Find 2.5% and 97.5% quantiles for HRCC
    DN2.maj.ePer <- ddply(DN2.maj.e, .(variable), transform, 
                         Lower = value < quantile(value, c(0.025)))

    DN2.maj.ePer <- ddply(DN2.maj.ePer, .(variable), transform, 
                         Upper = value > quantile(value, c(0.975)))

    # Remove variables outside of the middle 95%
    DN2.maj.ePer <- subset(DN2.maj.ePer, Lower == FALSE & Upper == FALSE)
    
    # Plot
    DN2.maj.p <- ggplot(DN2.maj.ePer, aes(variable, value)) +
                        geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
                        stat_smooth(se = FALSE) +
                        scale_x_continuous(breaks = c(1, 11, 21, 31, 41),
                                           labels = c(20, 40, 60, 80, 100)) +
                        scale_y_continuous(breaks = c(0, 0.02, 0.05, 0.1), 
                                          labels = c("", "", "", ""), 
                                          limits = c(0, 0.11)) +
                        xlab("\nGovernment Majority") + ylab("") +
                        theme_bw()
    
#### Combibine plots
    predicted.combine <- grid.arrange(DN2.HighProp.p, DN2.demAge.p, DN2.maj.p, ncol = 3, left = "Predicted Probability of Violence in a Year")
    print(predicted.combine)