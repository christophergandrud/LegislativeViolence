require(foreign)
require(ggplot2)

leg <- read.dta("/Users/christophergandrud/Dropbox/Leg_Violence/leg_violence_main.dta")

leg$violence <- factor(leg$violence, label = c("No Violence", "Violence"))
leg <- subset(leg, violence != "NA")
leg <- subset(leg, elect_legislature == 1)

require(gridExtra)
## Label violence variable and remove if violence is missing    
dem.p <- dem
dem.p$violence <- factor(dem.p$violence, label = c("No Violence", "Violence"))
dem.p <- subset(dem.p, violence != "NA")

## Box plot colours
box.col <- c("Violence" = "#ED6700", "No Violence" = "grey80")

## Disproportionality Box Plot
disp.boxp <- ggplot(leg, aes(violence, disproportionality)) +
  geom_jitter(aes(color = violence), alpha = 0.5, show_guide = FALSE) +
  geom_boxplot(aes(fill = violence), show_guide = FALSE) +
  scale_y_log10(breaks = c(1, 2.5, 5, 10, 20, 30), labels = c(1, 2.5, 5, 10, 20, 30)) +
  xlab("") + ylab("Disproportionality (Log Scale)\n") +
  scale_fill_manual(values = box.col, guide = "none") +
  scale_colour_manual(values = box.col, guide = "none") +
  theme_bw()
  
print(disp.boxp)

## Trust Box Plot
#trust.boxp <- ggplot(dem.p, aes(violence, CWtrust)) +
#  geom_jitter(aes(color = violence), alpha = 0.5, show_guide = FALSE) +
#  geom_boxplot(aes(fill = violence), show_guide = FALSE) +
#  xlab("") + ylab("Trust") +
#  scale_fill_manual(values = box.col, guide = "none") +
#  scale_colour_manual(values = box.col, guide = "none") +
#  scale_y_reverse(breaks = c(1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9), labels = c("High", "1.4", "1.5", "1.6", "1.7", "1.8", "Low")) +                      
#  theme_bw()

## Combibine plots
# boxPlot.combine <- grid.arrange(disp.boxp, trust.boxp, ncol = 2)