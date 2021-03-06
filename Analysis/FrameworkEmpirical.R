############
# Legislative Violence
# Combined scatter plots to illustrate how framework relates to findings
# Christopher Gandrud
# Updated 18 October 2012
############


## Create scatterplot of DemAge, Disproportionality, and Violence
    age.disp.scatter <- qplot(dem_age, disproportionality, position = position_jitter(w = 10), color = factor(violence_y_cum), data = leg.cumulative) +
                        facet_grid(.~violence) +
                        scale_y_log10(breaks = c(1, 2.5, 5, 10, 20, 30), labels = c(1, 2.5,  5, 10, 20, 30)) +
                        scale_x_continuous(breaks = c(0, 50, 100, 150, 200), labels = c("", "", "", "", "")) +
                        xlab(" ") +
                        ylab("Disproportionality (log)\n") +
                        theme_bw(base_size = 12) +
                        scale_colour_manual(values = cols, name = "Brawl/Year")         

## Create scatterplot of DemAge, Government Majority, and Violence
    age.maj.scatter <- qplot(dem_age, maj, position = position_jitter(w = 10), color = factor(violence_y_cum), data = leg.cumulative) +
                        facet_grid(.~violence) +
                        scale_y_reverse() +
                        xlab("\n Age of Democracy") +
                        ylab("Government Majoritty\n") +
                        theme_bw(base_size = 12) +
                        scale_colour_manual(values = cols, name = "Brawl/Year")        

    frame.emp <- grid.arrange(age.disp.scatter, age.maj.scatter)


## Combine plots and have only one legend
#    tmp <- ggplot_gtable(ggplot_build(age.disp.scatter))
#    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#    legend <- tmp$grobs[[leg]]

#    frame.emp <- grid.arrange(arrangeGrob(age.disp.scatter + opts(legend.position="none"),
#                         age.maj.scatter + opts(legend.position="none")), legend, 
#             widths=unit.c(unit(1, "npc") - legend$width, legend$width), nrow=1)
