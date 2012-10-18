############
# mtable for regression results using all countries with legislatures
# Christopher Gandrud
# Updated 1 February 2012
############  
  
    library(memisc)
        table.1 <- mtable(
            F1, F2, F3, F4, F5, F6, F7, F8,
            summary.stats = c("AIC", "Likelihood-ratio", "p", "N")
        )
        
        table.1 <- relabel(table.1, 
            maj = "Majority",
            DemAge = "Dem Age",
            frac = "Frac",
            polariz = "Polariz",
            "system: Assembly-Elected President/Presidential" = "Assembly-Elect Pres.",
            "system: Parliamentary/Presidential" = "Parliamentary",
            tenshort = "Tenshort",
            ethnic_Alesina = "Ethnic Frac.",
            CW_trust = "Trust",
            CW_survSelfExpr = "Self Expression",
            "udems_mean" = "UDS"
        )
        toLatex(table.1, colsep = "") 