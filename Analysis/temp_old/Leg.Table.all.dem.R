############
# mtable for regression results using all countries with elected legislatures
# Christopher Gandrud
# Updated 1 February 2012
############  
  
    library(memisc)
        table.2 <- mtable(
            D1, D2, D3, D4, D5, D6, D7, D8,
            summary.stats = c("AIC", "Likelihood-ratio", "p", "N")
        )
        
        table.2 <- relabel(table.2, 
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
        toLatex(table.2)