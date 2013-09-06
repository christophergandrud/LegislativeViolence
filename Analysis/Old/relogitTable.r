############
# From Thomas Jensen
# http://polstat.org/blog/2012/1/creating-latex-tables-zelig-and-statnet-objects/
############

relogitTable <- function(...){
  x <- list(...)
  coef.cols <- 1
  coef.rows <- 2
  adigits = 3
  align = "l"
  nmodels <- length(x)
  multicolumn.align <- "c"
  
  model.summaries <- lapply(x, function(x) {
    x <- summary(x)
    return(x)
    })
  
  m.first = 1
  m.last = m.first + (nmodels - 1)
  model.names = paste("Model", m.first:m.last)
  
  orderCoef <- function(model.summaries) {
    nmodels <- length(model.summaries)
    mlength <- sapply(model.summaries, function(x) nrow(coef(x)))
    longest <- which.max(mlength) # longest model
    modelorder <- 1:nmodels 
    coefnames <- rownames(coef(model.summaries[[modelorder[1]]]))
  
    for(i in seq_along(model.summaries)) {
      matched <- match(rownames(coef(model.summaries[[i]])), coefnames, nomatch=0)
      unmatched <- which(is.na(matched) | matched==0)
      coefnames <- c(coefnames, rownames(coef(model.summaries[[i]]))[unmatched])
      }
    return(coefnames)
    }

  orderInfo <- function(model.summaries) {
    nmodels <- length(model.summaries)
    mlength <- sapply(model.summaries, function(x) length(x))
    longest <- which.max(mlength) # longest model
    modelorder <- 1:nmodels 
    coefnames <- names(model.summaries[[modelorder[1]]])
  
    for(i in seq_along(model.summaries)){
      matched <- match(names(model.summaries[[i]]), coefnames, nomatch=0)
      unmatched <- which(is.na(matched) | matched==0)
      coefnames <- c(coefnames,names(model.summaries[[i]])[unmatched])
      }
    return(coefnames)
    }
  
  coefnames <- orderCoef(model.summaries)
  
  for (i in 1:length(coefnames)){
    coefnames[i] <- sub("_","\\_", as.character(coefnames[i]))
    }
  
  coefPosition <- function(model.summaries, coefnames) {
    model.summaries <- lapply(model.summaries, function(x) {
      pos <- match(rownames(coef(x)), coefnames)
      attr(x,"var.pos") <- pos
      return(x)
    })
  return(model.summaries)
  }
  
  infoPosition <- function(model.summaries, coefnames) {
    model.summaries <- lapply(model.summaries, function(x) {
      pos <- match(names(x), coefnames)
      attr(x,"var.pos") <- pos
      return(x)
      })
    return(model.summaries)
    }

  model.summaries <- coefPosition(model.summaries, coefnames)
  
  modelInfo <- function(x) {
    env <- sys.parent()
    digits <- evalq(adigits, env)
    model.info <- list("$N$" = x$df.null + 1,"$AIC$"=formatC(x$aic,format="f",digits=3))
    class(model.info) <- "model.info"
    invisible(model.info) 
  }


  out.table <- lapply(model.summaries, function(x){
    var.pos <- attr(x,"var.pos")
    model.out <- model.se.out <- star.out <- rep(NA,length(coefnames))
    model.out[var.pos] <- x$coefficients[,1]
    star.out[var.pos] <-  ifelse(abs(x$coefficients[,1]/x$coefficients[,2]) > 1.96,"^*","")
    model.out <- ifelse(!is.na(model.out),paste(formatC(model.out,digits=3,format="f"),star.out),"")
	  model.se.out[var.pos] <- x$coefficients[,2]
    model.se.out <- ifelse(!is.na(model.se.out),paste("(",formatC(model.se.out,digits=3,format="f"),")"),"")


	  model.out <- rep(model.out, each=2)
    model.se.out <- rep(model.se.out, each=2)
    pos.se <- (1:length(model.out))[(1:length(model.out) %% 2==0)]
    model.out[pos.se] <- model.se.out[pos.se]
    attr(model.out,"model.info") <- modelInfo(x)
	  return(model.out)
    })
  
  out.matrix <- matrix(unlist(out.table),length(coefnames)*coef.rows,nmodels*coef.cols)
  out.matrix <- cbind(rep(coefnames,each=2), out.matrix)
  out.matrix[(row(out.matrix)[,1] %% 2 == 0) , 1] <- ""
  
  out.info <- lapply(out.table, attr, "model.info")
  info.names <- orderInfo(out.info)
  out.info <- infoPosition( out.info, orderInfo(out.info) )
  out.info <- lapply(out.info, function(x) {
    var.pos <- attr(x,"var.pos")
	  model.out <- rep("",length(info.names))
    model.out[var.pos] <- x
    return(model.out)
    })
  
  out.info <- matrix(unlist(out.info), length(info.names), nmodels)
  out.info <- cbind(as.character(info.names), out.info)

  outrows <- nrow(out.matrix)

  out.matrix <- format(out.matrix)
  out.matrix <- apply(out.matrix, 1, paste, collapse=" & ")

  out.info[,-1] <- format(out.info[,-1])

  out.info[,-1] <- sapply(as.matrix(out.info[,-1]), function(x) {
    paste("\\multicolumn{",coef.cols,"}{",multicolumn.align,"}{",x,"}",sep="")
    })
	
  out.info[,1] <- format(out.info[,1])
  out.info <- apply(out.info, 1, paste, collapse=" & ")
  
  x <- c(paste("\\begin{tabular}{",align, paste(rep(paste("D{.}{.}{",rep(adigits,coef.cols), "}", sep="",collapse=""),nmodels),collapse="@{\\hspace{2em}}"),"}",sep=""), "\\toprule \n &")
  x <- c(x, paste("", paste("\\multicolumn{",coef.cols,"}{", multicolumn.align,"}{", model.names,"}", collapse=" & ")  ))
  x <- c(x,"\\\\ \\midrule\n")
  x <- c(x,paste(out.matrix, collapse="\\\\ \n"))
  x <- c(x,"\\\\ \\midrule \n")
  x <- c(x,paste(out.info, collapse="\\\\ \n"))
  x <- c(x,"\\\\ \\bottomrule  \n\\end{tabular}\n")

 return(cat(x)) 
}