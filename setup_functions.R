library(stringr)

setup.aim1 <- function(vars, do.survey.vars = TRUE, add.weight.var = TRUE,
                       add.ballot = FALSE) {
  g.reg.proc = create.df(data = da35478.0001, 
                         vars = vars,
                         do.survey.vars = do.survey.vars,
                         add.weight.var = add.weight.var,
                         add.ballot = add.ballot)
  return(g.reg.proc)
}


setup.df.full <- function(do.survey.vars = TRUE) {
  library(lm.beta)
  library(stringr)
  setwd("c:/Dropbox/0 - Diss/analyses/")
  load("35478-0001-Data.rda")
  # load("~/Documents/gss/analyses/35478-0001-Data.rda")
  # setwd("~/Documents/gss/analyses/")
  numerics <- read.csv("numeric.csv")
  numeric.vars <- names(numerics)
  binaries <- read.csv("binary.csv")
  binary.vars <- names(binaries)
  
  ## Set up variables.
  g.reg.proc = setup.aim1(c(numeric.vars, binary.vars), do.survey.vars = do.survey.vars)
  g.reg.proc = setup.binaries(g.reg.proc, binary.vars)
  # Consolidate pairs
  double.pairs <- read.csv("double_pairs.csv", header = FALSE, as.is = c(TRUE, TRUE, TRUE))
  g.reg.proc <- combine.vars(g.reg.proc, double.pairs[,1], double.pairs[,2], double.pairs[,3])

  return(g.reg.proc)
}


setup.vars.full <- function() {
  library(lm.beta)
  library(stringr)
  setwd("c:/Dropbox/0 - Diss/analyses/")
  load("35478-0001-Data.rda")
  # load("~/Documents/gss/analyses/35478-0001-Data.rda")
  # setwd("~/Documents/gss/analyses/")
  numerics <- read.csv("numeric.csv")
  numeric.vars <- names(numerics)
  binaries <- read.csv("binary.csv")
  binary.vars <- names(binaries)
  
  # Consolidate pairs
  double.pairs <- read.csv("double_pairs.csv", header = FALSE, as.is = c(TRUE, TRUE, TRUE))
  # g.reg.proc <- combine.vars(data, double.pairs[,1], double.pairs[,2], double.pairs[,3])
  numeric.vars <- c(numeric.vars, double.pairs[,3])
  numeric.vars <- clean.varlist(numeric.vars, c(double.pairs[,1], double.pairs[,2]))
  
  return(numeric.vars)
}


class.excludes <- function(varname) {
  if (varname == "HRS2" || varname == "SPHRS2" || 
     varname == "CRACK30" || varname == "ARTCOST" || varname == "ARTINT" || 
     varname == "ARTTRVL" || varname == "ARTGST" || varname == "ARTTIME" || 
     varname == "ARTLOC") {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}


skip.if.weighting <- function(varname) {
  if (varname == "PRFMATT") {
    return(TRUE)
  }
  if (varname == "PRFMWHY") {
    return(TRUE)
  }
  if (varname == "PRFMWHY1") {
    return(TRUE)
  }
  if (varname == "PRFMWHY2") {
    return(TRUE)
  }
  return(FALSE)
}


get.balanced.df <- function(data, term.var) {
  # bigger.list, smaller.list
  # Get the smallest label size.
  print(term.var)
  term.labels = labels(table(data[term.var]))[[1]]
  print(table(data[term.var]))
  print(term.labels)
  smallest.count = nrow(data[term.var])
  smallest.label = NULL
  for (term.label in term.labels) {
    # print(term.label)
    term.list = which(data[term.var] == term.label)
    # print(term.list)
    term.length = length(term.list)
    # print(term.length)
    if (term.length < smallest.count) { 
      smallest.count = term.length
      smallest.label = term.label
    }
  }
  
  print("Smallest is:")
  print(smallest.label)
  print(smallest.count)
  # Balance term variable.  
  full.balance.list = c()
  for (term.label in term.labels) {
    bigger.list = which(data[term.var] == term.label)
    larger.balance.list = sample(bigger.list, smallest.count, replace = FALSE)
    
    full.balance.list = c(full.balance.list, larger.balance.list)
  }
  
  # smaller.count = is.true.count(smaller.list)
  
  # larger.balance.list
  # larger.balance.list = sort(larger.balance.list)
  
  full.balance.list = sort(full.balance.list)
  # print(full.balance.list)
  
  return(data[full.balance.list,])
}
