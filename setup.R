# Load the multiple imputation package.
library(mice)
# Load the setup source files.
source("gss2012_functions.R")
source("setup_functions.R")

# Load the dataset.
load("35478-0001-Data.rda")

set.seed(23)
print("Do 2012 full dataset ballots A & C.")

# Set up variable lists.
ac15.numeric.vars <- scan("ac15_numeric_vars_raw.txt", what = "")
ac15.binary.vars <- scan("ac15_binary_vars.txt", what = "")

## Set up variables.
g2012.ac.raw = setup.aim1(c(ac15.numeric.vars, ac15.binary.vars),
                          do.survey.vars = FALSE,
                          add.weight.var = FALSE,
                          add.ballot = FALSE)
g2012.ac.raw = setup.binaries(g2012.ac.raw, ac15.binary.vars)

# Consolidate pairs
ac15.double.pairs <- read.csv("ac15_double_pairs.csv", header = FALSE, as.is = c(TRUE, TRUE, TRUE))
g2012.ac.raw <- combine.vars(g2012.ac.raw, ac15.double.pairs[,1], ac15.double.pairs[,2], ac15.double.pairs[,3])
ac15.numeric.vars <- c(ac15.numeric.vars, ac15.double.pairs[,3])
ac15.numeric.vars <- clean.varlist(ac15.numeric.vars, c(ac15.double.pairs[,1], ac15.double.pairs[,2]))
# Clean out binary variables from numeric variables list.
ac15.numeric.vars <- clean.varlist(ac15.numeric.vars, ac15.binary.vars)

dim(g2012.ac.raw)
dim(da35478.0001)
g2012.ac.raw = g2012.ac.raw[(da35478.0001$BALLOT == "(1) Ballot A" | 
                             da35478.0001$BALLOT == "(3) Ballot C"),]
print(dim(g2012.ac.raw))

g2012.all.imputed.data = mice(g2012.ac.raw, m = 5, maxit = 5, method = "pmm", seed = 23)
g2012.imputed.data = complete(g2012.all.imputed.data, 1)

print(dim(g2012.imputed.data))
print(sum(is.na(g2012.imputed.data)))

g2012.drop.list = c("YEAR")
g2012.imputed.data = g2012.imputed.data[, !(names(g2012.imputed.data) %in% g2012.drop.list)]

# Make binaries factors.
all.binary.vars <- scan("2012_unweighted_binary_vars.txt", what = "")
for (var in all.binary.vars) {
  if (var %in% names(g2012.imputed.data)) {
    g2012.imputed.data[,var] <- as.factor(g2012.imputed.data[,var])
    print(paste(var, "now a factor."))
  }
}

## Setup factor names.
g2012.imputed.data$RACE = as.factor(g2012.imputed.data$RACE)
table(g2012.imputed.data$RACE)
levels(g2012.imputed.data$RACE) = c("White", "Black")
table(g2012.imputed.data$RACE)

g2012.imputed.data$EDUC[g2012.imputed.data$EDUC < 13] = 0
g2012.imputed.data$EDUC[g2012.imputed.data$EDUC >= 13] = 1
g2012.imputed.data$EDUC = as.factor(g2012.imputed.data$EDUC)
table(g2012.imputed.data$EDUC)
levels(g2012.imputed.data$EDUC) = c("No college", "College")
table(g2012.imputed.data$EDUC)

table(g2012.imputed.data$SEX)
g2012.imputed.data$SEX = as.factor(g2012.imputed.data$SEX)
levels(g2012.imputed.data$SEX)
levels(g2012.imputed.data$SEX) = c("Female", "Male")
table(g2012.imputed.data$SEX)

## Setup Black only df
g2012.imputed.black = g2012.imputed.data[g2012.imputed.data$RACE == "Black",]
