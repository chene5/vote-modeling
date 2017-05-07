create.df <- function(data, vars, do.survey.vars = TRUE, add.weight.var = TRUE,
                      add.ballot = FALSE) {
  g.reg = setupDf(data, 
                  do.survey.vars = do.survey.vars, 
                  add.weight.var = add.weight.var,
                  add.ballot = add.ballot)

  g.reg = addVars(g.reg, data, vars)
  
  g.reg.proc <- processGss(g.reg)
  
  # Setup VETERAN binary variable
  g.reg.proc <- setup.veteran(g.reg.proc)
  
  dim(g.reg.proc)
  
  # Clean data
  g.reg.proc <- clean.data(g.reg.proc)
  
  # Recode variables for interpretability.
  g.reg.proc <- recode.variables(g.reg.proc)

  return(g.reg.proc)
}


setupDf <- function(data, variables = NULL, do.survey.vars = TRUE, add.weight.var = TRUE,
                    add.ballot = FALSE) {
  g.reg <- data.frame("POLVIEWS" = data$POLVIEWS)
  g.reg <- cbind(g.reg, "RACE" = data$RACE)
  g.reg <- cbind(g.reg, "AGE" = data$AGE)
  g.reg <- cbind(g.reg, "SEX" = data$SEX)
  g.reg <- cbind(g.reg, "ATTEND" = data$ATTEND)
  g.reg <- cbind(g.reg, "CLASS" = data$CLASS)
  g.reg <- cbind(g.reg, "CONINC" = data$CONINC)
  g.reg <- cbind(g.reg, "REGION" = data$REGION)
  g.reg <- cbind(g.reg, "YEAR" = data$YEAR)
  g.reg <- cbind(g.reg, "EDUC" = data$EDUC)
  # g.reg <- cbind(g.reg, "VOTE08" = data$VOTE08)
  # g.reg <- cbind(g.reg, "PRES08" = data$PRES08)

  # Add any additional variables.  
  if (!is.null(variables)) {
    for (variable in variables) {
      g.reg <- cbind(g.reg, variable = data[variable])
    }
  }

  # Add the ballot variable.
  if (add.ballot) {
    g.reg <- cbind(g.reg, "BALLOT" = data$BALLOT)
  }
  
  # Add the survey weight variable if requested.
  if (add.weight.var) {
    g.reg <- cbind(g.reg, "WTSSALL" = data$WTSSALL)
    if ("WTCOMBNR" %in% names(data)) {
      g.reg <- cbind(g.reg, "WTCOMBNR" = data$WTCOMBNR)
    }
  }
  
  # Add survey variables if requested.
  if (do.survey.vars == TRUE) {
    g.reg <- cbind(g.reg, "VPSU" = data$VPSU)
    g.reg <- cbind(g.reg, "VSTRAT" = data$VSTRAT)
  }
  
  # Setup region variable
  g.reg <- setup.region(g.reg)
  
  return(g.reg)
}


addVars <- function(new.df, full.df, varnames) {
  for (i in 1:length(varnames)) {
    # If the variable is already in the new df, skip it.
    if ((varnames[[i]] %in% names(new.df))) {
      next
    }
    if (!(varnames[[i]] %in% names(full.df))) {
      print(varnames[[i]])
    }
    # Add this variable to the new df.
    new.df <- cbind(new.df, "varnames[i]" = full.df[varnames[i]])
  }
  
  return(new.df)
}


recode.variables <- function(g.reg.proc) {
  #### Recoding for better interpretability. Usually reversing the coding.
  val.range = c(3, 4, 5, 6, 7)
  for (this.val in val.range) {
    this.filename = paste(this.val, "_max_vars.txt", sep = "")
    these.vars = scan(this.filename, what = "")
    for (this.var in these.vars) {
      if (!(this.var %in% names(g.reg.proc))) {
        print(paste(this.var, "NOT FOUND"))
        next
      }
      g.reg.proc[this.var] <- this.val - g.reg.proc[this.var]
      print(paste(this.var, "recoded"))
    }
  }
  # How scientific variables.
  sci.vars = scan("sci_vars.txt", what = "")
  for (this.var in sci.vars) {
    if (!(this.var %in% names(g.reg.proc))) {
      print(paste(this.var, "NOT FOUND"))
      next
    }
    g.reg.proc[this.var][g.reg.proc[this.var] == 5] <- NA
    g.reg.proc[this.var] <- 4 - g.reg.proc[this.var]
    print(paste(this.var, "recoded"))
  }
  
  return(g.reg.proc)
}


combine.vars <- function(data, vars1, vars2, varnames) {
  var.length = nrow(data)
  for (i in 1:length(vars1)) {
    if (!(vars1[[i]] %in% names(data))) {
      next
    }
    temp.var = rep(NA, var.length)
    print(vars1[[i]])
    print(summary(data[vars1[[i]]]))
    
    temp.var = scale(data[vars1[[i]]])
    temp.var[!is.na(data[vars2[[i]]])] = 
      scale(data[vars2[[i]]][!is.na(data[vars2[[i]]])])
    # data <- cbind(data, "varnames[i]" = temp.var)
    data[varnames[[i]]] <- temp.var
  }
  data <- data[ , !(names(data) %in% vars1)]
  data <- data[ , !(names(data) %in% vars2)]
  return(data)
}


clean.varlist <- function(varlist, vars.to.clean) {
  for (var in vars.to.clean) {
    match.result = match(var, varlist)
    if (is.na(match.result)) {
      next
    }
    varlist = varlist[-match.result]
  }
  return(varlist)
}


setup.college <- function(data) {
  data$EDUC[data$EDUC < 13] = 0
  data$EDUC[data$EDUC >= 13] = 1
  return(data)
}


setup.region <- function(full.df) {
  # Rename region variable levels to their numeric value.
  # But keep the variable as a factor variable.
  # Set reference level to 9 (Pacific region).
  library(prettyR)
  lbls <- sort(levels(full.df$REGION))
  lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
  full.df$REGION <- sub("^\\(0*([0-9]+)\\).+$", "\\1", full.df$REGION)
  full.df$REGION <- as.factor(full.df$REGION)
  full.df$REGION <- add.value.labels(full.df$REGION, lbls)
  full.df$REGION <- relevel(full.df$REGION, ref = "9")
  return(full.df)
}


setup.veteran <- function(full.df) {
  # Set veteran to 0 or 1.
  if (!("VETYEARS" %in% names(full.df))) {
    print("VETYEARS not found.")
    return(full.df)
  }
  temp.veteran.var = full.df$VETYEARS
  temp.veteran.var[full.df$VETYEARS > 0] <- 1
  full.df <- cbind(full.df, "VETERAN" = temp.veteran.var)
  return(full.df)
}


keep.as.factor <- function(varname) {
  if (names(varname) == "REGION") {
    # print("Skipping REGION")
    return(TRUE)
  }
  if (names(varname) == "REG16") {
    # print("Skipping REG16")
    return(TRUE)
  }
  if (names(varname) == "RELIG") {
    # print("Skipping REG16")
    return(TRUE)
  }
  if (names(varname) == "RELIG16") {
    # print("Skipping REG16")
    return(TRUE)
  }
  if (names(varname) == "MARITAL") {
    # print("Skipping MARITAL")
    return(TRUE)
  }
  if (names(varname) == "FAMILY16") {
    # print("Skipping FAMILY16")
    return(TRUE)
  }
  if (names(varname) == "RPLACE") {
    return(TRUE)
  }
  if (names(varname) == "DWELLING") {
    return(TRUE)
  }
  # if (names(varname) == "ZODIAC") {
  # print("Skipping ZODIAC")
  # return(TRUE)
  # }
  return(FALSE)
}


processGss <- function(gssData) {
  library(prettyR)
  for (i in 1:ncol(gssData)) {
    # Check if we want to keep this variable as a factor variable.
    if(keep.as.factor(gssData[i])) {
      next
    }
    
    varLevels <- levels(gssData[, i])
    if (!is.null(varLevels)) {
      # This code is from factor_to_numeric_icpsr.R
      lbls <- sort(levels(gssData[, i]))
      lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
      gssData[, i] <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", gssData[, i]))
      gssData[, i] <- add.value.labels(gssData[, i], lbls)
    }

  }
  return(gssData)
}


factor.to.numeric <- function(factor.var) {
  if (is.numeric(factor.var)) {
    return(factor.var)
  }
  # This code is from factor_to_numeric_icpsr.R
  lbls <- sort(levels(factor.var))
  lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
  numeric.var <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", factor.var))
  numeric.var <- add.value.labels(numeric.var, lbls)
  return(numeric.var)
}


processGss.orig <- function(gssData) {
  library(stringr)
  pattern = "\\([[:digit:]]+\\)"
  for (i in 1:ncol(gssData)) {
    if (names(gssData[i]) == "REGION") {
      # print("Skipping REGION")
      next
    }
    if (names(gssData[i]) == "MARITAL") {
      # print("Skipping MARITAL")
      next
    }
    # if (names(gssData[i]) == "ZODIAC") {
      # print("Skipping ZODIAC")
      # next
    # }
    varLevels <- levels(gssData[, i])
    if (!is.null(varLevels)) {
      # print(summary(gssData[, i]))
      for (j in 1:length(varLevels)) {
        # print(varLevels[j])
        # value <- grep("\\(([:digit:]+)\\)", varLevels[j])
        # m <- regexec("\\(([[:digit:]]+)\\)", varLevels[j])
        # matches <- regmatches(varLevels[j], m)
        # XXX: will the parenthesized match always be second? Hope so.
        # print(matches)
        # value <- matches[2]
        value = str_extract(varLevels[j], pattern)
        value = sub("\\(", "", value)
        value = sub("\\)", "", value)
        
        # print(value)
        levels(gssData[, i])[levels(gssData[, i]) == varLevels[j]] <- value
      }
      gssData[, i] <- as.numeric(as.character(gssData[, i]))
      # print(summary(gssData[, i]))
    }
  }
  return(gssData)
}


make.levels.numeric <- function(variable) {
  return(as.numeric(as.character(variable)))
}


setup.binaries <- function(data, binary.vars) {
  # Make 0 or 1
  # Many binary variables are coded as 2 = No.
  # Recode these variables so that 0 = No.
  # Other variables need to be specially handled.
  for (var in binary.vars) {
    print(var)
    if (!(var %in% names(data))) {
      print(paste(var, "not in dataframe."))
      next
    }
    if (var == "PARBORN") {
      print("Skipping PARBORN")
      cat("\n")
      next
    }
    if (var == "ETHNUM") {
      print("Skipping ETHNUM")
      cat("\n")
      next
    }
    if (var == "RACOPEN") {
      print("Skipping RACOPEN")
      cat("\n")
      next
    }
    if (var == "HELPFUL") {
      print(table(data[var]))
      data[var][data[var] == 2] <- 0
      data[var][data[var] == 3] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "FAIR") {
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] == 2] <- 1
      data[var][data[var] == 3] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "TRUST") {
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] == 2] <- 1
      data[var][data[var] == 3] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "EXPDESGN") {
      # Code this to be 1 = wrong (0), 2 = right (1)
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] == 2] <- 1
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "ODDS1") {
      # Code this to be 1 = wrong (0), 2 = right (1)
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] == 2] <- 1
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "ODDS2") {
      # Code this to be 1 = wrong (0), 2 = right (1)
      print(table(data[var]))
      data[var][data[var] == 2] <- 0
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "RADIOACT") {
      # Code this to be 1 = wrong (0), 2 = right (1)
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] == 2] <- 1
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "LASERS") {
      # Code this to be 1 = wrong (0), 2 = right (1)
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] == 2] <- 1
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "VIRUSES") {
      # Code this to be 1 = wrong (0), 2 = right (1)
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] == 2] <- 1
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "SOLARREV") {
      # Code this to be 1 = wrong (0), 2 = right (1)
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] == 2] <- 0
      data[var][data[var] == 3] <- 1
      data[var][data[var] == 4] <- 0
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "SEXORNT") {
      # Code this to be 0 = Hetero, 1 = Not
      print(table(data[var]))
      data[var][data[var] == 3] <- 0
      data[var][data[var] == 2] <- 1
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "HRTOP") {
      # Code this to be 0 = No difference, 1 = the 1st option.
      print(table(data[var]))
      data[var][data[var] == 3] <- 0
      data[var][data[var] == 2] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "HRTOP37") {
      # Code this to be 0 = No difference, 1 = the 1st option.
      print(table(data[var]))
      data[var][data[var] == 3] <- 0
      data[var][data[var] == 2] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "HRTOPKID") {
      # Code this to be 0 = No difference, 1 = the 1st option.
      print(table(data[var]))
      data[var][data[var] == 3] <- 0
      data[var][data[var] == 2] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "MEDPAY") {
      # Code this to be 0 = No difference, 1 = the 1st option.
      print(table(data[var]))
      data[var][data[var] == 3] <- 0
      data[var][data[var] == 2] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "MEDCOMMT") {
      # Code this to be 0 = No difference, 1 = the 1st option.
      print(table(data[var]))
      data[var][data[var] == 3] <- 0
      data[var][data[var] == 2] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "MEDUNAV") {
      # Code this to be 0 = No difference, 1 = the 1st option.
      print(table(data[var]))
      data[var][data[var] == 3] <- 0
      data[var][data[var] == 2] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "MEDWTLST") {
      # Code this to be 0 = No difference, 1 = the 1st option.
      print(table(data[var]))
      data[var][data[var] == 3] <- 0
      data[var][data[var] == 2] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "INSTYPE") {
      # Code this to be 0 = No coverage, 1 = coverage.
      print(table(data[var]))
      data[var][data[var] == 1] <- 0
      data[var][data[var] > 1] <- 1
      print(table(data[var]))
      cat("\n")
      next
    }
    if (var == "MAWORK14") {
      # Code this to be 0 = No difference, 1 = the 1st option.
      print(table(data[var]))
      data[var][data[var] == 3] <- NA
      print(table(data[var]))
      cat("\n")
      next
    }
    
    # Make 0 or 1
    # Many binary variables are coded as 2 = No.
    # Recode these variables so that 0 = No.
    if (var %in% names(data)) {
      print(table(data[var]))
      data[var][data[var] == 2] <- 0
      print(table(data[var]))
      cat("\n")
    }
  }
  return(data)
}


clean.data <- function(g.reg.proc) {
  if ("RACE" %in% names(g.reg.proc)) {
    # Make race binary: black or white
    # White = 0, Black = 1
    g.reg.proc$RACE[g.reg.proc$RACE == 1] <- 0
    g.reg.proc$RACE[g.reg.proc$RACE == 2] <- 1
    g.reg.proc$RACE[g.reg.proc$RACE == 3] <- NA
  }
  
  if ("SEX" %in% names(g.reg.proc)) {
    # Sex: Female = 0, Male = 1
    g.reg.proc$SEX[g.reg.proc$SEX == 2] <- 0
  }
  
  if ("VOTE08" %in% names(g.reg.proc)) {
    # Make vote binary: yes (1) or no (0)
    g.reg.proc$VOTE08[g.reg.proc$VOTE08 == 2] <- 0
    g.reg.proc$VOTE08[g.reg.proc$VOTE08 == 3] <- NA
  }
  if ("PRES08" %in% names(g.reg.proc)) {
    # PRES08: McCain = 0, Obama = 1
    g.reg.proc$PRES08[g.reg.proc$PRES08 == 2] <- 0
    g.reg.proc$PRES08[g.reg.proc$PRES08 == 3] <- NA
    g.reg.proc$PRES08[g.reg.proc$PRES08 == 4] <- NA
  }
  
  if ("PARTYID" %in% names(g.reg.proc)) {
    # Other party = NA.
    g.reg.proc$PARTYID[g.reg.proc$PARTYID == 7] <- NA
  }
  
  if ("HEFINFO" %in% names(g.reg.proc)) {
    # Not in household = NA.
    g.reg.proc$HEFINFO[g.reg.proc$HEFINFO == 22] <- NA
  }
  
  # Make 0 - Worse, 1 - Same, 2 - Better.
  if ("FINALTER" %in% names(g.reg.proc)) {
    g.reg.proc$FINALTER[g.reg.proc$FINALTER == 2] <- 0
    g.reg.proc$FINALTER[g.reg.proc$FINALTER == 1] <- 2
    g.reg.proc$FINALTER[g.reg.proc$FINALTER == 3] <- 1
  }

  # Make 0 or 1
  if ("COLATH" %in% names(g.reg.proc)) {
    g.reg.proc$COLATH[g.reg.proc$COLATH == 4] <- 1
    g.reg.proc$COLATH[g.reg.proc$COLATH == 5] <- 0
  }
  if ("COLRAC" %in% names(g.reg.proc)) {
    g.reg.proc$COLRAC[g.reg.proc$COLRAC == 4] <- 1
    g.reg.proc$COLRAC[g.reg.proc$COLRAC == 5] <- 0
  }
  if ("COLCOM" %in% names(g.reg.proc)) {
    g.reg.proc$COLCOM[g.reg.proc$COLCOM == 4] <- 1
    g.reg.proc$COLCOM[g.reg.proc$COLCOM == 5] <- 0
  }
  if ("COLMIL" %in% names(g.reg.proc)) {
    g.reg.proc$COLMIL[g.reg.proc$COLMIL == 4] <- 1
    g.reg.proc$COLMIL[g.reg.proc$COLMIL == 5] <- 0
  }
  if ("COLHOMO" %in% names(g.reg.proc)) {
    g.reg.proc$COLHOMO[g.reg.proc$COLHOMO == 4] <- 1
    g.reg.proc$COLHOMO[g.reg.proc$COLHOMO == 5] <- 0
  }
  if ("COLMSLM" %in% names(g.reg.proc)) {
    g.reg.proc$COLMSLM[g.reg.proc$COLMSLM == 4] <- 1
    g.reg.proc$COLMSLM[g.reg.proc$COLMSLM == 5] <- 0
  }
  
  # Make 0 or 1
  if ("LIBATH" %in% names(g.reg.proc)) {
    g.reg.proc$LIBATH <- g.reg.proc$LIBATH - 1
  }
  if ("LIBRAC" %in% names(g.reg.proc)) {
    g.reg.proc$LIBRAC <- g.reg.proc$LIBRAC - 1
  }
  if ("LIBCOM" %in% names(g.reg.proc)) {
    g.reg.proc$LIBCOM <- g.reg.proc$LIBCOM - 1
  }
  if ("LIBMIL" %in% names(g.reg.proc)) {
    g.reg.proc$LIBMIL <- g.reg.proc$LIBMIL - 1
  }
  if ("LIBHOMO" %in% names(g.reg.proc)) {
    g.reg.proc$LIBHOMO <- g.reg.proc$LIBHOMO - 1
  }
  if ("LIBMSLM" %in% names(g.reg.proc)) {
    g.reg.proc$LIBMSLM <- g.reg.proc$LIBMSLM - 1
  }
  
  if ("WKSUBS" %in% names(g.reg.proc)) {
    g.reg.proc$WKSUBS[g.reg.proc$WKSUBS == 3] <- 1
    g.reg.proc$WKSUBS[g.reg.proc$WKSUBS == 4] <- 0
  }
  if ("WKSUPS" %in% names(g.reg.proc)) {
    g.reg.proc$WKSUPS[g.reg.proc$WKSUPS == 3] <- 1
    g.reg.proc$WKSUPS[g.reg.proc$WKSUPS == 4] <- 0
  }

  # Recode AGED to go from 1 to 3, with "Depends" as the midpoint.
  if ("AGED" %in% names(g.reg.proc)) {
    g.reg.proc$AGED[g.reg.proc$AGED == 2] <- 4
    g.reg.proc$AGED[g.reg.proc$AGED == 3] <- 2
    g.reg.proc$AGED[g.reg.proc$AGED == 4] <- 3
  }
  
  # Make Other (4) = NA
  if ("BIBLE" %in% names(g.reg.proc)) {
    g.reg.proc$BIBLE[g.reg.proc$BIBLE == 4] <- NA
    # Reverse code for interpretability.
    g.reg.proc$BIBLE <- 3 - g.reg.proc$BIBLE
  }
  
  # Make As many as you want (8) = NA
  if ("CHLDIDEL" %in% names(g.reg.proc)) {
    g.reg.proc$CHLDIDEL[g.reg.proc$CHLDIDEL == 8] <- NA
  }

  # Make "Stay as is" the midpoint, 2.
  if ("DIVLAW" %in% names(g.reg.proc)) {
    g.reg.proc$DIVLAW[g.reg.proc$DIVLAW == 2] <- 4
    g.reg.proc$DIVLAW[g.reg.proc$DIVLAW == 3] <- 2
    g.reg.proc$DIVLAW[g.reg.proc$DIVLAW == 4] <- 3
  }
  
  # Make 0 (No phone) or 1 Phone anywhere (e.g., phone at home or cellphone)
  if ("PHONE" %in% names(g.reg.proc)) {
    g.reg.proc$PHONE[g.reg.proc$PHONE == 1] <- 0
    g.reg.proc$PHONE[g.reg.proc$PHONE == 2] <- NA
    g.reg.proc$PHONE[g.reg.proc$PHONE == 3] <- 1
    g.reg.proc$PHONE[g.reg.proc$PHONE == 4] <- 1
    g.reg.proc$PHONE[g.reg.proc$PHONE == 5] <- 1
    g.reg.proc$PHONE[g.reg.proc$PHONE == 6] <- 1
  }
  
  if ("PARBORN" %in% names(g.reg.proc)) {
    # Change parborn to 0 or 1
    # 1 = Both parents born in US.
    # 0 = At least one parent born outside US
    # XXX: Change don't know to NA, which drops 15 people whose 
    # mothers were born in the US
    g.reg.proc$PARBORN[g.reg.proc$PARBORN == 2] <- 1
    g.reg.proc$PARBORN[g.reg.proc$PARBORN == 3] <- 1
    g.reg.proc$PARBORN[g.reg.proc$PARBORN == 4] <- NA
    g.reg.proc$PARBORN[g.reg.proc$PARBORN == 5] <- 1
    g.reg.proc$PARBORN[g.reg.proc$PARBORN == 6] <- NA
    g.reg.proc$PARBORN[g.reg.proc$PARBORN == 7] <- NA
    g.reg.proc$PARBORN[g.reg.proc$PARBORN == 8] <- 1
    g.reg.proc$PARBORN <- 1 - g.reg.proc$PARBORN
  }

  if ("PARTNERS" %in% names(g.reg.proc)) {
    # Don't know becomes NA.
    # Several becomes NA.
    g.reg.proc$PARTNERS[g.reg.proc$PARTNERS == 9] <- NA
    g.reg.proc$PARTNERS[g.reg.proc$PARTNERS == 95] <- NA
  }
  if ("PARTNRS5" %in% names(g.reg.proc)) {
    # Don't know becomes NA.
    g.reg.proc$PARTNRS5[g.reg.proc$PARTNRS5 == 9] <- NA
  }
  
  if ("MEDBEST" %in% names(g.reg.proc)) {
    # Can't choose becomes NA.
    g.reg.proc$MEDBEST[g.reg.proc$MEDBEST == 8] <- NA
    # Reverse the coding.
    g.reg.proc$MEDBEST <- 5 - g.reg.proc$MEDBEST
  }

  if ("MEDDRCH" %in% names(g.reg.proc)) {
    # Can't choose becomes NA.
    g.reg.proc$MEDDRCH[g.reg.proc$MEDDRCH == 8] <- NA
    # Reverse the coding.
    g.reg.proc$MEDDRCH <- 5 - g.reg.proc$MEDDRCH
  }
  
  if ("DOCVISIT" %in% names(g.reg.proc)) {
    # Can't choose becomes NA.
    g.reg.proc$DOCVISIT[g.reg.proc$DOCVISIT == 8] <- NA
    # Reverse the coding.
    g.reg.proc$DOCVISIT <- 7 - g.reg.proc$DOCVISIT
  }
  
  if ("HOSPSAT" %in% names(g.reg.proc)) {
    # Can't choose becomes NA.
    g.reg.proc$HOSPSAT[g.reg.proc$HOSPSAT == 8] <- NA
    # Reverse the coding.
    g.reg.proc$HOSPSAT <- 7 - g.reg.proc$HOSPSAT
  }
  
  if ("PAIDLVPY" %in% names(g.reg.proc)) {
    # Order PAIDLVPY
    g.reg.proc$PAIDLVPY[g.reg.proc$PAIDLVPY == 4] <- NA
    g.reg.proc$PAIDLVPY[g.reg.proc$PAIDLVPY == 1] <- 0
    g.reg.proc$PAIDLVPY[g.reg.proc$PAIDLVPY == 3] <- 1
  }
  
  if ("ETHNUM" %in% names(g.reg.proc)) {
    # Change ETHNUM to 0 or 1
    # 0 = Selects one country for background.
    # 1 = Selects at least two.
    # Can't name one becomes NA.
    g.reg.proc$ETHNUM[g.reg.proc$ETHNUM == 1] <- 0
    g.reg.proc$ETHNUM[g.reg.proc$ETHNUM == 2] <- 1
    g.reg.proc$ETHNUM[g.reg.proc$ETHNUM == 3] <- 1
    g.reg.proc$ETHNUM[g.reg.proc$ETHNUM == 4] <- NA
  }
  
  if ("RACOPEN" %in% names(g.reg.proc)) {
    # Change RACOPEN to 0 or 1
    # 0 = Choose who to sell to.
    # 1 = Can't choose who to sell to.
    # Can't name one becomes NA.
    g.reg.proc$RACOPEN[g.reg.proc$RACOPEN == 1] <- 0
    g.reg.proc$RACOPEN[g.reg.proc$RACOPEN == 2] <- 1
    g.reg.proc$RACOPEN[g.reg.proc$RACOPEN == 3] <- NA
  }
  
  if ("COURTS" %in% names(g.reg.proc)) {
    # Order COURTS
    g.reg.proc$COURTS[g.reg.proc$COURTS == 2] <- 4
    g.reg.proc$COURTS[g.reg.proc$COURTS == 3] <- 2
    g.reg.proc$COURTS[g.reg.proc$COURTS == 4] <- 3
  }
  
  if ("OWNGUN" %in% names(g.reg.proc)) {
    # Make 0 = No
    # 3 = Refused, make NA
    g.reg.proc$OWNGUN[g.reg.proc$OWNGUN == 2] <- 0
    g.reg.proc$OWNGUN[g.reg.proc$OWNGUN == 3] <- NA
  }
  
  if ("PISTOL" %in% names(g.reg.proc)) {
    # Make 0 = No
    # 3 = Refused, make NA
    g.reg.proc$PISTOL[g.reg.proc$PISTOL == 2] <- 0
    g.reg.proc$PISTOL[g.reg.proc$PISTOL == 3] <- NA
    # If OWNGUN == 0, this is also 0
    g.reg.proc$PISTOL[g.reg.proc$OWNGUN == 0] <- 0
  }
  
  if ("SHOTGUN" %in% names(g.reg.proc)) {
    # Make 0 = No
    # 3 = Refused, make NA
    g.reg.proc$SHOTGUN[g.reg.proc$SHOTGUN == 2] <- 0
    g.reg.proc$SHOTGUN[g.reg.proc$SHOTGUN == 3] <- NA
    # If OWNGUN == 0, this is also 0
    g.reg.proc$SHOTGUN[g.reg.proc$OWNGUN == 0] <- 0
  }
  
  if ("RIFLE" %in% names(g.reg.proc)) {
    # Make 0 = No
    # 3 = Refused, make NA
    g.reg.proc$RIFLE[g.reg.proc$RIFLE == 2] <- 0
    g.reg.proc$RIFLE[g.reg.proc$RIFLE == 3] <- NA
    # If OWNGUN == 0, this is also 0
    g.reg.proc$RIFLE[g.reg.proc$OWNGUN == 0] <- 0
  }
  
  if ("ROWNGUN" %in% names(g.reg.proc)) {
    # Make 0 = No
    # 3 = Refused, make NA
    g.reg.proc$ROWNGUN[g.reg.proc$ROWNGUN == 2] <- 0
    g.reg.proc$ROWNGUN[g.reg.proc$ROWNGUN == 3] <- NA
  }
  
  if ("DWELOWN" %in% names(g.reg.proc)) {
    # Make 0 = Pays rent
    # 3 = Other, make NA
    g.reg.proc$DWELOWN[g.reg.proc$DWELOWN == 2] <- 0
    g.reg.proc$DWELOWN[g.reg.proc$DWELOWN == 3] <- NA
  }
  
  if ("EVSTRAY" %in% names(g.reg.proc)) {
    # Make 0 = No cheating
    # 3 = Never married, make NA
    g.reg.proc$EVSTRAY[g.reg.proc$EVSTRAY == 2] <- 0
    g.reg.proc$EVSTRAY[g.reg.proc$EVSTRAY == 3] <- NA
  }
  
  if ("HUNT" %in% names(g.reg.proc)) {
    # Make 0 = Neither R nor spouse hunts
    # 2 = Spouse, make 1
    # 3 = Both, make 1
    g.reg.proc$HUNT[g.reg.proc$HUNT == 4] <- 0
    g.reg.proc$HUNT[g.reg.proc$HUNT == 2] <- 1
    g.reg.proc$HUNT[g.reg.proc$HUNT == 3] <- 1
  }
  
  if ("UNION" %in% names(g.reg.proc)) {
    # Make 0 = Neither R nor spouse member of a union
    # 2 = Spouse, make 1
    # 3 = Both, make 1
    g.reg.proc$UNION[g.reg.proc$UNION == 4] <- 0
    g.reg.proc$UNION[g.reg.proc$UNION == 2] <- 1
    g.reg.proc$UNION[g.reg.proc$UNION == 3] <- 1
  }
  
  return(g.reg.proc)  
}


clean.vote.var <- function(vote.var, make.factor = FALSE) {
  # Make vote binary: yes (1) or no (0)
  vote.var[vote.var == 2] <- 0
  vote.var[vote.var == 3] <- NA
  vote.var[vote.var == 4] <- NA
  vote.var[vote.var == 6] <- NA
  if (make.factor) {
    vote.var = as.factor(vote.var)
  }
  return(vote.var)
}


clean.pres.var <- function(pres.var, make.factor = FALSE) {
  # Make pres binary: yes (1) or no (0)
  pres.var[pres.var == 2] <- 0
  pres.var[pres.var == 3] <- NA
  pres.var[pres.var == 4] <- NA
  pres.var[pres.var == 6] <- NA
  if (make.factor) {
    pres.var = as.factor(pres.var)
  }
  return(pres.var)
}


is.skip.var <- function(varname) {
  # These variables are factor variables.
  if (varname == "REGION") {
    return(TRUE)
  }
  if (varname == "REG16") {
    return(TRUE)
  }
  if (varname == "RELIG") {
    return(TRUE)
  }
  if (varname == "RELIG16") {
    return(TRUE)
  }
  if (varname == "DWELLING") {
    return(TRUE)
  }
  if (varname == "MARITAL") {
    return(TRUE)
  }
  if (varname == "FAMILY16") {
    return(TRUE)
  }
  if (varname == "RPLACE") {
    return(TRUE)
  }
  # Skip these variables: they don't have enough observations.
  if (varname == "SPJOTH16") {
    return(TRUE)
  }
  if (varname == "MAJWOTH") {
    return(TRUE)
  }
  if (varname == "PAJWOTH") {
    return(TRUE)
  }
  if (varname == "BMITZVAH") {
    return(TRUE)
  }
  if (varname == "SYNMEM") {
    return(TRUE)
  }
  if (varname == "LOSEJOB5") {
    return(TRUE)
  }
  if (varname == "NEGJOB5") {
    return(TRUE)
  }
  if (varname == "HARJOB5") {
    return(TRUE)
  }
  if (varname == "DWELL5") {
    return(TRUE)
  }
  if (varname == "KIDNUM") {
    return(TRUE)
  }
  if (varname == "IDU30") {
    return(TRUE)
  }
  
  return(FALSE)  
}


make.formula <- function(dv, ivs) {
  # formula = as.formula(paste(varname, "~ POLVIEWS + RACE + AGE + SEX + ATTEND + CLASS"))
  
  iv.str = paste(ivs, collapse = " + ")
  # print(iv.str)
  formula = paste(dv, iv.str, sep = " ~ ")
  # print(formula)
  
  return(as.formula(formula))
}


center.vars <- function(data, var.list) {
  for (variable in var.list) {
    data[variable] = scale(data[variable], center = TRUE, scale = FALSE)
  }
  return(data)
}
