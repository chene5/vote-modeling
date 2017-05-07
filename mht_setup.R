names(g2012.imputed.data)

# Remove REGION. It can't be made numeric.
g2012.numeric = g2012.imputed.data[,-8]
sink("make_numeric.txt")
for (variable in names(g2012.numeric)) {
  print(names(g2012.numeric[variable]))
  print(summary(g2012.numeric[,variable]))
  if(!is.null(levels(g2012.numeric[,variable]))) {
    print(levels(g2012.numeric[,variable]))
    g2012.numeric[,variable] = as.numeric(g2012.numeric[,variable])
    print(table(g2012.numeric[,variable]))
  }
  g2012.numeric[,variable] = scale(g2012.numeric[,variable], 
                                   center = TRUE, scale = TRUE)
  print(summary(g2012.numeric[,variable]))
}
sink()

names(g2012.numeric)

g2012.numeric.black = g2012.imputed.data[,-8]
g2012.numeric.black = g2012.numeric.black[g2012.numeric.black$RACE == "Black",]
dim(g2012.numeric.black)
sink("make_numeric_black.txt")
for (variable in names(g2012.numeric.black)) {
  print(names(g2012.numeric.black[variable]))
  print(summary(g2012.numeric.black[,variable]))
  if(!is.null(levels(g2012.numeric.black[,variable]))) {
    print(levels(g2012.numeric.black[,variable]))
    g2012.numeric.black[,variable] = as.numeric(g2012.numeric.black[,variable])
    print(table(g2012.numeric.black[,variable]))
  }
  g2012.numeric.black[,variable] = scale(g2012.numeric.black[,variable], 
                                   center = TRUE, scale = TRUE)
  print(summary(g2012.numeric.black[,variable]))
}
sink()

names(g2012.numeric.black)
