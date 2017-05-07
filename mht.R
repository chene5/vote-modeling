library(mht)

sink("mht_output.txt")
vote08.mht = mht(data = g2012.numeric[-106], g2012.imputed.data$VOTE08, maxq = 6)
print(vote08.mht)
print(vote08.mht$coefficients)
print(vote08.mht$ordre)
cat("\n")
cat("\n")
sink()

