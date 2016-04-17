# This script was used to remove correlating environmental factors from a list obtained by GIS

# used packages
library(Hmisc)
library(caret)

# Usage: Rscript eliminate_correlated_variables.R input.csv output.csv R²
# "input.csv" should be a tab delimited CSV, where the first row should contain the variable lables, and the first column should contain the cases labels.
# "output.csv" is the path to where you want to write the results.
# "R²" is the maximum allowed correlation between variables. Any variables with a value higher than this will be collapsed. This value have to be between 0 and 1. It is optional and if omitted will defalut to 0.85.

args <- commandArgs(trailingOnly = TRUE) # Use arguments

infile <- args[1]
outfile <- args[2]
r_square <- if (try(typeof(args[3]) == "double")) {args[3]} else {0.85}


# descriptive matrices of p-values and correlation coefficients (after pearson)

data <- read.csv(file=infile, header=TRUE, sep="\t", row.names = 1)
data <- as.matrix(data)  # remove specimen names etc, only keep the variables
corrmatrix <- rcorr(data, type = "pearson")       # analysis (from Hmisc)
correlation <- corrmatrix$r                       # save correlation coefficients separately
correlation[abs(correlation) > r_square] <- "high"    # replace values over threshhold with sth
pvalues <- format(corrmatrix$P, digits = 4)       # save p-values separately and only display 4 digits of p-values (no rounding)

# pvalues[abs(pvalues) < 0.0005] <- "***"         <- can be added if youo like it
# pvalues[abs(pvalues) < 0.005] <- "**"
# pvalues[abs(pvalues) < 0.05] <- "*"

correlation[upper.tri(correlation)] <- pvalues[upper.tri(pvalues)]  # put p-values and coefficients together
diag(correlation) <- NA                                             # replace diagonal with sth

# so what you get is the triangle with coefficients in the lower half and according p-values in the upper half...

print(correlation)


### remove variables with correlations over given threshhold

# use again correlation matrix from above for findCorrelation function (from caret), set your threshhold,
# ("exact" recalculates correlations after removal of variables...mostly same results with or without in my case)

highcorr <- findCorrelation(corrmatrix$r, cutoff=r_square, exact = TRUE)    # output is a vector of column numbers of correlated variables
highcorr <- sort(highcorr)                                              # put them in order for removing them from the data set
reduced_data <- data[,-c(highcorr)]                                     # save new data set only including the not too correlated variables
print (reduced_data)

write.csv(reduced_data, file=outfile, row.names=TRUE, col.names=TRUE) # save your output

