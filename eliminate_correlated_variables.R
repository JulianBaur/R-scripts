# This script was used to remove correlating environmental factors from a list obtained by GIS

# used packages
library(Hmisc)
library(caret)

# descriptive matrices of p-values and correlation coefficients (after pearson)

data <- as.matrix(input_file = "/path/to/your/infile.csv")  # remove specimen names etc, only keep the variables
corrmatrix <- rcorr(data, type = "pearson")       # analysis (from Hmisc)
correlation <- corrmatrix$r                       # save correlation coefficients separately
correlation[abs(correlation) > 0.85] <- "high"    # replace values over threshhold with sth
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

highcorr <- findCorrelation(corrmatrix$r, cutoff=0.85, exact = TRUE)    # output is a vector of column numbers of correlated variables
highcorr <- sort(highcorr)                                              # put them in order for removing them from the data set
reduced_data <- data[,-c(highcorr)]                                     # save new data set only including the not too correlated variables
print (reduced_data)

write.csv(reduced_data, file = "/path/to/your/outpt/file.whatever", row.names = FALSE, col.names = TRUE) # save your output

