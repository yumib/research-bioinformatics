
#CALCULATING CORRELATION AND PLOTTING HISTOGRAM:

#read excel
library(readxl)
oglcnac_data = read_excel("OGlcNAc-copy.xlsx")

#delete last column Gene Symbol from data
# %>% syntax is called piping
library(dplyr)
olgcnac_filtered <- oglcnac_data %>% select(-c("Gene_ID"))

#transpose data
transposed = t(olgcnac_filtered)

#find the correlation values 
#i think btwn each col and row not individually like in python
correlation <- cor(transposed)

# stacks all of the values into a single line/vector
correlation_values <- as.vector(correlation)

# Plot histogram of correlation values
#main = title, xlab = x axis correlation values, ylab = frequency of values
hist(correlation_values, main = "Correlation Values Histogram of Oglycnac", xlab = "Correlation", ylab = "Frequency")