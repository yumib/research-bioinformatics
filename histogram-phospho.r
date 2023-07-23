#read excel
library(readxl)
phospho_data = read_excel("phospho-copy.xlsx", sheet = "Phosphoproteome")

#delete last column Gene Symbol from data
library(dplyr)
phospho_filtered <- phospho_data %>% select(-c("Gene Symbol"))

#transpose data
transposed = t(phospho_filtered)

#find the correlation values 
#i think btwn each col and row not individually like in python
correlation <- cor(transposed)

# stacks all of the values into a single line/vector
correlation_values <- as.vector(correlation)

# Plot histogram of correlation values
#main = title, xlab = x axis correlation values, ylab = frequency of values
hist(correlation_values, main = "Correlation Values Histogram of Phosphorylation", xlab = "Correlation", ylab = "Frequency")