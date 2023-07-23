#Normalize phospho and oglyc 
#Find all oglyc values then use "Gene_ID" to identify the matching names
library(readxl)
library(dplyr)
oglyc_df = read_excel("OGlcNAc-copy.xlsx")
phosph_df = read_excel("total-protoeme-expression.xlsx", sheet = "total proteome")

#find unique values for phosph based on column Gene_ID
uniqcol_phosph = "Gene_ID"
uphosph_df = phosph_df[!duplicated(phosph_df[[uniqcol_phosph]]), ]


#extract the Gene_ID column
col1 = oglyc_df$Gene_ID
col2 = uphosph_df$Gene_ID

#check if values from oglycdata are found in phosph data
found_val = col1[col1 %in% col2]

match_col = "Gene_ID"

#These lines are what actually normalize the data, it basically creates a mega df with every column from both excel sheets but since the column names are duplicated, theres the .x and .y columns, all these lines do is subtract the .x - .y and add the difference to a different df
#for the new df, i have made the column names nColumn to not get confused with the original columns
#Also, it was simpler to not have spaces in the column names so I just added an 
#FFR
sub_data = oglyc_df %>% inner_join(uphosph_df, by = match_col) %>% mutate(nWT4_FFR = .data[[paste0("WT4 FFR.x")]] - .data[[paste0("WT4 FFR.y")]],nWT5_FFR = .data[[paste0("WT5 FFR.x")]] - .data[[paste0("WT5 FFR.y")]], nWT6_FFR = .data[[paste0("WT6 FFR.x")]] - .data[[paste0("WT6 FFR.y")]])
norm_df = data.frame(Gene_ID = sub_data$Gene_ID, nWT4_FFR = sub_data$nWT4_FFR, nWT5_FFR = sub_data$nWT5_FFR, nWT6_FFR = sub_data$nWT6_FFR)

#ISO
sub_data1 = oglyc_df %>% inner_join(uphosph_df, by = match_col) %>% mutate(nWT7ISO = .data[[paste0("WT7ISO.x")]] - .data[[paste0("WT7ISO.y")]])
norm_df = norm_df %>% mutate(data.frame(Gene_ID = sub_data1$Gene_ID, nWT7ISO = sub_data1$nWT7ISO)) 
#ObOb
sub_data2 = oglyc_df %>% inner_join(uphosph_df, by = match_col) %>% mutate(nObOb4FFR = .data[[paste0("ObOb4FFR.x")]] - .data[[paste0("ObOb4FFR.y")]], nObOb5FFR = .data[[paste0("ObOb5FFR.x")]] - .data[[paste0("ObOb5FFR.y")]], nObOb6FFR = .data[[paste0("ObOb6FFR.x")]] - .data[[paste0("ObOb6FFR.y")]], nObOb7ISO = .data[[paste0("ObOb7ISO.x")]] - .data[[paste0("ObOb7ISO.y")]])
norm_df = norm_df %>% mutate(data.frame(Gene_ID = sub_data2$Gene_ID, nObOb4FFR = sub_data2$nObOb4FFR, nObOb5FFR = sub_data2$nObOb5FFR, nObOb6FFR = sub_data2$nObOb6FFR, nObOb7ISO = sub_data2$nObOb7ISO))

#Histogram
norm_filt <- norm_df %>% select(-c("Gene_ID"))
norm_transp = t(norm_filt)
norm_correlation = cor(norm_transp)
norm_corr_values = as.vector(norm_correlation)
hist(norm_corr_values, main = "Normalized Hist", xlab = "Correlation", ylab = "Frequency")