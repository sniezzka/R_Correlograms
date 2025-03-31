install.packages(c("readxl", "compositions", "corrplot", "vegan", "MASS", "ggplot2", "ggrepel", "scales", "dplyr"))

library(readxl)      
library(compositions) 
library(corrplot)    
library(vegan)       
library(MASS)        
library(ggplot2)     
library(ggrepel)     
library(scales)      
library(dplyr)       








# Load the Excel file
file_path <- "~/Documents/Doktorat/R/isoGDGTs.xlsx"  # Modify with correct path
data <- read_excel(file_path, sheet = 1) %>% as.data.frame()

# Convert character columns to factors
data <- data %>% mutate_if(is.character, as.factor)

# Replace all zero values with a small constant (avoiding log issues)
data[data == 0] <- 0.0001

# Remove rows with excessive missing values
data <- data[rowSums(is.na(data)) < ncol(data) * 0.5, ]

# Keep only numeric data
data_numeric <- data %>% select_if(is.numeric)

# Ensure there are no negative values
if (any(data_numeric < 0)) {
  stop("Negative values detected! Please check data.")
}

# Set "Sample" as row names if it exists
if ("Sample" %in% colnames(data)) {
  rownames(data) <- data$Sample
  data$Sample <- NULL
}









# Compute Spearman correlation matrix
cor_matrix <- cor(data_numeric, method = "spearman", use = "pairwise.complete.obs")


library(Hmisc)

cor.mtest <- function(mat, method = "spearman", nperm = 1000) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test_result <- rcorr(mat[, i], mat[, j], type = method)
      p.mat[i, j] <- p.mat[j, i] <- test_result$P[1, 2]
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

p_matrix <- cor.mtest(data_numeric)








# Make sure you have the latest version
install.packages("corrplot")

# Use col.lim in the latest version
corrplot(cor_matrix, 
         method = "circle",   
         type = "upper",      
         order = "hclust",    
         tl.cex = 0.7,        
         col = colorRampPalette(c("blue", "white", "red"))(200),  
         p.mat = p_matrix,    
         sig.level = 0.05,    
         insig = "blank",     
         addrect = 3,         
         tl.col = "black",    
         col.lim = c(-1, 1),  # Corrected for newer versions
         number.cex = 0.6,    
         pch.cex = 1.5,       
         pch.col = "black"    
)
