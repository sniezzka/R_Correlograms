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














data_numeric <- data

# Compute Spearman correlation matrix
cor_matrix <- cor(data_numeric, method = "spearman")

# Function to compute p-values for correlation matrix
cor.mtest <- function(mat, method = "spearman") {
  n <- ncol(mat)
  p_mat <- matrix(NA, n, n)
  rownames(p_mat) <- colnames(p_mat) <- colnames(mat)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      test <- cor.test(mat[, i], mat[, j], method = method)
      p_mat[i, j] <- p_mat[j, i] <- test$p.value
    }
  }
  
  return(p_mat)
}

# Compute p-values
p_matrix <- cor.mtest(data_numeric)

# Create a mask for the upper triangle (set non-significant values to NA)
upper_triangle <- cor_matrix
upper_triangle[p_matrix > 0.05] <- NA  # Keep only significant correlations

# Create a mask for the lower triangle (keep raw Spearman correlations)
lower_triangle <- cor_matrix
lower_triangle[upper.tri(lower_triangle)] <- NA  # Keep only lower triangle values

# Combine both matrices
combined_matrix <- ifelse(is.na(upper_triangle), lower_triangle, upper_triangle)

# Create a version of the correlation matrix without NAs for hierarchical clustering
cor_for_clustering <- combined_matrix
cor_for_clustering[is.na(cor_for_clustering)] <- 0  # Replace NA with 0

# Perform hierarchical clustering
hc <- hclust(as.dist(1 - cor_for_clustering), method = "average") 

sum(is.na(combined_matrix))  # Count NA values
sum(is.nan(combined_matrix)) # Count NaN values
sum(is.infinite(combined_matrix)) # Count Inf values

combined_matrix[is.na(combined_matrix)] <- 0  # Replace NA with 0
combined_matrix[is.nan(combined_matrix)] <- 0 # Replace NaN with 0
combined_matrix[is.infinite(combined_matrix)] <- 0 # Replace Inf with 0





# Replace all zero values with a small constant (avoiding log issues)
data[data == 0] <- 0.0001



# Plot correlogram with both weighted upper and unweighted lower triangles
corrplot(combined_matrix, 
         method = "circle",   
         type = "full",        
         order = "hclust",     
         hclust.method = "average",  
         tl.cex = 0.7,        
         col = colorRampPalette(c("blue", "white", "red"))(200),  
         addrect = 3,         
         tl.col = "black",    
         number.cex = 0.6,    
         pch.cex = 1.5,       
         pch.col = "black",
         sig.level = 0.05,
         insig = "blank"
)
