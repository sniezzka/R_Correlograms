# Install packages if not already installed
install.packages(c("readxl", "compositions", "corrplot", "vegan", "MASS", "ggplot2", "ggrepel", "scales", "dplyr"))

# Load the required libraries
library(readxl)      # To read Excel files
library(compositions) # CLR transformation
library(corrplot)    # Correlation matrix and correlogram
library(vegan)       # Multivariate analysis
library(MASS)        # Classical MDS
library(ggplot2)     # Visualization
library(ggrepel)     # To avoid label overlapping
library(scales)      # Additional visualization tools
library(dplyr)       # Data wrangling

# Load the Excel file (modify the path to your actual file)
file_path <- "~/Documents/Doktorat/R/isoGDGTs.xlsx"  # Change this to your actual file path

# Read data (assuming first sheet)
data <- read_excel(file_path, sheet = 1)

# Convert to a data frame
data <- as.data.frame(data)

# Check the first few rows
head(data)

# Convert character columns to factors (if needed)
data <- data %>% mutate_if(is.character, as.factor)

# Replace all 0 values with a small number to avoid log issues
data[data == 0] <- 0.0001

# Remove rows with too many missing values (if needed)
data <- data[rowSums(is.na(data)) < ncol(data) * 0.5, ]

# Convert data to numeric (except sample names)
data_numeric <- data %>% select_if(is.numeric)

# Check for negative values (Bray-Curtis cannot handle them)
if (any(data_numeric < 0)) {
  stop("Negative values detected! Bray-Curtis cannot handle negative values. Please check data.")
}

# Check if "Sample" column exists
if (!"Sample" %in% colnames(data)) {
  stop("Error: 'Sample' column is missing from the dataset. Please add it before proceeding.")
}

# Set "Sample" as row names and remove it from the dataset
rownames(data) <- data$Sample
data$Sample <- NULL  # Remove the column

plot.new()  # Start a new blank plot
dev.off()   # Reset graphics device

# Apply Spearman correlation (handles non-normal data)
cor_matrix <- cor(data_numeric, method = "spearman", use = "pairwise.complete.obs")

# Function to compute p-values safely
cor.mtest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test_result <- suppressWarnings(tryCatch(
        cor.test(mat[, i], mat[, j], method = "spearman"),
        error = function(e) NA
      ))
      p.mat[i, j] <- p.mat[j, i] <- ifelse(is.list(test_result), test_result$p.value, NA)
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

# Compute p-values
p_matrix <- cor.mtest(data_numeric)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", p.mat = p_matrix, sig.level = 0.05, insig = "blank")

corrplot(cor_matrix, method = "circle", p.mat = p_matrix, sig.level = 0.05, insig = "blank", tl.cex = 0.5)

