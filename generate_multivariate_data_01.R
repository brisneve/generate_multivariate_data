generate_multivariate_data <- function(student_id){
  if (!require("MASS")) {
    install.packages("MASS", dependencies = TRUE)
    library(MASS)
  }

  if (!require("tidyverse")) {
    install.packages("tidyverse", dependencies = TRUE)
    library(tidyverse)
  }


  # Function to make covariance matrix positive definite
  make_positive_definite <- function(cov_matrix) {
    eigen_decomp <- eigen(cov_matrix)
    eigen_decomp$values <- pmax(eigen_decomp$values, 0)  # Set negative eigenvalues to 0
    cov_matrix_pd <- eigen_decomp$vectors %*% diag(eigen_decomp$values) %*% t(eigen_decomp$vectors)
    return(cov_matrix_pd)
  }

  # Define the means for each variable
  means <- c(30, 18, 14, 7, 6, 0.6, 1.5, 3.5, 12, 25)  # Approximate realistic means for snail shell morphology

  # Define the covariance matrix for correlated variables
  cov_matrix <- matrix(c(
    10,  6,  5,  2,  4,  0.3,  0.8,  0.4,  4,  7,
    6,  8,  4,  2,  3,  0.2,  0.7,  0.3,  3,  6,
    5,  4,  6,  3,  3,  0.2,  0.6,  0.3,  3,  5,
    2,  2,  3,  2,  1,  0.1,  0.3,  0.2,  1,  2,
    4,  3,  3,  1,  4,  0.2,  0.5,  0.2,  3,  4,
    0.3, 0.2, 0.2, 0.1, 0.2, 0.04, 0.08, 0.05, 0.2, 0.3,
    0.8, 0.7, 0.6, 0.3, 0.5, 0.08, 0.25, 0.1, 0.6, 0.7,
    0.4, 0.3, 0.3, 0.2, 0.2, 0.05, 0.1, 0.2, 0.3, 0.4,
    4,  3,  3,  1,  3,  0.2,  0.6,  0.3,  6,  5,
    7,  6,  5,  2,  4,  0.3,  0.7,  0.4,  5,  9
  ), nrow=10, byrow=TRUE)

  # Make the covariance matrix positive definite
  cov_matrix <- make_positive_definite(cov_matrix)

  # Simulate data using multivariate normal distribution
  set.seed(student_id)
  simulated_data <- mvrnorm(n = 100, mu = means, Sigma = cov_matrix)

  # Convert to data frame
  df <- as.data.frame(simulated_data) %>%
    rownames_to_column("ID")

  # Set column names
  colnames(df) <- c("ID","ShellLength", "ShellWidth", "ApertureHeight", "ApertureWidth", "WhorlIndex",
                    "ShellThickness", "ShellMass", "SpireHeight", "OperculumLength", "ShellSurfaceArea")

  df %>%
    select(ID, ShellLength, ShellWidth, ApertureHeight, ApertureWidth, WhorlIndex) %>%
    unite(ShellLength_ShellWidth, ShellLength, ShellWidth, sep="_") %>%
    unite(`ApertureHeight ApertureWidth WhorlIndex`, ApertureHeight, ApertureWidth, WhorlIndex, sep=" ") %>%
    pivot_longer(cols = c(ShellLength_ShellWidth, `ApertureHeight ApertureWidth WhorlIndex`), names_to = "parameter", values_to = "estimates") %>%
    write_csv("snail_dataset_01.csv")

  df %>%
    select(ID, ShellThickness, ShellMass, SpireHeight, OperculumLength, ShellSurfaceArea) %>%
    unite(ShellThickness_ShellMass_SpireHeight_OperculumLength_ShellSurfaceArea,
          ShellThickness, ShellMass, SpireHeight, OperculumLength, ShellSurfaceArea) %>%
    mutate(parameter = "ShellThickness_ShellMass_SpireHeight_OperculumLength_ShellSurfaceArea") %>%
    pivot_wider(names_from = "ID", values_from = "ShellThickness_ShellMass_SpireHeight_OperculumLength_ShellSurfaceArea") %>%
    write_csv("snail_dataset_02.txt")
}
