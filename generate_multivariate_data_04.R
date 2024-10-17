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

  # Define the means for each variable (e.g., algal cell size, weight, and shape traits)
  means <- c(10, 5, 100, 200, 1.5, 0.02, 0.03, 0.5, 2, 0.9)  # Approximate realistic means for algal cell traits

  # Define the covariance matrix for correlated variables
  cov_matrix <- matrix(c(
    2,  1,  5,  6,  0.2,  0.05, 0.06, 0.8, 0.3, 0.1,
    1,  3,  4,  5,  0.1,  0.03, 0.04, 0.6, 0.2, 0.05,
    5,  4, 10, 8,  0.3,  0.06, 0.07, 1.0, 0.5, 0.2,
    6,  5,  8, 15, 0.25, 0.07, 0.09, 1.5, 0.4, 0.15,
    0.2, 0.1, 0.3, 0.25, 0.5,  0.02, 0.03, 0.15, 0.08, 0.02,
    0.05, 0.03, 0.06, 0.07, 0.02, 0.005, 0.006, 0.02, 0.01, 0.005,
    0.06, 0.04, 0.07, 0.09, 0.03, 0.006, 0.008, 0.03, 0.015, 0.006,
    0.8,  0.6,  1.0, 1.5, 0.15, 0.02, 0.03, 3.0, 0.5, 0.3,
    0.3,  0.2,  0.5, 0.4, 0.08, 0.01, 0.015, 0.5, 0.2, 0.1,
    0.1, 0.05, 0.2, 0.15, 0.02, 0.005, 0.006, 0.3, 0.1, 0.4
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
  colnames(df) <- c("ID", "CellLength", "CellWidth", "CellVolume", "SurfaceArea",
                    "ChlorophyllContent", "DryWeight", "WetWeight", "Biomass",
                    "AspectRatio", "Sphericity")

  # Combine size and shape-related parameters and pivot the data for analysis
  df %>%
    select(ID, CellLength,CellWidth,CellVolume,SurfaceArea,ChlorophyllContent) %>%
    unite(CellLength_CellWidth, CellLength, CellWidth, sep="_") %>%
    unite(`CellVolume SurfaceArea ChlorophyllContent`, CellVolume,SurfaceArea,ChlorophyllContent, sep=" ") %>%
    pivot_longer(cols = c(CellLength_CellWidth, `CellVolume SurfaceArea ChlorophyllContent`), names_to = "parameter", values_to = "estimates") %>%
    write_csv("algae_dataset_01.csv")

  df %>%
    select(ID, DryWeight, WetWeight, Biomass, AspectRatio, Sphericity) %>%
    unite(DryWeight_WetWeight_Biomass_AspectRatio_Sphericity, DryWeight, WetWeight, Biomass, AspectRatio, Sphericity) %>%
    mutate(parameter = "DryWeight_WetWeight_Biomass_AspectRatio_Sphericity") %>%
    pivot_wider(names_from = "ID", values_from = "DryWeight_WetWeight_Biomass_AspectRatio_Sphericity") %>%
    write_csv("algae_dataset_02.txt")
}
