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

  # Define the means for each variable (e.g., plant morphology traits)
  means <- c(15, 7, 100, 12, 50, 30, 0.5, 200, 3.5, 500)  # Approximate realistic means for plant traits

  # Define the covariance matrix for correlated variables
  cov_matrix <- matrix(c(
    5,  2,  3,  1,  2.5,  0.8,  0.3,  2,  0.4,  10,
    2,  3,  1.5,  0.8,  1.0,  0.5,  0.2,  1.5,  0.3,  5,
    3,  1.5, 6,  2,  2.5,  1.0,  0.5,  3,  0.8,  15,
    1,  0.8, 2,  4,  1.8,  0.7,  0.4,  1,  0.2,  4,
    2.5,  1.0, 2.5,  1.8,  10,  1.5,  0.6,  4,  0.9,  20,
    0.8,  0.5, 1.0,  0.7,  1.5,  2,  0.5,  1.5,  0.4,  7,
    0.3,  0.2, 0.5,  0.4,  0.6,  0.5,  0.2,  0.8,  0.1,  2,
    2,  1.5, 3,  1,  4,  1.5,  0.8,  12,  1,  30,
    0.4,  0.3, 0.8,  0.2,  0.9,  0.4,  0.1,  1,  3,  5,
    10,  5, 15,  4,  20,  7,  2,  30,  5,  100
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
  colnames(df) <- c("ID", "LeafLength", "LeafWidth", "LeafArea", "PetioleLength",
                    "StemHeight", "RootLength", "LeafThickness", "Biomass",
                    "ChlorophyllContent", "PlantVolume")

  # Combine some parameters and pivot the data for further analysis
  df %>%
    select(ID, LeafLength, LeafWidth, LeafArea, PetioleLength, StemHeight) %>%
    unite(LeafLength_LeafWidth, LeafLength, LeafWidth, sep="_") %>%
    unite(`LeafArea PetioleLength StemHeight`, LeafArea, PetioleLength, StemHeight, sep=" ") %>%
    pivot_longer(cols = c(LeafLength_LeafWidth, `LeafArea PetioleLength StemHeight`), names_to = "parameter", values_to = "estimates") %>%
    write_csv("plant_dataset_01.csv")

  df %>%
    select(ID, RootLength, LeafThickness,Biomass, ChlorophyllContent, PlantVolume) %>%
    unite(RootLength_LeafThickness_Biomass_ChlorophyllContent_PlantVolume, RootLength, LeafThickness,Biomass, ChlorophyllContent, PlantVolume, sep="_") %>%
    mutate(parameter = "RootLength_LeafThickness_Biomass_ChlorophyllContent_PlantVolume") %>%
    pivot_wider(names_from = "ID", values_from = "RootLength_LeafThickness_Biomass_ChlorophyllContent_PlantVolume") %>%
    write_csv("plant_dataset_02.txt")
}
