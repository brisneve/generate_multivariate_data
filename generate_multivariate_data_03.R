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

  # Define the means for each variable (e.g., fish size, weight, and shape traits)
  means <- c(60, 20, 15, 25, 30, 2000, 12, 10, 45, 5.0)  # Approximate realistic means for fish traits

  # Define the covariance matrix for correlated variables
  cov_matrix <- matrix(c(
    50, 20, 15, 10, 5, 40, 8, 10, 15, 5,
    20, 30, 10, 8, 4, 25, 6, 8, 12, 3,
    15, 10, 25, 6, 5, 18, 7, 5, 10, 4,
    10, 8, 6, 20, 8, 15, 4, 6, 8, 3,
    5, 4, 5, 8, 12, 10, 3, 4, 6, 2,
    40, 25, 18, 15, 10, 100, 12, 15, 20, 8,
    8, 6, 7, 4, 3, 12, 8, 3, 5, 2,
    10, 8, 5, 6, 4, 15, 3, 9, 8, 4,
    15, 12, 10, 8, 6, 20, 5, 8, 30, 6,
    5, 3, 4, 3, 2, 8, 2, 4, 6, 7
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
  colnames(df) <- c("ID", "BodyLength", "BodyWidth", "BodyHeight", "FinLength",
                    "TailLength", "Weight", "BodyDepth", "HeadLength", "Girth",
                    "BodyMassIndex")

  # Combine size and weight-related parameters and pivot the data for analysis
  df %>%
    select(ID, BodyLength, BodyWidth, BodyHeight, FinLength, TailLength) %>%
    unite(BodyLength_BodyWidth, BodyLength, BodyWidth, sep="_") %>%
    unite(`BodyHeight FinLength TailLength`, BodyHeight, FinLength, TailLength, sep=" ") %>%
    pivot_longer(cols = c(BodyLength_BodyWidth, `BodyHeight FinLength TailLength`), names_to = "parameter", values_to = "estimates") %>%
    write_csv("fish_dataset_01.csv")

  df %>%
    select(ID, Weight,BodyDepth,HeadLength,Girth,BodyMassIndex) %>%
    unite(Weight_BodyDepth_HeadLength_Girth_BodyMassIndex, Weight,BodyDepth,HeadLength,Girth,BodyMassIndex) %>%
    mutate(parameter = "Weight_BodyDepth_HeadLength_Girth_BodyMassIndex") %>%
    pivot_wider(names_from = "ID", values_from = "Weight_BodyDepth_HeadLength_Girth_BodyMassIndex") %>%
    write_csv("fish_dataset_02.txt")
}
