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

  # Define the means for each variable (e.g., bird morphology traits)
  means <- c(20, 4, 30, 15, 8, 300, 10, 60, 2, 5.0)  # Approximate realistic means for bird traits

  # Define the covariance matrix for correlated variables
  cov_matrix <- matrix(c(
    5,  1,  3,  2,  1.5,  10,  1,  4,  0.5,  2,
    1,  2,  1,  0.5, 0.3,  5,   0.6, 1.5, 0.2,  0.8,
    3,  1,  4,  1.2, 1.0,  8,   1.2, 2,   0.7,  1.5,
    2,  0.5, 1.2, 3.5, 1.8, 6,   0.9, 1.8, 0.6,  1.2,
    1.5, 0.3, 1.0, 1.8, 4,  5.5, 1.1, 2.5, 0.4,  0.9,
    10, 5,  8,  6,  5.5,  30,  2,  10, 1.5,  6,
    1,  0.6, 1.2, 0.9, 1.1, 2,   1.8, 2.3, 0.3,  0.5,
    4,  1.5, 2,  1.8, 2.5, 10,  2.3, 15,  0.6,  2.5,
    0.5, 0.2, 0.7, 0.6, 0.4, 1.5, 0.3, 0.6, 1.2,  0.6,
    2,  0.8, 1.5, 1.2, 0.9, 6,   0.5, 2.5, 0.6,  4.5
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
  colnames(df) <- c("ID", "WingLength", "BeakLength", "BodyLength", "TailLength",
                    "LegLength", "Weight", "BodyWidth", "WingSpan", "BeakDepth",
                    "BodyMassIndex")

  # Combine size and shape-related parameters and pivot the data for analysis
  df %>%
    select(ID, WingLength, BeakLength, BodyLength, TailLength, LegLength) %>%
    unite(WingLength_BeakLength, WingLength, BeakLength, sep="_") %>%
    unite(`BodyLength TailLength LegLength`, BodyLength, TailLength, LegLength, sep=" ") %>%
    pivot_longer(cols = c(WingLength_BeakLength, `BodyLength TailLength LegLength`), names_to = "parameter", values_to = "estimates") %>%
    write_csv("bird_dataset_01.csv")

  df %>%
    select(ID, Weight, BodyWidth, WingSpan, BeakDepth, BodyMassIndex) %>%
    unite(Weight_BodyWidth_WingSpan_BeakDepth_BodyMassIndex, Weight, BodyWidth, WingSpan, BeakDepth, BodyMassIndex) %>%
    mutate(parameter = "Weight_BodyWidth_WingSpan_BeakDepth_BodyMassIndex") %>%
    pivot_wider(names_from = "ID", values_from = "Weight_BodyWidth_WingSpan_BeakDepth_BodyMassIndex") %>%
    write_csv("bird_dataset_02.txt")
}
