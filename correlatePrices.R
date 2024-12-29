
source('/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Miscellaneous/Misc_functions.R') 

#Given a matrix of simulated county yields return a matrix that preserves the original correlation between the county 
#yields while correlating each of the yields to its respective harvest price.
correlatePrices <- function( data, soy_price_sim, corn_price_sim, soy_price_cor, corn_price_cor, seedval = 1234) {
  
  library(MASS)
  
  # Add soy_price_sim and corn_price_sim to the dataset
  data$soy_price_sim <- soy_price_sim
  data$corn_price_sim <- corn_price_sim
  
  # Compute original correlation   matrix.
  originalMatrix <- cor(data)
  
  # Find columns with soy and corn in their names
  soy_cols <- grep("soy", names(data), value = TRUE)
  corn_cols <- grep("corn", names(data), value = TRUE)
  
  # Rebuild the correlation matrix so corn and soy yields have the specified correlation with their harvest prices.
  corr_matrix <- originalMatrix
  for (soy_col in soy_cols) {
    if (soy_col != "soy_price_sim") {
      corr_matrix["soy_price_sim", soy_col] <- soy_price_cor
      corr_matrix[soy_col, "soy_price_sim"] <- soy_price_cor
     }
   }
  
  for (corn_col in corn_cols) {
    if (corn_col != "corn_price_sim") {
      corr_matrix["corn_price_sim", corn_col] <- corn_price_cor
      corr_matrix[corn_col, "corn_price_sim"] <- corn_price_cor
    }
   }
  
  # Ensure the rebuilt correlation matrix is positive definite.
  eig <- eigen(corr_matrix)
  eig$values[eig$values <= 0] <- 1e-4
  corr_matrix <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  
  #Introduce the new correlation structure to the simulated data via Iman-Conover. 
  #This allows for correlation between counties to be preserved while also making yields correlated with price
  rebuiltMatrix <- ImanConover(as.matrix(data), sigma = corr_matrix, seedval = seedval)
  
  # Return the re-correlated data.
  return(list(data = rebuiltMatrix, corr_matrix = rebuiltMatrix))
}

