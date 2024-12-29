
corrYields <- function(nsims = 10000)
  {
  source('/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Miscellaneous/Misc_functions.R')
  
  #Load in book of business to get required counties.
  BOB <- read.csv("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/IOWA_BOB_Counties.csv")
  
  # Load corn and soybean yield data for Story County, Iowa.
  load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/RMA-Yields-Corn.RData")
  
  #Load the PP and HP data.
  load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/YPCOR_PPHP_IVOL_Data.RData")
  
  
  #We only have corn and soybean yields from 1993-2023 so we can only use the PP and HP values from this period.
  pphp_subset <- subset(PPHP, yr >= 1993 & yr <= 2023)
  
  n <- nrow(pphp_subset)
  NL <- nsims # Desired number of simulations
  # Total number of simulated values. Ensures that the number is a multiple of the length of the original data.
  nreps <- floor(NL / n) + 1
  N <- n * nreps
  
  #Calculate PP and HP ratios.
  corn_ratio  <- pphp_subset$HP41 / pphp_subset$PP41
  soybean_ratio  <- pphp_subset$HP81 / pphp_subset$PP81
  
  # Get simulated PP/HP ratios for Corn and Soybeans by sampling from their historical distributions.
  tmp_corn_ratio <- density(corn_ratio)
  sim_corn_ratio <- sample(tmp_corn_ratio$x, N, prob=tmp_corn_ratio$y, replace=TRUE)
  
  tmp_soy_ratio <- density(soybean_ratio)
  sim_soy_ratio <- sample(tmp_soy_ratio$x, N, prob=tmp_soy_ratio$y, replace=TRUE)
  
  #Populate initial values of simulated data and historical data matrices 
  simulated_data <- data.frame(cornHPPP = sim_corn_ratio, soyHPPP = sim_soy_ratio)
  historical_data <- data.frame(cornHPPP = corn_ratio, soyHPPP = soybean_ratio)
  
  #Create vectors that will be used for mapping a county fips code to the correct columns in the historical_data and simulated_data matrices. 
  counties <- c()
  cornMap <- c()
  soyMap <- c()
  iter <- -3
  
  #Extract the detrended corn and soybean yields for each county and store its historical data and simulated versions in the corresponding matrix.
  for (fips in BOB$cfips) {
    iter <- iter + 2
    load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/RMA-Yields-Corn.RData")
    df_corn <- subset(CYLDS, cropcode == 41 & cfips == fips & type == 16 & prac == 3)
    
    load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/RMA-Yields-Soybeans.RData")
    df_soybean <- subset(CYLDS, cropcode == 81 & cfips == fips & type == 91 & prac == 3)
    
    # Extract yields for simulation
    corn_yields <- df_corn$yld_detrend
    soybean_yields <- df_soybean$yld_detrend
    
    # Get simulated corn yields by sampling from historical distribution
    tmp_corn <- density(corn_yields)
    sim_corn <- sample(tmp_corn$x, N, prob = tmp_corn$y, replace = TRUE)
    
    # Get simulated soybean yields by sampling from historical distribution
    tmp_soy <- density(soybean_yields)
    sim_soy <- sample(tmp_soy$x, N, prob = tmp_soy$y, replace = TRUE)
    
    # Dynamically name columns for simulated and historical data
    sim_corn_name <- paste0("sim_corn_", fips)
    sim_soy_name <- paste0("sim_soy_", fips)
    corn_yields_name <- paste0("corn_yields_", fips)
    soybean_yields_name <- paste0("soybean_yields_", fips)
    
    # Add dynamically named columns to the matrices
    simulated_data <- cbind(simulated_data, setNames(data.frame(sim_corn), sim_corn_name))
    simulated_data <- cbind(simulated_data, setNames(data.frame(sim_soy), sim_soy_name))
    historical_data <- cbind(historical_data, setNames(data.frame(corn_yields), corn_yields_name))
    historical_data <- cbind(historical_data, setNames(data.frame(soybean_yields), soybean_yields_name))
    
    # Update other tracking variables
    counties <- c(counties, fips)
    cornMap <- c(cornMap, iter + 2)
    soyMap <- c(soyMap, iter + 3)
    cat(fips, " ")
  }
  
  
  #combine vectors to get mappings where the row containing a particular county fips 
  #maps to the indexes corresponding to that counties corn and soybean yields in the spatially indexed matrices.
  Map <- data.frame(counties,cornMap,soyMap)
  
  ###############################################################################
  # Historical Copula Smoothing for Story County
  ###############################################################################
  
  # Set seed for reproducability
  set.seed(1001)
  
  # Combine original data and generate nreps of it and store in HCOP matrix.
  HCOP <- historical_data[rep(1:n, nreps), ]
  
  # identify smoothing factors.
  sdevs <- apply(HCOP, 2, sd)
  s_scale <- 0.25
  
  # Apply smoothing to HCOP matrix one column at a time.
  #Introduces noise to the rank orderings that gives the historic copula a cloud like shape around each original observation.
  for (j in 1:ncol(simulated_data)) {
    HCOP[, j] <- HCOP[, j] + s_scale * rnorm(N, mean=0, sd=sdevs[j])
  }
  
  # Sort each simulated column by the rank order of the corresponding column in the HCOP matrix.
  #This effectively introduces the HCOPs correlation structure to the simulated data.
  for (j in 1:ncol(simulated_data)) {
    sorted_data <- sort(simulated_data[, j])
    simulated_data[, j] <- sorted_data[rank(HCOP[, j], ties.method='first')]
  }
  return(list("simulated" = simulated_data, "historic" = historical_data, "map" = Map))
}
