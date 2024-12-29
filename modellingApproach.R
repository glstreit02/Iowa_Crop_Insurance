
###############################################################################
# Initialization
###############################################################################
source('/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Miscellaneous/Misc_functions.R') 
source("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/finalStrucutre/corrYields.R")
source('/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/finalStrucutre/correlatePrices.R')

load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/State-Price-Yield-Data-Correlations.RData")
load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/CROP-41-FARM-dbars.RData")
load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/CROP-81-FARM-dbars.RData")

#Calculate Soy and Corn Price Correlation
###############################################################################
HPPP <- df0$HP81 / df0$PP81
soyYields <- df0$yld81
cornYields <- df0$yld41

ypcor_soy <- cor(HPPP, soyYields)
ypcor_corn <- cor(HPPP, cornYields)

################################################################################
# Simulate prices using geometric Brownian motion
ivol_corn <- 0.24
ivol_soy <- 0.20
dT <- 8/12
N <- 10013
PP_price_corn <- 4.65
PP_price_soy <- 11.54

dG_corn <- (0 - 0.5 * ivol_corn^2) * dT + ivol_corn * rnorm(N) * sqrt(dT)  # Corn price shocks
corn_prices_sim <- PP_price_corn * exp(dG_corn)  # Simulated corn prices

dG_soy <- (0 - 0.5 * ivol_soy^2) * dT + ivol_soy * rnorm(N) * sqrt(dT)  # Soybean price shocks
soybean_prices_sim <- PP_price_soy * exp(dG_soy)  # Simulated soybean prices

###############################################################################

#Use function that correlates simulated de-trended county yields using HCOP process.This will also introduce correlation to the counties losses later on.
#Also simulated shocks to county yields as well.
countyYields <- corrYields()

#Generate the final master data table for simulations.
countyYields$simulated <- correlatePrices(countyYields$simulated[,3:ncol(countyYields$simulated)], soybean_prices_sim, corn_prices_sim, ypcor_soy, ypcor_corn)$data

#Function is used to simulate the premiums, and payouts for a hypothetical farm in a given county.
farm_sim <- function(farmQuantile, county_fips, CVG, PP_price_corn, PP_price_soy, acreCorn, acreSoy, seed) {
  
  #Set seed for reproducability
  set.seed(seed)
  
  #Retrieve keys used to acsess a counties specific simulated yield data for corn and soybeans.
  Keys <- countyYields$map[countyYields$map == county_fips,]

  # Extract historical and simulated yield data for corn and soybeans
  corn_yields <- unlist(countyYields$historic[Keys[[2]] +2]) # Detrended corn yield data
  soybean_yields <- unlist(countyYields$historic[Keys[[3]]+2]) # Detrended soybean yield data
  
  corn_yields_sim <- unlist(countyYields$simulated[,Keys[[2]]])
  soybean_yields_sim <- unlist(countyYields$simulated[,Keys[[3]]])

  #Simulated prices for both crops are the same across all counties.
  corn_prices_sim <- countyYields$simulated[,ncol(countyYields$simulated)]
  soy_prices_sim <- countyYields$simulated[,ncol(countyYields$simulated)-1]

  # Number of simulated shocks to generate
  N <- length(corn_yields_sim)

  # Apply Iman-Conover to correlate corn and soybean farm-level shocks. 
  #corn 
  load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/Crop-41-County-inverseCDF-datablocks.RData")
  cornETF <- ETF[, subset(df_etf, cfips == county_fips & type == 16 & prac == 3)$index]
  tmp_density <- density(cornETF)
  cornETF <- sample(tmp_density$x, N, prob = tmp_density$y, replace = TRUE)
  
  #soy
  load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/Crop-81-County-inverseCDF-datablocks.RData")
  soyETF <- ETF[, subset(df_etf, cfips == county_fips & type == 997 & prac == 93)$index]
  tmp_density <- density(soyETF)
  soyETF <- sample(tmp_density$x, N, prob = tmp_density$y, replace = TRUE)
  
  #Correlate the shocks with 0.25 coefficient
  combinedData <- cbind(cornETF, soyETF)

  combined_corr <- matrix(c(
    1, 0.25,
    0.25, 1), nrow = 2)
  corellated_shocks <- ImanConover( combinedData, combined_corr)  # Apply Iman-Conovers

  #Combine farm residuals and county yields
  corn_yields_sim <-  corn_yields_sim + corellated_shocks[,1]
  corn_yields_sim[corn_yields_sim < 0] <- 0 
  
  soybean_yields_sim <-  soybean_yields_sim + corellated_shocks[,2]
  soybean_yields_sim[soybean_yields_sim < 0] <- 0
  
  # Compute average historical yields for insurance calculations
  load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/CROP-41-FARM-dbars.RData")
  farm_dbar <- subset(dbarQ, cfips == county_fips & type == 16 & prac == 3)
  index = 4 + farmQuantile
  farm_dbar <- unlist(farm_dbar[,index])
  APH_corn <- unlist(round(mean(corn_yields), 0) + farm_dbar)
  
  load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/CROP-81-FARM-dbars.RData")
  farm_dbar <- subset(dbarQ, cfips == county_fips & type == 997 & prac == 93)
  index = 4 + farmQuantile
  farm_dbar <- unlist(farm_dbar[,index])
  APH_soybean <- unlist(round(mean(soybean_yields), 0) + farm_dbar)
  
  #Calculate liability amounts for revenue insurance product 
  LIAB_PP_corn <- CVG * PP_price_corn * APH_corn  # Liability under projected price for corn
  LIAB_PP_soybean <- CVG * PP_price_soy * APH_soybean  # Liability under projected price for soybeans
  LIAB_HP_corn <- CVG * corn_prices_sim * APH_corn  # Liability under harvest price for corn
  LIAB_HP_soybean <- CVG * soybean_prices_sim * APH_soybean  # Liability under harvest price for soybeans

  # Determine the maximum liability for each crop
  LIAB_corn <- pmax(LIAB_PP_corn, LIAB_HP_corn)  # Maximum liability for corn
  LIAB_soybean <- pmax(LIAB_PP_soybean, LIAB_HP_soybean)  # Maximum liability for soybeans

  # Compute revenue-to-count (RTC)
  RTC_corn <- corn_prices_sim * corn_yields_sim  # Revenue to count for corn
  RTC_soybean <- soybean_prices_sim * soybean_yields_sim  # Revenue to count for soybeans

  # Calculate indemnity payments (payouts)
  IPAY_corn <- pmax(0, LIAB_corn - RTC_corn)   # Indemnity for corn
  IPAY_soybean <- pmax(0, LIAB_soybean - RTC_soybean)  # Indemnity for soybeans

  # Calculate insurance premiums
  premium_level_corn <- mean(IPAY_corn) # Average indemnity for corn
  premium_level_soybean <- mean(IPAY_soybean) # Average indemnity for soybeans

  # Return results
  return(list(
     joint_Payments = (IPAY_corn * acreCorn + IPAY_soybean * acreSoy),
     joint_Premiums = rep((premium_level_corn * acreCorn + premium_level_soybean * acreSoy),N),
     corn_payments = (IPAY_corn * acreCorn),
     soy_payments = (IPAY_soybean * acreSoy),
     corn_premium = (rep((premium_level_corn),N) * acreCorn),
     soy_premium = (rep((premium_level_soybean),N) * acreSoy)
  ))

}

###############################################################################
#Demo of farm_sim() function
###############################################################################


result <- farm_sim(
  farmQuantile = 10, #specify the Farms relative performance compared to rest of county based on quantile.
  county_fips = 19169, #County Fips.
  CVG = 0.85, #Coverage level
  PP_price_corn = 4.65,  # projected price for corn in 2024.
  PP_price_soy = 11.54,  # projected price for soybeans in 2024.
  acreCorn = 640, #acres of corn per farmer
  acreSoy = 640, #acres of soy per farmer
  seed= 1234
)

#Verify that the output results make intuitive sense.
cat( "Corn", mean(result$corn_payments), mean(result$corn_premium) )
cat( "Soy", mean(result$soy_payments), mean(result$soy_premium) )
cat("Joint", mean(result$joint_Payments), mean(result$joint_Premiums))

cat( "Corn", quantile(result$corn_payments / result$corn_premium, 0.99) )
cat( "Soy", quantile(result$soy_payments / result$soy_premium, 0.99) )
cat( "Joint", quantile(result$joint_Payments / result$joint_Premiums, 0.99) )

iowa_counties = read.csv("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/IOWA_BOB_Counties.csv")


#Simulate the losses across all counties and the entire BOB.
simulate_loss_ratio_var <- function(iowa_counties, coverage_level, acreCorn, acreSoy, num_farms, nsims, rho_residual, seed) {
  
  set.seed(seed)  # Set seed for reproducibility
  print("Starting simulation for all counties...")
  
  # Initialize parameters
  results <- list()  # Store results for each county
  countyRatios <-  data.frame(matrix(ncol = length(iowa_counties$cname), nrow = nsims))

  BOB_Payments <- c(nsims)
  BOB_Premiums <- c(nsims)
  
  quantiles <- c(10.0, 18.9, 27.8, 36.7, 45.6, 54.4, 63.3, 72.2, 81.1, 90.0)

  # Iterate over each county
  for (county in iowa_counties$cname) {
    
    cat("Processing county:", county, "\n")  # Log the county being processed
    
    # Get the county FIPS code
    county_fips <- iowa_counties[iowa_counties$cname == county, "cfips"]
    
    # Simulate farm-level results for all farms in the county
    farm_loss_ratios <- numeric(num_farms)
    
    #Initialize Matrices that will store premiums and payments for all 10 farms in each county.
    Premiums = data.frame(matrix(ncol = num_farms, nrow = nrow(countyYields$simulated)))
    Payments = data.frame(matrix(ncol = num_farms, nrow = nrow(countyYields$simulated)))
    
    #for each farm compute 10,000 simulated losses 
    for (farm_id in 1:num_farms) {
      
      cat("  Processing farm:", farm_id, "for county:", county, "\n")
      
      result <- farm_sim(
        farmQuantile = quantiles[farm_id] , #Relative performance of farms APH compared to oother farms in county.
        county_fips = county_fips, #Specified county 
        CVG = coverage_level,  #Specified Coverage Level
        PP_price_corn = 4.65,  # projected price for corn in 2024.
        PP_price_soy = 11.54,  # projected price for soybeans in 2024.
        acreCorn = acreCorn, #acres of corn per farmer
        acreSoy = acreSoy, #acres of soy per farmer
        seed = seed # Ensure different random seed for each farm
      )
      
      #Add the simulated farms premiums and payments to county matrices.
      Premiums[,farm_id] <- (c(result$corn_premium) + c(result$soy_premium))
      Payments[,farm_id] <- result$soy_payments + result$corn_payments
    }
    
    #Compute total losses and premiums for all the farms in a county.
    countyPrem <- rowSums(Premiums)
    countyPay <- rowSums(Payments)
    
    #compute vector of potential county loss ratios.
    countyRatio <- countyPay/countyPrem
    
    #Add premiums and payments to vector representing cashflows in the entire BOB.
    BOB_Premiums <- BOB_Premiums + countyPrem
    BOB_Payments <- BOB_Payments + countyPay
    
    # Aggregate farm loss ratios for the county
    county_loss_ratio <- c(mean(countyRatio))  # Average loss ratio across all farms
    var_99 <- quantile(countyRatio, 0.99)  # 99% VaR
    var_996 <- quantile(countyRatio, 0.996)  # 99.6% VaR
    
    # Store results for the county
    results[[county]] <- list(
      average_loss_ratio = county_loss_ratio,
      var_99 = var_99,
      var_996 = var_996,
      ratioDist = countyRatio
    )
  }
  
  #Compute loss ratios for entire BOB, 99% VAR, and 99.6% VAR
  bobRatio <- BOB_Payments / BOB_Premiums
  bob_average_loss <- c(mean(bobRatio))
  var_99 <- quantile(bobRatio, 0.99)
  var_996 <- quantile(bobRatio, 0.996)
  
  results[["BOB"]] <- list(
     average_loss_ratio = bob_average_loss,
     var_99 = var_99,
     var_996 = var_996,
     ratioDist = bobRatio
  )
    
  print("Simulation completed for all counties.")
  return(results)
}

# Parameters for simulation to get individual county loss ratios and entire BOB loss ratio. 
coverage_level <- 0.85  # Coverage level (85%)
num_farms <-10 # Number of farms per county
nsims <- 10000  # Number of Monte Carlo simulations per farm
rho_residual <- 0.25  # Correlation of farm-level corn-soybean residuals
seed <- 1001  # Seed for reproducibility
acresCorn <- 640 #Specified number of corn acres
acresSoy <- 640 #specified number of soy acres.

# Run simulation
results <- simulate_loss_ratio_var(iowa_counties, coverage_level, acresCorn, acresSoy, num_farms, nsims, rho_residual, seed)

###############################################################################
# Output Results for county level loss ratios 
###############################################################################
for (county in names(results)) {
  cat("County:", county, "\n")
  cat("Average Loss Ratio:", results[[county]]$average_loss_ratio, "\n")
  cat("VaR 99% Loss Ratio:", results[[county]]$var_99, "\n")
  cat("VaR 99.6% Loss Ratio:", results[[county]]$var_996, "\n")
  cat("\n")
}

#Verify that the losses are still correlated among counties
countyDists = data.frame(matrix(ncol = length(names(results)), nrow = nrow(countyYields$simulated)))
iter = 1
for (county in names(results)){
  countyDists[,iter] = results[[county]]$ratioDist
  iter = 1+iter
}
cor(countyDists)

library(ggplot2)

#Prepare results for visualization.
plot_data <- data.frame(
  County = names(results),
  Average_Loss_Ratio = sapply(results, function(x) x$average_loss_ratio),
  VaR_99 = sapply(results, function(x) x$var_99),
  VaR_996 = sapply(results, function(x) x$var_996)
)

plot_data_BOB <- plot_data[plot_data$County == "BOB",]
plot_data <- plot_data[plot_data$County != "BOB",]


# Melt data for easier plotting
library(reshape2)
plot_data_melted <- melt(plot_data, id.vars = "County", 
                         variable.name = "Metric", 
                         value.name = "Value")

plot_data_melted_BOB <- melt(plot_data_BOB, id.vars = "County", 
                             variable.name = "Metric", 
                             value.name = "Value")

# Plots for all of the counties losses.
ggplot(plot_data_melted, aes(x = County, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Loss Ratios and VaR Quantiles by County",
    x = "County",
    y = "Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Individual Plot for BOB VARs 
ggplot(plot_data_melted_BOB, aes(x = County, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Loss Ratios and VaR Quantiles for BOB",
    y = "Value",
    fill = "Metric",
    x = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
