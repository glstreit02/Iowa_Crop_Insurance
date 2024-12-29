#Load in required data and custom functions.

source('/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Miscellaneous/Misc_functions.R') 
source("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/finalStrucutre/corrYields.R")

load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/State-Price-Yield-Data-Correlations.RData")

#Function is used to simulate the premiums, and payouts for a hypothetical farm in a given county.
ratings_engine <- function(code, county_fips, APH, CVG, PP_Price, ivol, dT, ypcor, seed, nsims) {
  
  #Obtain simulated de-trended yields for each county in a spatially indexed matrix.
  countyYields <- corrYields(nsims = nsims)
 
  #Set seed for reproducability.
  set.seed(seed)
  
  #Retrieve keys used to acsess a counties specific simulated yield data for corn and soybeans.
  Keys <- countyYields$map[countyYields$map == county_fips,]
  
  # Extract historical and simulated yield data 
  if(code == 41){
    yields <- unlist(countyYields$historic[Keys[[2]] +2]) 
    yields_sim <- unlist(countyYields$simulated[,Keys[[2]]])

  }
  else if(code == 81){
    yields <- unlist(countyYields$historic[Keys[[3]]+2])
    yields_sim <- unlist(countyYields$simulated[,Keys[[3]]])
  }
  
  N <- length(yields_sim)
  
  #Create future simulated harvest prices.
  dG <- (0 - 0.5 * ivol^2) * dT + ivol * rnorm(N) * sqrt(dT)  # price shocks
  prices_sim <- PP_Price * exp(dG)  # Simulated prices
  
  #Extract appropriate shock to apply to simulated yields
  if(code == 41){
    load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/Crop-41-County-inverseCDF-datablocks.RData")
    ETF <- ETF[, subset(df_etf, cfips == county_fips & type == 16 & prac == 3)$index]
    tmp_density <- density(ETF)
    ETF <- sample(tmp_density$x, N, prob = tmp_density$y, replace = TRUE)
  }
  
  else if(code == 81){
    load("/Users/garrett/Desktop/Strategy/2024/Q3/Academic_Plan/Fall_2024/EFIN_401/Crop_Insurance_Project/Data/Crop-81-County-inverseCDF-datablocks.RData")
    ETF <- ETF[, subset(df_etf, cfips == county_fips & type == 997 & prac == 93)$index]
    tmp_density <- density(ETF)
    ETF <- sample(tmp_density$x, N, prob = tmp_density$y, replace = TRUE)
  }
  
  #Combine farm residuals and county yields
  yields_sim <-  yields_sim + ETF
  yields_sim[yields_sim < 0] <- 0 
  
  #Correlate simulated yields with harvest prices.
  combined_corr <- matrix(c(
    1, ypcor,
    ypcor, 1), nrow = 2)
  
  correlatedData <- ImanConover(cbind(yields_sim,prices_sim), combined_corr)
  yields_sim <- correlatedData[,1]
  prices_sim <- correlatedData[,2]
  
  #Calculate liability amounts for revenue insurance product 
  LIAB_PP <- CVG * PP_Price* APH  # Liability under projected price
  LIAB_HP <- CVG * prices_sim * APH  # Liability under harvest price 

  # Determine the maximum liability 
  LIAB <- pmax(LIAB_PP, LIAB_HP)

  # Compute revenue-to-count (RTC)
  RTC <- prices_sim * yields_sim
  
  # Calculate indemnity payments (payouts)
  IPAY <- pmax(0, LIAB - RTC) 

  # Calculate insurance premiums
  premium_level <- mean(IPAY)

  # Return Premium Level and Premium Rate
  return(list(
    premium_level = premium_level,
    premium_rate = premium_level / LIAB_PP
  ))
  
}


#Demos

code <- 41
fips <- 19169
APH <- 200
CVG <- 0.85
PP_Price <- 4.50
ivol <- 0.22
dT <- 8/12
ypCor <- 0.25
seed <- 1023
nsims <- 10000

#Example Run for Corn in Story County for a farm with APH approxiamtley equal to county average.
results<- ratings_engine(code, fips, APH, CVG, PP_Price, ivol, dT, ypCor, seed, nsims)
print(results)

code <- 81
fips <- 19169
APH <- 57
CVG <- 0.85
PP_Price <- 11
ivol <- 0.24
dT <- 8/12
ypCor <- 0.25
seed <- 1025
nsims <- 10000

#Example Run for Soy in Story County for a farm with APH approxiamtley equal to county average.
results<- ratings_engine(code, fips, APH, CVG, PP_Price, ivol, dT, ypCor, seed, nsims)
print(results)


