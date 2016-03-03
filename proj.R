require(ROCR)
require(dplyr)
require(ggplot2)
require(glmnet)

# load tables

# biot_data <- read.table("BTK_BIOTECH.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")
# oil_gas_data <- read.table("XOI_OIL_GAS_IDX.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")
# housing_data <- read.table("HGX_HOUSING.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")
# security_data <- read.table("XBD_SECURITY_DEA.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")

# Response Variable: Tech Index
tech_data <- read.table("XCI_TECH_IDX.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")
# Correlate S&P, Shanghai Market, Hong Kong Market, Japanese Market, tax laws, interest rates, ...
s_p_data <- read.table("S&P.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")
nikkei_data <- read.table("NIKKEI.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")
shanghai_data <- read.table("SHANGHAI.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")
hong_kong_data <- read.table("HONG_KONG.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")

# Calculate Weekly % change
# Weekly % Change = (This week's price - last Week's price) / (last week's price * 100)
p_change <- function(d_frame) {
  # sort table by date
  d_frame <- arrange(d_frame, desc(format(Date, format="%Y-%B-%d")))
  d_frame$wp_change <- ((d_frame$Close - d_frame$Close[2:(nrow(d_frame)+1)]) / d_frame$Close[2:(nrow(d_frame)+1)])* 100
  d_frame[is.na(d_frame)] <- 0
  return(d_frame)
}

tech_data <- p_change(tech_data)
s_p_data <- p_change(s_p_data)
nikkei_data <- p_change(nikkei_data)
shanghai_data <- p_change(shanghai_data)
hong_kong_data <- p_change(hong_kong_data)

# normalize tables
primary_data <- select(inner_join(tech_data, s_p_data, by="Date"), Date, tech_wp_change=wp_change.x, sp_wp_change=wp_change.y)
primary_data <- select(inner_join(primary_data, nikkei_data, by="Date"), Date, tech_wp_change, sp_wp_change, nikkei_wp_change=wp_change)
primary_data <- select(inner_join(primary_data, shanghai_data, by="Date"), Date, tech_wp_change, sp_wp_change, nikkei_wp_change,shanghai_wp_change=wp_change)
primary_data <- select(inner_join(primary_data, hong_kong_data, by="Date"), Date, tech_wp_change, sp_wp_change, nikkei_wp_change,shanghai_wp_change, hong_kong_wp_change=wp_change)

# Beta(stock) = covariance(Stock's % weekly change, index's % weekly change) / Variance(index's % weekly change)
# ** these are irrelevant, just use model **
beta <- function(vec1, vec2) {
  return(cov(vec1,vec2)/var(vec2))
}

betas <- c(0, beta(primary_data$tech_wp_change, primary_data$sp_wp_change), 
           beta(primary_data$tech_wp_change, primary_data$nikkei_wp_change),
           beta(primary_data$tech_wp_change, primary_data$shanghai_wp_change),
           beta(primary_data$tech_wp_change, primary_data$hong_kong_wp_change))
betas[1] <- mean(primary_data$tech_wp_change) - mean(primary_data[,2])*betas[2] -
            mean(primary_data[,3])*betas[3]- mean(primary_data[,4])*betas[4] -
            mean(primary_data[,5])*betas[5]

# model of tech data vs various foreign market indices
tech_model <- lm(formula=tech_wp_change ~ 1+sp_wp_change+nikkei_wp_change+shanghai_wp_change+hong_kong_wp_change, data=primary_data)

# predictions
primary_data$p_tech_wp_change <- predict(tech_model, primary_data)

# analysis
rmse <- function(target, prediction) {
  return(sqrt(mean((target - prediction)^2)))
}

r2 <- function(target, prediction) {
  # cor(target, prediction)^2
  return(sum((prediction-mean(target))^2)/sum((target-mean(target))^2))
}

print(paste("RMSE of tech prediction:", rmse(primary_data$tech_wp_change, primary_data$p_tech_wp_change)))
print(paste("R^2 of tech prediction:",r2(primary_data$tech_wp_change, primary_data$p_tech_wp_change)))
