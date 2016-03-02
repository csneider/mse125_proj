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
# Correlate S&P, Chinese Market, Japanese Market, tax laws, interest rates, ...
s_p_data <- read.table("S&P.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")
tech_data <- read.table("XCI_TECH_IDX.csv", sep=",", header=TRUE, as.is = TRUE, quote="\"")


# normalize tables