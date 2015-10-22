# Initialize environment
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"randomForest" %in% installed.packages()) install.packages("randomForest")
library(dplyr)
library(randomForest)

# Read data
data <- read.csv("DataScienceChallenge_Training.csv", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character", "character", "character", "character", "numeric"))

# Replace invalid customer values with numbers
RowsWithInvalidCustID <- grep("e+05", data$Cust_map, fixed=TRUE)
data$Cust_map[RowsWithInvalidCustID] <- gsub("e+05", "00000", data$Cust_map[RowsWithInvalidCustID], fixed=TRUE)
  
# Assign quarters to transactions
attach(data)
data$QTR[TXN_MTH == "201309" | TXN_MTH == "201310" | TXN_MTH == "201311"] <- 1
data$QTR[TXN_MTH == "201312" | TXN_MTH == "201401" | TXN_MTH == "201402"] <- 2
data$QTR[TXN_MTH == "201403" | TXN_MTH == "201404" | TXN_MTH == "201405"] <- 3
data$QTR[TXN_MTH == "201406" | TXN_MTH == "201407" | TXN_MTH == "201408"] <- 4
detach(data)
  
# Create factors for variables
data$QTR <- as.factor(data$QTR)
data$SPND_CATGY <- as.factor(data$SPND_CATGY)

# Create shells of quarterly data with unique customer and merchant IDs
uniqueCustID <- unique(data$Cust_map)
uniqueMerchID <- unique(data$Merch_Map_final)
#data_Q1 <- 

MerchData <- filter(data, QTR == 1) %>%
  group_by(Merch_Map_final, SPND_CATGY, QTR) %>%
  summarise(TotalTrans = sum(NumTrans)) %>%
  mutate(Q1.Rank = ave(TotalTrans, QTR))

# Summarize data by quarters