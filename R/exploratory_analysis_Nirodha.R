library(dplyr)
library(ggplot2)

# Read the train data
train_data <- read.csv("Data/train.csv")
# Read the test data
test_data <- read.csv("Data/test.csv")

data <- cbind(train_data, test_data)

# Consider variables 1 to 38
train_ob <- train_data[, c(1:38, 81)]

# Summary of the variables
str(train_ob)

train_ob[sapply(train_ob, is.character)] <- lapply(train_ob[sapply(train_ob, is.character)], 
                                       as.factor)

# Select only factor variables
fact_var <- train_ob %>% 
  select_if(~class(.) == 'factor')

fact_var$response <- train_ob[, 39]

# Select only numeric variables
int_var <- train_ob %>% 
  select_if(~class(.) == 'integer')

int_var$response <- train_ob[, 39]

########################################################################
##############Plots###########################################

# response variable

hits <- ggplot(int_var, aes(x = SalePrice)) + 
  geom_histogram(color="black", fill="blue") + xlab("Sale Price") + ylab("Frequency")

# build year and sale price

int_var$YearBuilt <- as.character(int_var$YearBuilt)

box_build <- ggplot(int_var, 
                    aes(x = YearBuilt, y = SalePrice)) +
  geom_boxplot() +
  ylab("Sale Price") + xlab("Built Year") +
  theme(axis.text.x = element_text(angle = 45))

buit <- int_var %>%
  group_by(YearBuilt) %>%
  summarise(mean_sale = mean(SalePrice))

dot_build <- ggplot(buit, 
                    aes(x = YearBuilt, y = mean_sale)) +
  geom_boxplot() +
  ylab("Mean Sale Price") + xlab("Built Year") +
  theme(axis.text.x = element_text(angle = 90))

# LotArea

lot <- ggplot(int_var, aes(x = LotArea, y = SalePrice)) +
  geom_point() 

cor(int_var$LotArea, int_var$SalePrice)

# LotFrontage

lot_f <- ggplot(int_var, aes(x = LotFrontage, y = SalePrice)) +
  geom_point() 


