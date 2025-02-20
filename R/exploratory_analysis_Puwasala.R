library(tidyverse)

train_data <- read.csv(file = here::here("Data", "train.csv"))
test_data <- read.csv(file = here::here("Data", "test.csv"))

myvars <- names(train_data)[(40:81)]

train_data_sub <- train_data %>% 
  select(all_of(myvars))

train_data_sub <- train_data_sub %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate(BsmtFullBath = factor(BsmtFullBath),
         BsmtHalfBath = factor(BsmtHalfBath),
         FullBath = factor(FullBath),
         HalfBath = factor(HalfBath),
         BedroomAbvGr = factor(BedroomAbvGr),
         KitchenAbvGr = factor(KitchenAbvGr),
         TotRmsAbvGrd = factor(TotRmsAbvGrd),
         Fireplaces = factor(Fireplaces))

predictors <- names(train_data_sub) 
predictors <- predictors[!predictors %in% c("SalePrice")]

dependent <- "SalePrice"

train_data_sub %>% 
  select({{dependent}}, all_of(predictors[1:5])) %>% 
  GGally::ggpairs()

train_data_sub %>% 
  select({{dependent}}, all_of(predictors[6:10])) %>% 
  GGally::ggpairs()

train_data_sub %>% 
  select({{dependent}}, all_of(predictors[11:15])) %>% 
  GGally::ggpairs()

train_data_sub %>% 
  select({{dependent}}, all_of(predictors[16:20])) %>% 
  GGally::ggpairs()

train_data_sub %>% 
  select({{dependent}}, all_of(predictors[21:25])) %>% 
  GGally::ggpairs()

train_data_sub %>% 
  select({{dependent}}, all_of(predictors[26:30])) %>% 
  GGally::ggpairs()

train_data_sub %>% 
  select({{dependent}}, all_of(predictors[31:35])) %>% 
  GGally::ggpairs()

train_data_sub %>% 
  select({{dependent}}, all_of(predictors[36:40])) %>% 
  GGally::ggpairs()
