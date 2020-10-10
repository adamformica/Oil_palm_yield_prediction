library(dplyr)
library(lubridate)
library(glmnet)

#' Note: the data files are witheld because of
#' commercial confidentiality

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Yield prediction/Oil_palm_yield_prediction_GitHub/Oil_palm_yield_prediction/")

load("Data/SDP_YPC_processed_data.Rda")

## Prepare data for glmnet ##

#' Remove factors with only one level as these
#' can't be used for prediction.

df$ESTATE_CODE <- as.factor(df$ESTATE_CODE)

df_factor_cols <- df %>% select_if(is.factor)

df_factor_cols_level_count <- sapply(df_factor_cols, function(x) length(levels(x)))

df_factor_cols_one_level <- names(which(df_factor_cols_level_count<2))

df_factor_multi_level <- df %>% 
  select(-df_factor_cols_one_level)

#' Separate data into training data before 2018
#' and test data in 2018.

df_training <- filter(df_factor_multi_level,CALENDAR_DATE<ymd(20171231))

df_test <- filter(df_factor_multi_level,CALENDAR_DATE>ymd(20171231))

#' Select variables with no NA data in 2018
#' as these can be used to predict 2018 yields.

not_any_na <- function(x) all(!is.na(x))

df_test_no_na <- df_test %>% 
  select_if(not_any_na)

#' Remove variables where data would
#' not be availabile for forward prediction
#' at the start of 2018.

df_test_forward_prediction <- df_test_no_na %>%
  select(ESTATE_CODE:PALM_AGE,contains("LAG"))

#' Subset training data by columns which can be
#' used to predict test data.
#' Add back in yield column because 
#' these are the data the model tries to predict.

df_training_sub <- df_training %>%
  select(colnames(df_test_sub),CPB_YIELD)



#' Add 2018 yields to test data

yields_2018 <- read.csv("Data/EVALUATION_QSENSONOMICS_Actual.csv")

names(yields_2018) <- gsub("\\.","_",names(yields_2018))

yields_2018_complete <- yields_2018[complete.cases(yields_2018),]

yields_2018_complete$FIELD <- rep(paste0("F",seq(1,5)),each=12)

yields_2018_complete$FIELD <- rep(paste0("F",seq(1,5)),each=12)

yields_2018_complete$FIELD <- as.factor(yields_2018_complete$FIELD)

yields_2018_rename <- yields_2018_complete %>%
  rename(CPB_YIELD=MT_HA,
         CALENDAR_MONTH=CALENDER_MONTH,
         CALENDAR_YEAR=CALENDER_YEAR)

yields_2018_sub <- yields_2018_rename %>%
  select(FIELD,CALENDAR_YEAR,CALENDAR_MONTH,CPB_YIELD)

yields_2018_sub$CALENDAR_YEAR <- as.factor(yields_2018_sub$CALENDAR_YEAR)

df_test_sub <- df_test_forward_prediction %>%
  left_join(yields_2018_sub,by=c("FIELD","CALENDAR_YEAR","CALENDAR_MONTH"))



#' Remove columns with too many NAs, e.g. total cutters.
#' There are many NAs in some columns because the data do
#' not begin until well after 2008.
#' Including columns with many NAs causes 
#' other data to be thrown away which could
#' improve the predictions.

cols_to_include <- colSums(is.na(df_training_sub)) < 116

df_training_sub <- df_training_sub[,cols_to_include]

df_test_sub <- df_test_sub[,cols_to_include]



#' Save training and test data

save(df_training_sub,file="Data/SDP_YPC_site_variation_training_data.Rda")

save(df_test_sub,file="Data/SDP_YPC_site_variation_test_data.Rda")
