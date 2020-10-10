library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)

#' Note: the data files are witheld because of
#' commercial confidentiality

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Yield prediction/Oil_palm_yield_prediction_GitHub/Oil_palm_yield_prediction/")

## load crop info ##

df <- read.csv("Data/external_YPC Datasets A.csv")

names(df) <- gsub("\\.","_",names(df))

df <- rename(df,CALENDAR_YEAR=CALENDER_YEAR,
             CALENDAR_MONTH=CALENDER_MONTH)

# format dates

df$CALENDAR_DATE <- ymd(paste(df$CALENDAR_YEAR,df$CALENDAR_MONTH,1))

df$PLANTING_DATE <- mdy(df$PLANTING_DATE)

df$MATURE_DATE <- mdy(df$MATURE_DATE)

# divide field output by number of hectares
# because field size varies

df <- mutate(df,CPB_YIELD=CPB_TONNAGE/CPB_HECTRAGE)

# add 2018 data

df_empty <- filter(df,CALENDAR_DATE>ymd(20161231))

df_empty <- select(df_empty,-CALENDAR_YEAR,-CPB_TONNAGE,-CPB_BUNCHES,-RAINFALL,-CALENDAR_DATE,-CPB_YIELD)

df_empty$CALENDAR_YEAR <- 2018

df_empty$CALENDAR_DATE <- ymd(paste(df_empty$CALENDAR_YEAR,df_empty$CALENDAR_MONTH,1))

df <- bind_rows(df,df_empty)

# calculate palm age

df$PLANTING_DATE <- ymd(df$PLANTING_DATE)

df$PALM_AGE <- df$CALENDAR_YEAR-year(df$PLANTING_DATE)

#' inspect palm age data

# ggplot(df,aes(x=CALENDAR_DATE,y=PALM_AGE)) +
#   geom_line() +
#   facet_wrap(~FIELD)

#' calculate yields one year ago from the current month

df <- arrange(df,CALENDAR_DATE)

df <- df %>%
  group_by(FIELD) %>%
  mutate(CPB_YIELD_LAG_12_MONTHS=lag(CPB_YIELD,12))

#' inspect yield data

# ggplot(df,aes(x=CALENDAR_DATE,y=CPB_YIELD_LAG_12_MONTHS)) +
#   geom_line() +
#   facet_wrap(~FIELD)

#' rename rainfall data to distinguish from others

df <- df %>%
  rename(RAINFALL_FIELD=RAINFALL)

#' convert data before 2009 to NA

df_before_2009 <- df %>%
  filter(CALENDAR_DATE<ymd(20091001)) %>%
  mutate(RAINFALL_FIELD=NA)

#' merge with data after 2009

df_after_2009 <- df %>%
  filter(CALENDAR_DATE>ymd(20091001)) 

df <- bind_rows(df_before_2009,df_after_2009)

#' calculate rainfall in past 6 and 12 months
#' one year ago from the current month

df <- arrange(df,CALENDAR_DATE)

df <- df %>%
  group_by(FIELD) %>%
  mutate(RAINFALL_FIELD_PAST_6_MONTHS=rollsum(RAINFALL_FIELD,6,align=("right"),na.pad = TRUE),
         RAINFALL_FIELD_PAST_12_MONTHS=rollsum(RAINFALL_FIELD,12,align=("right"),na.pad = TRUE),
         RAINFALL_FIELD_PAST_6_MONTHS_LAG_12_MONTHS=lag(RAINFALL_FIELD_PAST_6_MONTHS,12),
         RAINFALL_FIELD_PAST_12_MONTHS_LAG_12_MONTHS=lag(RAINFALL_FIELD_PAST_12_MONTHS,12))

#' inspect rainfall data
#' data doesn't start until 2009
#' and ends in 2017

# ggplot(df,aes(x=CALENDAR_DATE,y=RAINFALL_FIELD)) +
#   geom_line() +
#   facet_wrap(~FIELD)



## add weather data ##

johor <- read.csv("Data/Johor_Weather_D.UR.all.20080101-20180630.csv")

selangor <- read.csv("Data/Selangor_Weather_D.DD.all.20080101-20180630.csv")

weather <- bind_rows(johor,selangor)

weather$State <- c(rep("johor",nrow(johor)),rep("selangor",nrow(selangor)))

weather$State <- as.factor(weather$State)

names(weather) <- gsub("\\.","_",names(weather))

weather$Date <- dmy(weather$Date)

weather$CALENDAR_MONTH <- month(weather$Date)

weather$CALENDAR_MONTH <- toupper(month.abb[weather$CALENDAR_MONTH])

weather$CALENDAR_MONTH <- as.factor(weather$CALENDAR_MONTH)

weather$CALENDAR_YEAR <- year(weather$Date)

weather <- weather %>%
  group_by(State,CALENDAR_YEAR,CALENDAR_MONTH) %>%
  summarize_at(vars(Average_Relative_Humidity:Average_Wind_Speed,Rainfall),funs(mean(.,na.rm = TRUE)))

weather$CALENDAR_DATE <- ymd(paste(weather$CALENDAR_YEAR,weather$CALENDAR_MONTH,1))

weather_gather <- gather(weather,Average_Relative_Humidity:Rainfall,key="PREDICTOR",value="AMOUNT")

weather_gather <- weather_gather %>%
  gather(variable,value,-(State:PREDICTOR)) %>%
  unite(temp,PREDICTOR,variable) %>%
  spread(temp,value)

johor_fields <- c("F1","F2","F5")

df_johor <- left_join(filter(df,FIELD %in% johor_fields),
                            filter(weather_gather,State=="johor"),
                            by=c("CALENDAR_YEAR","CALENDAR_MONTH","CALENDAR_DATE"))

selangor_fields <- c("F3","F4")

df_selangor <- left_join(filter(df,FIELD %in% selangor_fields),
                            filter(weather_gather,State=="selangor"),
                            by=c("CALENDAR_YEAR","CALENDAR_MONTH","CALENDAR_DATE"))

df <- bind_rows(df_johor,df_selangor)

#' rename rainfall data to distinguish from others

df <- df %>%
  rename(Rainfall_Met_Station_AMOUNT=Rainfall_AMOUNT)

df <- gather(df,Average_Relative_Humidity_AMOUNT:Rainfall_Met_Station_AMOUNT,key="PREDICTOR",value="AMOUNT")

#' fill in missing values before using rollsum

#' fill in NAs with values from a year ago
#' then fill in remaining NAs with previous value

df <- df %>%
  arrange(CALENDAR_DATE) %>%
  group_by(FIELD,PREDICTOR) %>%
  mutate(AMOUNT_LAG_12=lag(AMOUNT,12),
         AMOUNT=ifelse(is.na(AMOUNT),AMOUNT_LAG_12,AMOUNT)) %>%
  mutate(AMOUNT=na.locf(AMOUNT,na.rm = FALSE))

#' remove values from a year ago
#' as they are only used to fill in missing values

df <- df %>% select(-AMOUNT_LAG_12)

#' convert data after 2015 to NA

df_wind_after_2015 <- df %>%
  filter(CALENDAR_DATE>ymd(20150501),
         PREDICTOR=="Average_Wind_Speed_AMOUNT") %>%
  mutate(AMOUNT=NA)

#' merge with data before 2015

df_before_2015 <- df %>%
  filter(CALENDAR_DATE<ymd(20150501))

df_no_wind_after_2015 <- df %>%
  filter(CALENDAR_DATE>ymd(20150501),
         PREDICTOR!="Average_Wind_Speed_AMOUNT")

df <- bind_rows(df_before_2015,df_no_wind_after_2015,df_wind_after_2015)

df_rain <- df %>%
  group_by(FIELD,PREDICTOR) %>%
  filter(PREDICTOR=="Rainfall_Met_Station_AMOUNT") %>%
  mutate(PAST_6_MONTHS=rollsum(AMOUNT,6,align=("right"),na.pad = TRUE),
         PAST_12_MONTHS=rollsum(AMOUNT,12,align=("right"),na.pad = TRUE),
         PAST_6_MONTHS_LAG_12_MONTHS=lag(PAST_6_MONTHS,12),
         PAST_12_MONTHS_LAG_12_MONTHS=lag(PAST_12_MONTHS,12))

df_other <- df %>%
  group_by(FIELD,PREDICTOR) %>%
  filter(PREDICTOR!="Rainfall_Met_Station_AMOUNT") %>%
  mutate(PAST_6_MONTHS=rollmean(AMOUNT,6,align=("right"),na.pad = TRUE),
         PAST_12_MONTHS=rollmean(AMOUNT,12,align=("right"),na.pad = TRUE),
         PAST_6_MONTHS_LAG_12_MONTHS=lag(PAST_6_MONTHS,12),
         PAST_12_MONTHS_LAG_12_MONTHS=lag(PAST_12_MONTHS,12))

df <- bind_rows(df_rain,df_other)

df <- df %>%
  gather(variable,value,AMOUNT:PAST_12_MONTHS_LAG_12_MONTHS) %>%
  unite(temp,PREDICTOR,variable) %>%
  spread(temp,value)

df <- rename(df,
             Rainfall_Met_Station_AMOUNT=Rainfall_Met_Station_AMOUNT_AMOUNT,
             Average_Temperature_AMOUNT=Average_Temperature_AMOUNT_AMOUNT,
             Average_Wind_Speed_AMOUNT=Average_Wind_Speed_AMOUNT_AMOUNT,
             Average_Solar_Radiation_AMOUNT=Average_Solar_Radiation_AMOUNT_AMOUNT,
             Average_Relative_Humidity_AMOUNT=Average_Relative_Humidity_AMOUNT_AMOUNT)

#' inspect weather data
#' data go from 2008 to 2018
#' except for windspeed data which drops to zero after 

# ggplot(df,aes(x=CALENDAR_DATE,y=Rainfall_Met_Station_AMOUNT)) +
#   geom_line() +
#   facet_wrap(~FIELD)



## add rainfall data ##

rain <- read.csv("Data/external_YPC Datasets A_rainfall.csv")

rain$RAINFALL_DT <- dmy(rain$RAINFALL_DT)

rain$CALENDAR_MONTH <- month(rain$RAINFALL_DT)

rain$CALENDAR_MONTH <- toupper(month.abb[rain$CALENDAR_MONTH])

rain$CALENDAR_MONTH <- as.factor(rain$CALENDAR_MONTH)

rain <- rain %>%
  group_by(ESTATE_CODE,CALENDAR_YEAR,CALENDAR_MONTH) %>%
  summarize(AVG_RAINFALL_MONTH=mean(QTY,na.rm = TRUE))

df <- left_join(df,rain,by=c("ESTATE_CODE","CALENDAR_YEAR","CALENDAR_MONTH"))

#' rename rainfall data to distinguish from others

df <- df %>%
  rename(RAINFALL_DIVISION=AVG_RAINFALL_MONTH)

#' fill in missing values before using rollsum

#' fill in NAs with values from a year ago
#' then fill in remaining NAs with previous value

df <- df %>%
  arrange(CALENDAR_DATE) %>%
  group_by(FIELD) %>%
  mutate(RAINFALL_DIVISION_LAG_12=lag(RAINFALL_DIVISION,12),
         RAINFALL_DIVISION=ifelse(is.na(RAINFALL_DIVISION),RAINFALL_DIVISION_LAG_12,RAINFALL_DIVISION)) %>%
  mutate(RAINFALL_DIVISION=na.locf(RAINFALL_DIVISION,na.rm = FALSE))

#' remove values from a year ago
#' as they are only used to fill in missing values

df <- df %>% select(-RAINFALL_DIVISION_LAG_12)

#' convert data for F2 to NA

df_F2 <- df %>%
  filter(FIELD=="F2") %>%
  mutate(RAINFALL_DIVISION=NA)

#' merge with data other fields

df_other_fields <- df %>%
  filter(FIELD!="F2")
  
df <- bind_rows(df_F2,df_other_fields)

#' calculate rainfall in past 6 and 12 months
#' one year ago from the current month

df <- df %>%
  arrange(CALENDAR_DATE) %>%
  group_by(FIELD) %>%
  mutate(RAINFALL_DIVISION_PAST_6_MONTHS=rollsum(RAINFALL_DIVISION,6,align=("right"),na.pad = TRUE),
         RAINFALL_DIVISION_PAST_12_MONTHS=rollsum(RAINFALL_DIVISION,12,align=("right"),na.pad = TRUE),
         RAINFALL_DIVISION_PAST_6_MONTHS_LAG_12_MONTHS=lag(RAINFALL_DIVISION_PAST_6_MONTHS,12),
         RAINFALL_DIVISION_PAST_12_MONTHS_LAG_12_MONTHS=lag(RAINFALL_DIVISION_PAST_12_MONTHS,12))

#' inspect rainfall data
#' data go from 2009 to 2018
#' data are missing for F2

# ggplot(df,aes(x=CALENDAR_DATE,y=RAINFALL_DIVISION)) +
#   geom_line() +
#   facet_wrap(~FIELD)



## add foliar data ##

foliar <- read.csv("Data/external_YPC Datasets A_foliar.csv",skip=5)

names(foliar) <- gsub("\\.","_",names(foliar))

foliar1 <- select(foliar,ESTATE:BLOCK,S_Date,Result)

foliar2 <- select(foliar,ESTATE:BLOCK,S_Date_1,Result_1)

foliar3 <- select(foliar,ESTATE:BLOCK,S_Date_2,Result_2)

foliar4 <- select(foliar,ESTATE:BLOCK,S_Date_3,Result_3)

foliar3 <- rename(foliar3,S_Date=S_Date_2,Result=Result_2)

foliar2 <- rename(foliar2,S_Date=S_Date_1,Result=Result_1)

foliar4 <- rename(foliar4,S_Date=S_Date_3,Result=Result_3)

foliar <- bind_rows(foliar1,foliar2,foliar3,foliar4)

foliar$CALENDAR_DATE <- dmy(paste(1,foliar$S_Date))

foliar$CALENDAR_YEAR <- year(foliar$CALENDAR_DATE)

foliar <- rename(foliar,NUTRIENT_AMOUNT=Result)

foliar <- foliar[grep("Avg",foliar$NUTRIENT),]

foliar <- select(foliar,FIELD,NUTRIENT,NUTRIENT_AMOUNT,CALENDAR_DATE,CALENDAR_YEAR)

# annual

foliar_annual <- foliar %>%
  group_by(FIELD,CALENDAR_YEAR,NUTRIENT) %>%
  summarize(NUTRIENT_AMOUNT_ANNUAL=mean(NUTRIENT_AMOUNT))

foliar_annual <- arrange(foliar_annual,CALENDAR_YEAR)

foliar_annual <-  foliar_annual %>%
  group_by(FIELD,NUTRIENT) %>%
  mutate(NUTRIENT_AMOUNT_ANNUAL_LAG1=lag(NUTRIENT_AMOUNT_ANNUAL,1))

foliar <- left_join(foliar,foliar_annual,by=c("FIELD","CALENDAR_YEAR","NUTRIENT"))

df <- left_join(df,foliar,by=c("FIELD","CALENDAR_DATE","CALENDAR_YEAR"))

#' inspect foliar data

# ggplot(df,aes(x=CALENDAR_DATE,y=NUTRIENT_AMOUNT)) +
#   geom_col() +
#   facet_wrap(~FIELD)



## add workers and harvest interval data ##

harvest <- read.csv("Data/Harvesting Interval.csv")

harvest <- harvest[-1,1:9]

level_names <- c(".Low",".High",".Average")

level_names_headers <- paste0(rep(names(harvest)[7],3),level_names)

names(harvest)[7:9] <- level_names_headers

names(harvest) <- gsub("\\.","_",names(harvest))

harvest <- rename(harvest,CALENDAR_YEAR=CALENDER_YEAR,
                  CALENDAR_MONTH=CALENDER_MONTH)

harvest <- mutate_at(harvest,
                     vars(Harvesting_Interval_Low:Harvesting_Interval_Average),
                     funs(as.numeric(.)))

harvest[harvest$Total_Cutter_Name==0,]$Total_Cutter_Name <- NA

harvest <- select(harvest,ESTATE_CODE,CALENDAR_YEAR:Harvesting_Interval_Average)

df <- left_join(df,harvest,by=c("ESTATE_CODE","CALENDAR_YEAR","CALENDAR_MONTH"))

df <- mutate(df,Total_Cutter_Name=Total_Cutter_Name/CPB_HECTRAGE)

#' calculate harvesters one year ago from the current month

df <- df %>%
  group_by(FIELD) %>%
  mutate(Total_Cutter_Name_LAG_12_MONTHS=lag(Total_Cutter_Name,12))

#' inspect workers and harvest interval data
#' the worker data doesn't start in all fields
#' until 2011

# ggplot(df,aes(x=CALENDAR_DATE,y=Total_Cutter_Name)) +
#   geom_line() +
#   facet_wrap(~FIELD,scales="free")

#' after 2011 there's a strong relationship
#' between total cutters per ha and yields
#' in most fields

# ggplot(df %>% filter(CALENDAR_DATE>ymd(20110101)),aes(x=Total_Cutter_Name,y=CPB_YIELD)) +
#   geom_point() +
#   facet_wrap(~FIELD,scales="free")



## add pruning, manuring, and pests and disease data ##

predictors <- c("pruning", "manuring","pd")

for (i in 1:length(predictors)) {

  assign(predictors[i],read.csv(paste0("Data/external_YPC Datasets A_",predictors[i],".csv")))

}

manuring <- rename(manuring,AMOUNT=X0AMOUNT)

pd <- rename(pd,AMOUNT=X0AMOUNT)


dfs <- rbind(pruning,manuring,pd)

dfs$PREDICTOR <- c(rep("PRUNING",nrow(pruning)),rep("MANURING",nrow(manuring)),rep("PD",nrow(pd)))

names(dfs) <- toupper(names(dfs))

names(dfs) <- gsub("\\.","_",names(dfs))

dfs$CALENDAR_DATE <- ymd(paste0(dfs$CALENDAR_YEAR,"/",dfs$CALENDAR_MONTH,"/",1))

dfs <- rename(dfs,FIELD=ZSFIELD)

dfs$AMOUNT <- as.numeric(dfs$AMOUNT)

# sum all account codes together instead of leaving them as separate

dfs <- dfs %>%
  group_by(FIELD,CALENDAR_DATE,PREDICTOR) %>%
  summarize(SUM=sum(AMOUNT,na.rm = TRUE))

# add columns for new variables to master data frame

dfs <- dfs %>%
  gather(variable,value,-(FIELD:PREDICTOR)) %>%
  unite(temp,PREDICTOR,variable) %>%
  spread(temp,value)

df <- left_join(df,dfs,by=c("CALENDAR_DATE","FIELD"))


df <- gather(df,MANURING_SUM:PRUNING_SUM,key="PREDICTOR",value="SUM")

df <- arrange(df,CALENDAR_DATE)

df <- df %>%
  group_by(FIELD,PREDICTOR) %>%
  mutate(PAST_6_MONTHS=rollsum(SUM,6,na.rm=TRUE,align=("right"),na.pad = TRUE),
         PAST_12_MONTHS=rollsum(SUM,12,na.rm=TRUE,align=("right"),na.pad = TRUE),
         PAST_6_MONTHS_LAG_12_MONTHS=lag(PAST_6_MONTHS,12),
         PAST_12_MONTHS_LAG_12_MONTHS=lag(PAST_12_MONTHS,12))

df <- df %>%
  gather(variable,value,SUM:PAST_12_MONTHS_LAG_12_MONTHS) %>%
  unite(temp,PREDICTOR,variable) %>%
  spread(temp,value)

# divide field inputs by hectares
# because field size varies

df <- mutate_at(df,vars(contains("PRUNING_SUM"),contains("MANURING_SUM"),contains("PD_SUM")),funs(./CPB_HECTRAGE))

df <- rename(df,
             MANURING_SUM=MANURING_SUM_SUM,
             PD_SUM=PD_SUM_SUM,
             PRUNING_SUM=PRUNING_SUM_SUM)

#' inspect field input data
#' gaps in the monthly data indicate no money was spent

# ggplot(df,aes(x=CALENDAR_DATE,y=MANURING_SUM_PAST_6_MONTHS)) +
#   geom_line() +
#   facet_wrap(~FIELD)



## add fertilizer data ##

estates <- paste0("E",seq(1:5))

for (i in 1:length(estates)) {

  assign(estates[i],read.csv(paste0("Data/external_YPC Datasets B_",estates[i],".csv"),skip=2,stringsAsFactors = FALSE))

}

E5$Result.4 <- as.numeric(gsub(",","",E5$Result.4))

fert <- bind_rows(E1,E2,E3,E4,E5)

names(fert) <- gsub("\\.","_",names(fert))

# filter rows to actual qty

fert <- fert[fert$Calendar_Year_Month=="Actual Qty",]

# remove result columns

fert <- select(fert,-starts_with("Result"))

# convert month columns to one single column
# and actualy qty as an adjacent column with
# tidyr gather()

fert <- fert %>%
  group_by(Plant) %>%
  gather(OCT_2009:MAY_2009,key="date",value="amount")

fert$date <- dmy(paste(1,fert$date))

fert <- rename(fert,ESTATE_CODE=Plant,CALENDAR_DATE=date,FERTILIZER_AMOUNT=amount)

fert <- fert %>% select(ESTATE_CODE,AMIS_Fertilizer_Type,CALENDAR_DATE,FERTILIZER_AMOUNT)

fert$AMIS_Fertilizer_Type <- as.factor(fert$AMIS_Fertilizer_Type)

#' 40003 = Ammonium Chloride (N)
#' 40004 = Ammonium Sulphate (N)
#' 40006 = Rock Phosphate (P)
#' 40009 = Muriate of Potash (K)
#' Note that 40003 and 40004 are used as substitutes.
#' Where one is lower, the other is higher.
#' Also, all three nutrients are applied in similar
#' quantities in kg whereas some nutrients such as
#' organic fertilizer are applied in much higher quantities
#' in kg.

fert_combined <- fert %>%
  filter(AMIS_Fertilizer_Type==40003 |
           AMIS_Fertilizer_Type==40004 |
           AMIS_Fertilizer_Type==40006 |
           AMIS_Fertilizer_Type==40009) %>%
  group_by(ESTATE_CODE,CALENDAR_DATE) %>%
  summarize(FERTILIZER_SUM=sum(FERTILIZER_AMOUNT,na.rm = TRUE)) %>%
  mutate(AMIS_Fertilizer_Type="40003_9") %>%
  rename(FERTILIZER_AMOUNT=FERTILIZER_SUM)

fert <- fert %>% 
  bind_rows(fert_combined) %>%
  arrange(ESTATE_CODE,AMIS_Fertilizer_Type,CALENDAR_DATE)

#' select only NPK fertilizers

fert <- fert %>%
  filter(AMIS_Fertilizer_Type=="40003_9") %>%
  select(-AMIS_Fertilizer_Type)

df <- left_join(df,fert,by=c("ESTATE_CODE","CALENDAR_DATE"))

df <- df %>%
  group_by(FIELD) %>%
  mutate(FERTILIZER_AMOUNT_PAST_12_MONTHS=rollsum(FERTILIZER_AMOUNT,12,na.rm=TRUE,align=("right"),na.pad = TRUE),
         FERTILIZER_AMOUNT_PAST_12_MONTHS_LAG_12_MONTHS=lag(FERTILIZER_AMOUNT_PAST_12_MONTHS,12),
         FERTILIZER_AMOUNT_PAST_12_MONTHS_LAG_24_MONTHS=lag(FERTILIZER_AMOUNT_PAST_12_MONTHS,24))

# divide field inputs by hectares
# because field size varies

df <- mutate_at(df,vars(contains("FERTILIZER_AMOUNT")),funs(./CPB_HECTRAGE))

#' inspect fertilizer data
#' gaps in the monthly data indicate no fertilizer was applied
#' in F5, fertilizer amount is negative
#' but this is probably because of adjusting
#' earlier amounts

# ggplot(df,aes(x=CALENDAR_DATE,y=FERTILIZER_AMOUNT)) +
#   geom_line() +
#   facet_wrap(~FIELD)



## add SYP ##

syp <- read.csv("Data/external_YPC Datasets A_SYP.csv")

names(syp) <- gsub("\\.","_",names(syp))

syp <- rename(syp,CALENDAR_YEAR=Fiscal_year,
              FIELD=ZSFIELD)

syp <- select(syp,FIELD,CALENDAR_YEAR,SYP_Value)

syp <- arrange(syp,FIELD,CALENDAR_YEAR)

df <- left_join(df,syp,by=c("CALENDAR_YEAR","FIELD"))

#' inspect SYP data

# ggplot(df,aes(x=CALENDAR_YEAR,y=SYP_Value)) +
#   geom_col() +
#   facet_wrap(~FIELD)



## add NDVI data ##

ndvi <- read.csv("Data/mean_ndvi_estates.csv")

ndvi$calendar_date <- ymd(ndvi$calendar_date)

ndvi <- rename(ndvi,ESTATE_CODE=estate,
               CALENDAR_DATE=calendar_date,
               MEAN_NDVI=mean_ndvi)

df <- left_join(df,ndvi,by=c("ESTATE_CODE","CALENDAR_DATE"))

#' calculate ndvi one year ago from the current month

df <- arrange(df,CALENDAR_DATE)

df <- df %>%
  group_by(FIELD) %>%
  mutate(MEAN_NDVI_LAG_12_MONTHS=lag(MEAN_NDVI,12))

df <- df %>%
  group_by(FIELD) %>%
  mutate(MEAN_NDVI_PAST_6_MONTHS=rollsum(MEAN_NDVI,6,align=("right"),na.pad = TRUE),
         MEAN_NDVI_PAST_12_MONTHS=rollsum(MEAN_NDVI,12,align=("right"),na.pad = TRUE),
         MEAN_NDVI_PAST_6_MONTHS_LAG_12_MONTHS=lag(MEAN_NDVI_PAST_6_MONTHS,12),
         MEAN_NDVI_PAST_12_MONTHS_LAG_12_MONTHS=lag(MEAN_NDVI_PAST_12_MONTHS,12))

#' inspect NDVI data

# ggplot(df,aes(x=CALENDAR_DATE,y=MEAN_NDVI)) +
#   geom_line() +
#   facet_wrap(~FIELD)



## convert month and year to factor ##

df$CALENDAR_MONTH <- as.factor(df$CALENDAR_MONTH)

df$CALENDAR_YEAR <- as.factor(df$CALENDAR_YEAR)



## rearrange and write final data frame ##

df <- df %>% arrange(FIELD,desc(CALENDAR_DATE))

save(df,file="Data/SDP_YPC_processed_data.Rda")
