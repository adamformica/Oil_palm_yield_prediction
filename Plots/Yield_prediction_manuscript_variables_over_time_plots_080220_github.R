library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)
library(forcats)

#' Note: the data files are witheld because of
#' commercial confidentiality

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Yield prediction/Oil_palm_yield_prediction_GitHub/Oil_palm_yield_prediction/")

load("Data/SDP_YPC_processed_data.Rda")

df_ungroup <- ungroup(df)

#' Normalize variables to comply with SDP permission to use data

normalize <- function(x) { x/max(x,na.rm = TRUE) }

df_norm <- df_ungroup %>%
  mutate_at (c("CPB_YIELD",
               "MANURING_SUM_PAST_6_MONTHS",
               "PD_SUM_PAST_6_MONTHS",
               "PRUNING_SUM_PAST_6_MONTHS",
               "FERTILIZER_AMOUNT_PAST_12_MONTHS"),
             normalize)

#' Yields

yield_plot <- ggplot(df_norm,aes(x=CALENDAR_DATE,y=CPB_YIELD)) +
  geom_line() +
  facet_wrap(~FIELD) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  xlab("Calendar date") +
  ylab("Normalized yield") +
  theme_bw()

file_path <- paste0("Manuscript_figures/yield_plots.png")

png(file_path,width=6,height=4,units="in",res=100)

print(yield_plot)

dev.off()

#' Weather

df_region <- df %>%
  ungroup(FIELD) %>%
  mutate(REGION=ifelse((FIELD=="F3" | FIELD=="F4"),"Coastal fields","Inland fields"))

df_region_relevel <- df_region %>%
  mutate(REGION=as.factor(REGION),
         REGION=fct_relevel(REGION,c("Inland fields","Coastal fields")))

rain_plot <- ggplot(df_region_relevel,aes(x=CALENDAR_DATE,y=Rainfall_Met_Station_AMOUNT)) +
  geom_line() +
  facet_wrap(~REGION) +
  xlab("Calendar date") +
  ylab("Total\nrainfall (mm)") +
  theme_bw()

humid_plot <- ggplot(df_region_relevel,aes(x=CALENDAR_DATE,y=Average_Relative_Humidity_AMOUNT)) +
  geom_line() +
  facet_wrap(~REGION) +
  xlab("Calendar date") +
  ylab("Average relative\nhumidity (%)") +
  theme_bw()

radiation_plot <- ggplot(df_region_relevel,aes(x=CALENDAR_DATE,y=Average_Solar_Radiation_AMOUNT)) +
  geom_line() +
  facet_wrap(~REGION) +
  xlab("Calendar date") +
  ylab("Average solar\nradiation (W/m2)") +
  theme_bw()

temp_plot <- ggplot(df_region_relevel,aes(x=CALENDAR_DATE,y=Average_Temperature_AMOUNT)) +
  geom_line() +
  facet_wrap(~REGION) +
  xlab("Calendar date") +
  ylab("Average\n temperature (deg C)") +
  theme_bw()

weather_plots <- plot_grid(rain_plot,
                           humid_plot,
                           radiation_plot,
                           temp_plot,
                           ncol = 1,
                           labels=toupper(letters[1:4]))

save_plot("Manuscript_figures/weather_plots.png",
          weather_plots,
          base_height = 8,
          base_width = 6)

#' Management

df_before_2018 <- df_norm %>%
  filter(CALENDAR_DATE<ymd(20180101))

manuring_plot <- ggplot(df_before_2018,aes(x=CALENDAR_DATE,y=MANURING_SUM_PAST_6_MONTHS)) +
  geom_line() +
  facet_grid(cols=vars(FIELD)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  xlab("Calendar date") +
  ylab("Normalized\nmanuring\nspend per ha") +
  theme_bw()

pest_plot <- ggplot(df_before_2018,aes(x=CALENDAR_DATE,y=PD_SUM_PAST_6_MONTHS)) +
  geom_line() +
  facet_grid(cols=vars(FIELD)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  xlab("Calendar date") +
  ylab("Normalized\npest & disease\nspend per ha") +
  theme_bw()

pruning_plot <- ggplot(df_before_2018,aes(x=CALENDAR_DATE,y=PRUNING_SUM_PAST_6_MONTHS)) +
  geom_line() +
  facet_grid(cols=vars(FIELD)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  xlab("Calendar date") +
  ylab("Normalized\npruning\nspend per ha") +
  theme_bw()

fertilizer_plot <- ggplot(df_before_2018,aes(x=CALENDAR_DATE,y=FERTILIZER_AMOUNT_PAST_12_MONTHS)) +
  geom_line() +
  facet_grid(cols=vars(FIELD)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  xlab("Calendar date") +
  ylab("Normalized\nNPK fertilizer\napplied per ha") +
  theme_bw()

management_plots <- plot_grid(manuring_plot,
                              pest_plot,
                              pruning_plot,
                              fertilizer_plot,
                              ncol = 1,
                              labels=toupper(letters[1:4]))

save_plot("Manuscript_figures/management_plots.png",
          management_plots,
          base_height = 7,
          base_width = 7)
