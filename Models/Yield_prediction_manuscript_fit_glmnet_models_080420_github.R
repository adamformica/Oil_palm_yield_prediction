library(tidyverse)
library(glmnet)
library(broom)
library(cowplot)

#' Note: the data files are witheld because of
#' commercial confidentiality

setwd("C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Yield prediction/Oil_palm_yield_prediction_GitHub/Oil_palm_yield_prediction/")

load("Data/SDP_YPC_site_variation_training_data.Rda")

load("Data/SDP_YPC_site_variation_test_data.Rda")



### Preprocess data ###

#' Note that "test" means data from 2018 and "training"
#' for these files means data from 2008-2017.

#' Join the 2018 and 2008-17 data.

data <- bind_rows(df_training_sub,df_test_sub)

data$CALENDAR_YEAR <- as.factor(data$CALENDAR_YEAR)

#' Remove calendar year and date from the training data
#' as these are factors that can fit historical data but
#' not useful for predicting future data.

#' Remove all site-level variables except for field

data_variables_removed <- data %>%
  select(-CALENDAR_DATE,-CALENDAR_YEAR,
         -ESTATE_CODE,-(DIV:TOPO_TYPE),-CPB_HECTRAGE,
         -contains("NDVI"))

#' Reorder variables

reordered_variables <- c("FIELD",
                         "CALENDAR_MONTH",
                         "Rainfall_Met_Station_AMOUNT_PAST_12_MONTHS_LAG_12_MONTHS",
                         "Rainfall_Met_Station_AMOUNT_PAST_6_MONTHS_LAG_12_MONTHS",
                         "Average_Relative_Humidity_AMOUNT_PAST_12_MONTHS_LAG_12_MONTHS",
                         "Average_Relative_Humidity_AMOUNT_PAST_6_MONTHS_LAG_12_MONTHS",
                         "Average_Solar_Radiation_AMOUNT_PAST_12_MONTHS_LAG_12_MONTHS",
                         "Average_Solar_Radiation_AMOUNT_PAST_6_MONTHS_LAG_12_MONTHS",
                         "Average_Temperature_AMOUNT_PAST_12_MONTHS_LAG_12_MONTHS",
                         "Average_Temperature_AMOUNT_PAST_6_MONTHS_LAG_12_MONTHS",
                         "MANURING_SUM_PAST_12_MONTHS_LAG_12_MONTHS",
                         "MANURING_SUM_PAST_6_MONTHS_LAG_12_MONTHS",
                         "PD_SUM_PAST_12_MONTHS_LAG_12_MONTHS",
                         "PD_SUM_PAST_6_MONTHS_LAG_12_MONTHS",
                         "PRUNING_SUM_PAST_12_MONTHS_LAG_12_MONTHS",
                         "PRUNING_SUM_PAST_6_MONTHS_LAG_12_MONTHS",
                         "FERTILIZER_AMOUNT_PAST_12_MONTHS_LAG_12_MONTHS",
                         "PALM_AGE",
                         "CPB_YIELD_LAG_12_MONTHS",
                         "CPB_YIELD")

data_reordered <- data_variables_removed %>%
  select(reordered_variables)

# Remove rows with NA values

data_reordered_complete <- data_reordered[complete.cases(data_reordered),]



### Functions and variables ###

## Train glmnet models and generate predictions ##

#' Randomly shuffle the data
#' and break each field into
#' several folds.

create_spatial_folds <- function(data_complete,splits_per_field) {
  
  if(splits_per_field < 2)
    stop("splits per field must be greater than 2")
  
  data_shuffled <- data_complete[sample(nrow(data_complete)),]
  
  field_numbers <- as.character(unique(data_shuffled$FIELD))
  
  start_indices <- seq(1,splits_per_field*length(field_numbers),by=splits_per_field)
  
  foldid <- as.character(data_shuffled$FIELD)
  
  for (i in 1:length(field_numbers)) {
    
    fields <- data_shuffled$FIELD
    
    field <- fields[fields==field_numbers[i]]
    
    field_length <- length(field)
    
    folds <- rep((start_indices[i]):(start_indices[i]+(splits_per_field-1)),
                 length.out = field_length)
    
    foldid[foldid==field_numbers[i]] <- folds
    
  }
  
  foldid <- as.numeric(foldid)
  
  return(foldid)
  
}

# Train glmnet model

train_glmnet_model <- function(data,fold_no) {
  
  data_no_field <- data %>% 
    ungroup(FIELD) %>% 
    select(-FIELD)
  
  f <- as.formula(CPB_YIELD ~ .)
  
  data_mat <- model.matrix(f, data_no_field)[, -1]
  
  x <- data_mat
  
  y <- data$CPB_YIELD
  
  foldid <- create_spatial_folds(data,fold_no)
  
  cvfit = cv.glmnet(x, y, foldid = foldid, keep=T)
  
  return(cvfit)
  
}

# Generate glmnet model predictions

generate_glmnet_predictions <- function(data,glmnet_model) {
  
  data_no_field <- data %>% 
    ungroup(FIELD) %>% 
    select(-FIELD)
  
  f <- as.formula(CPB_YIELD ~ .)
  
  data_mat <- model.matrix(f, data_no_field)[, -1]
  
  preds <- predict(glmnet_model,newx=data_mat,s="lambda.min")
  
}


## Display glmnet model coefficients ##

rename_variables <- function(variables) {
  
  variables <- gsub("AMOUNT_","",variables)
  
  variables <- gsub("Average_","",variables)
  
  variables <- gsub("SUM_","",variables)
  
  variables <- gsub("_MONTHS_","_",variables)
  
  variables <- gsub("_MONTHS$","",variables)
  
  variables <- gsub("_MONTH","_MONTH ",variables)
  
  variables <- gsub("FIELD","FIELD ",variables)
  
  variables <- gsub("YIELD_LAG_12","YIELD_LAG",variables)
  
  variables <- gsub("_LAG_12","",variables)
  
  variables <- gsub("YIELD_LAG","YIELD_LAG_12",variables)
  
  variables <- gsub("MEAN_","",variables)
  
  variables <- gsub("CPB_","",variables)
  
  variables <- gsub("Total_Cutter_Name","Total_Cutters",variables)
  
  variables <- gsub("PD_","PEST_AND_DISEASE_",variables)
  
  variables <- tolower(variables)
  
  variables <- paste(toupper(substr(variables, 1, 1)), substr(variables, 2, nchar(variables)), sep="")
  
  variables <- gsub("Ndvi","NDVI",variables)
  
  variables <- gsub("intercept","Intercept",variables)
  
  return(variables)
  
}

#' Create table with glmnet model coefficients

create_coefficient_table <- function(cvfit_model,model) {
  
  cvfit_coef <- coef(cvfit_model, s = "lambda.min")
  
  cvfit_coef_mat <- as.matrix(cvfit_coef)
  
  cvfit_coef_var_names <- rownames(cvfit_coef_mat)
  
  cvfit_coef_var_names_renamed <- rename_variables(cvfit_coef_var_names)
  
  cvfit_coef_var_values_signif <- signif(cvfit_coef_mat[,1],2)
  
  cvfit_coef_df <- data.frame(Term=cvfit_coef_var_names_renamed,
                              Value=cvfit_coef_var_values_signif)
  
  colnames(cvfit_coef_df)[2] <- paste(model,"value")
  
  rownames(cvfit_coef_df) <- NULL
  
  return(cvfit_coef_df)
  
}


## Display glmnet fits ##

#' Define functions for testing model fit

combine_predictions_with_data <- function(data,preds) {
  
  data_preds <- data %>%
    ungroup(FIELD) %>%
    mutate(FIELD=fct_drop(FIELD)) %>%
    mutate(CPB_YIELD_PREDICTED = as.vector(preds))
  
}

display_model_fit <- function(data_preds_across_models) {
  
  fitted_models <- data_preds_across_models %>%
    group_by(Training_vs_test_data) %>%
    do(model=lm(CPB_YIELD~CPB_YIELD_PREDICTED,data=.))
  
  model_stats <- fitted_models %>% glance(model)
  
  print(model_stats)
  
  model_stats_low_p <- model_stats %>% filter(p.value<0.01)
  model_stats_high_p <- model_stats %>% filter(p.value>=0.01)
  
  if (nrow(model_stats_low_p)>0) {
    
    model_stats_low_p$label <-   paste0("R sq. = ",round(model_stats_low_p$r.squared,2),
                                        ",\np < 0.01")
    
  }
  
  if (nrow(model_stats_high_p)>0) {
    
    model_stats_high_p$label <- paste0("R sq. = ",round(model_stats_high_p$r.squared,2),
                                       ",\np = ",round(model_stats_high_p$p.value,2))
    
  }
  
  model_stats_labels <- bind_rows(model_stats_low_p,model_stats_high_p) %>%
    select(Training_vs_test_data,label,p.value) %>%
    arrange(Training_vs_test_data)
  
  p <- ggplot(data_preds_across_models, aes(x = CPB_YIELD_PREDICTED, y = CPB_YIELD)) +
    geom_point() +
    geom_smooth(method="lm") +
    facet_wrap(vars(Training_vs_test_data),scales="free",ncol=2) +
    geom_text(
      data    = subset(model_stats_labels, p.value < 0.05),
      mapping = aes(x = -Inf, y = Inf, label = label),
      hjust   = 0,
      vjust   = 1
    ) +
    xlab("Yield predicted (T/ha)") +
    ylab("Yield observed (T/ha)") +
    theme_bw()
  
}

create_model_fit_table <- function(model_data,model_training_data,model_test_data) {
  
  fitted_model <- lm(CPB_YIELD~CPB_YIELD_PREDICTED,data=model_data)
  
  model_stats <- fitted_model %>% 
    glance(model) %>%
    mutate(Training_data=model_training_data,
           Test_data=model_test_data) %>%
    select(Training_data,Test_data,adj.r.squared,statistic,df,df.residual,p.value)
  
  return(model_stats)

}


## Check glmnet model variable importance ##

check_var_importance <- function(data_complete,model_preds,fold_no) {
  
  benchmark_preds <- data_complete %>%
    ungroup(FIELD) %>%
    mutate(FIELD=fct_drop(FIELD)) %>%
    add_column(CPB_YIELD_PREDICTED = as.vector(model_preds)) 
  
  benchmark_lm <- lm(CPB_YIELD~CPB_YIELD_PREDICTED,data=benchmark_preds)
  
  benchmark_lm_summary <- summary(benchmark_lm)
  
  benchmark_rsq <- benchmark_lm_summary$r.squared
  
  data_complete_no_field <- data_complete %>% 
    ungroup(FIELD) %>% 
    select(-FIELD)
  
  f <- as.formula(CPB_YIELD ~ .)
  
  foldid <- create_spatial_folds(data_complete,fold_no)
  
  importances <- list()
  
  for (i in 1:(ncol(data_complete_no_field %>% select(-CPB_YIELD)))) {
    
    data_mat_drop_col <- model.matrix(f, data_complete_no_field[,-i])[, -1]
    
    x <- data_mat_drop_col
    
    y <- data_complete$CPB_YIELD
    
    cvfit_drop_col = cv.glmnet(x, y, foldid = foldid)
    
    preds_drop_col <- predict(cvfit_drop_col,newx=x,s="lambda.min")
    
    preds_drop_col_vec <- as.vector(preds_drop_col)
    
    df_drop_col <- data.frame(preds=preds_drop_col_vec,obs=y)
    
    lm_drop_col <- lm(obs~preds,data=df_drop_col)
    
    lm_summary_drop_col <- summary(lm_drop_col)
    
    drop_col_rsq <- lm_summary_drop_col$r.squared
    
    importances[[i]] <- benchmark_rsq - drop_col_rsq
    
  }
  
  var_names <- colnames(data_complete_no_field %>% select(-CPB_YIELD))
  
  importances_vec <- unlist(importances)
  
  df_importances <- data.frame(Variable=var_names,Importance=importances_vec)
  
  return(df_importances)
  
}


## Check model fits and variable importance across areas ##

check_model_fits_var_importance_across_areas <- function(data_area1,data_area2,
                                                         area1_fold_no,area2_fold_no) {
  
  # filter data by area
  
  data_area1 <- data_reordered_complete %>%
    filter(FIELD %in% area1)
  
  data_area2 <- data_reordered_complete %>%
    filter(FIELD %in% area2)
  
  # model fits
  
  area1_glmnet_model <- train_glmnet_model(data_area1,area1_fold_no)
  
  area2_glmnet_model <- train_glmnet_model(data_area2,area2_fold_no)
  
  area1_glmnet_area1_predictions <- generate_glmnet_predictions(data_area1,area1_glmnet_model)
  
  area1_glmnet_area2_predictions <- generate_glmnet_predictions(data_area2,area1_glmnet_model)
  
  area2_glmnet_area2_predictions <- generate_glmnet_predictions(data_area2,area2_glmnet_model)
  
  area1_data_area1_glmnet_area1_predictions <- combine_predictions_with_data(data_area1,
                                                                             area1_glmnet_area1_predictions)
  
  area2_data_area1_glmnet_area2_predictions <- combine_predictions_with_data(data_area2,
                                                                             area1_glmnet_area2_predictions)
  
  area2_data_area2_glmnet_area2_predictions <- combine_predictions_with_data(data_area2,
                                                                             area2_glmnet_area2_predictions)
  
  area1_data_area1_glmnet_area1_predictions_label <- area1_data_area1_glmnet_area1_predictions %>%
    mutate(Training_vs_test_data=paste(paste(area1,collapse = ", "),"vs.",paste(area1,collapse = ", ")))
  
  area2_data_area1_glmnet_area2_predictions_label <- area2_data_area1_glmnet_area2_predictions %>%
    mutate(Training_vs_test_data=paste(paste(area1,collapse = ", "),"vs.",paste(area2,collapse = ", ")))
  
  area1_glmnet_area1_fit <- create_model_fit_table(area1_data_area1_glmnet_area1_predictions,
                                                   paste(area1,collapse = ", "),
                                                   paste(area1,collapse = ", "))
  
  area1_glmnet_area2_fit <- create_model_fit_table(area2_data_area1_glmnet_area2_predictions,
                                                   paste(area1,collapse = ", "),
                                                   paste(area2,collapse = ", "))
  
  model_fit_table <- bind_rows(area1_glmnet_area1_fit,area1_glmnet_area2_fit)
  
  # variable importance
  
  area1_importance <- check_var_importance(data_area1,area1_glmnet_area1_predictions,area1_fold_no)
  
  area2_importance <- check_var_importance(data_area2,area2_glmnet_area2_predictions,area2_fold_no)
  
  area1_importance_label <- area1_importance %>%
    mutate(Model=paste(area1,collapse = ", "),
           Model=paste(Model,"model"))
  
  area2_importance_label <- area2_importance %>%
    mutate(Model=paste(area2,collapse = ", "),
           Model=paste(Model,"model"))
  
  output_list <- list(model_fit_table,
                      area1_importance_label,
                      area2_importance_label,
                      area1_data_area1_glmnet_area1_predictions_label,
                      area2_data_area1_glmnet_area2_predictions_label)
  
  return(output_list)
  
}


## Plot variable importance ##

plot_var_importance <- function(var_importance_summary_table,plot_name,fig_width,fig_height) {
  
  var_importance_rename <- var_importance_summary_table %>%
    mutate(Variable=rename_variables(Variable))
  
  var_importance_factor <- var_importance_rename %>%
    mutate(Variable=factor(Variable,levels=rev(unique(Variable))))
  
  # Offsets correspond to number of variables in each group
  p <- ggplot(var_importance_factor,aes(x=Variable,y=Importance)) +
    geom_col() +
    facet_wrap(vars(Model),ncol=2) +
    theme_bw() +
    # Palm_level
    geom_segment(aes(x=2.5, xend=2.5, y=min(var_importance_factor$Importance), yend=max(var_importance_factor$Importance)),linetype=2) +
    # Management
    geom_segment(aes(x=2.5+7, xend=2.5+7, y=min(var_importance_factor$Importance), yend=max(var_importance_factor$Importance)),linetype=2) +
    # Weather
    geom_segment(aes(x=2.5+7+8, xend=2.5+7+8, y=min(var_importance_factor$Importance), yend=max(var_importance_factor$Importance)),linetype=2) +
    coord_flip() +
    ylab("Importance (change in R sq.)")
  
  file_path <- paste0("Manuscript_figures/",plot_name,".png")
  
  png(file_path,width=fig_width,height=fig_height,units="in",res=100)
  
  print(p)
  
  dev.off()
  
}



### Fit glmnet models and generate outputs ###

## Inland fields ##

# F1 and F2 vs F5

area1 <- c("F1","F2")

area2 <- c("F5")

F1_F2_vs_F5_outputs <- check_model_fits_var_importance_across_areas(area1,area2,5,10)

F1_F2_vs_F5_model_fits <- F1_F2_vs_F5_outputs[[1]]

F1_F2_var_importance <- F1_F2_vs_F5_outputs[[2]]

F5_var_importance <- F1_F2_vs_F5_outputs[[3]]

F1_F2_data_F1_F2_glmnet_F1_F2_predictions <- F1_F2_vs_F5_outputs[[4]]

F1_F2_data_F1_F2_glmnet_F5_predictions <- F1_F2_vs_F5_outputs[[5]]

# F1 and F5 vs F2

area1 <- c("F1","F5")

area2 <- c("F2")

F1_F5_vs_F2_outputs <- check_model_fits_var_importance_across_areas(area1,area2,5,10)

F1_F5_vs_F2_model_fits <- F1_F5_vs_F2_outputs[[1]]

F1_F5_var_importance <- F1_F5_vs_F2_outputs[[2]]

F2_var_importance <- F1_F5_vs_F2_outputs[[3]]

F1_F5_data_F1_F5_glmnet_F1_F5_predictions <- F1_F5_vs_F2_outputs[[4]]

F1_F5_data_F1_F5_glmnet_F2_predictions <- F1_F5_vs_F2_outputs[[5]]

# F2 and F5 vs F1

area1 <- c("F2","F5")

area2 <- c("F1")

F2_F5_vs_F1_outputs <- check_model_fits_var_importance_across_areas(area1,area2,5,10)

F2_F5_vs_F1_model_fits <- F2_F5_vs_F1_outputs[[1]]

F2_F5_var_importance <- F2_F5_vs_F1_outputs[[2]]

F1_var_importance <- F2_F5_vs_F1_outputs[[3]]

F2_F5_data_F2_F5_glmnet_F2_F5_predictions <- F2_F5_vs_F1_outputs[[4]]

F2_F5_data_F2_F5_glmnet_F1_predictions <- F2_F5_vs_F1_outputs[[5]]


## Coastal and inland fields ##

# Coastal vs inland fields

area1 <- c("F3","F4")

area2 <- c("F1","F2","F5")

coastal_vs_inland_outputs <- check_model_fits_var_importance_across_areas(area1,area2,5,3)

coastal_vs_inland_model_fits <- coastal_vs_inland_outputs[[1]]

coastal_var_importance <- coastal_vs_inland_outputs[[2]]

inland_var_importance <- coastal_vs_inland_outputs[[3]]

coastal_data_coastal_glmnet_coastal_predictions <- coastal_vs_inland_outputs[[4]]

coastal_data_coastal_glmnet_inland_predictions <- coastal_vs_inland_outputs[[5]]

# Inland vs coastal fields

area1 <- c("F1","F2","F5")

area2 <- c("F3","F4")

inland_vs_coastal_outputs <- check_model_fits_var_importance_across_areas(area1,area2,3,5)

inland_vs_coastal_model_fits <- inland_vs_coastal_outputs[[1]]

inland_var_importance <- inland_vs_coastal_outputs[[2]]

coastal_var_importance <- inland_vs_coastal_outputs[[3]]

inland_data_inland_glmnet_inland_predictions <- inland_vs_coastal_outputs[[4]]

inland_data_inland_glmnet_coastal_predictions <- inland_vs_coastal_outputs[[5]]



### Generate summary table with model fits ###

model_fits_summary_table <- bind_rows(F1_F2_vs_F5_model_fits,
                                      F1_F5_vs_F2_model_fits,
                                      F2_F5_vs_F1_model_fits,
                                      inland_vs_coastal_model_fits,
                                      coastal_vs_inland_model_fits)

model_fits_summary_table_sub <- model_fits_summary_table %>%
  mutate(Training_data=gsub("F3, F4","Coastal fields",Training_data),
         Training_data=gsub("F1, F2, F5","Inland fields",Training_data),
         Test_data=gsub("F3, F4","Coastal fields",Test_data),
         Test_data=gsub("F1, F2, F5","Inland fields",Test_data))

model_fits_summary_table_scale <- model_fits_summary_table_sub %>%
  mutate(Regions=c(rep("Inland",6),rep("Inland and coastal",4))) %>%
  select(Regions,Training_data:p.value)

model_fits_summary_table_round <- model_fits_summary_table_scale %>%
  mutate_at(vars(adj.r.squared:statistic),~(round(.,2)))

model_fits_summary_table_round$p.value <- ifelse(model_fits_summary_table_round$p.value<0.01,
                                                 "p < 0.01",
                                                 paste("p =",round(model_fits_summary_table_round$p.value,2)))

colnames(model_fits_summary_table_round)[2:8] <- c("Training data",
                                                   "Test data",
                                                   "Adj. R sq.",
                                                   "F statistic",
                                                   "df",
                                                   "df residual",
                                                   "p value")

write.csv(model_fits_summary_table_round,
          paste0("Manuscript_figures/model_fits_summary_table.csv"),
          row.names = FALSE)



### Generate figures with variable importance ###

## Inland fields ##

inland_var_importance_summary_table <- bind_rows(F1_F2_var_importance,
                                                 F5_var_importance,
                                                 F1_F5_var_importance,
                                                 F2_var_importance,
                                                 F2_F5_var_importance,
                                                 F1_var_importance)

inland_var_importance_relevel <- inland_var_importance_summary_table %>%
  mutate(Model=as.factor(Model),
         Model=fct_relevel(Model,c("F1, F2 model",
                                   "F5 model",
                                   "F1, F5 model",
                                   "F2 model",
                                   "F2, F5 model",
                                   "F1 model"))) 
  
plot_var_importance(inland_var_importance_relevel,
                    "inland_var_importance_plots",
                    6.5,8)

## Coastal and inland fields ##

coastal_var_importance_label <- coastal_var_importance %>%
  mutate(Model="Coastal model")

inland_var_importance_label <- inland_var_importance %>%
  mutate(Model="Inland model")

coastal_inland_var_importance_summary_table <- bind_rows(coastal_var_importance_label,
                                                         inland_var_importance_label)

coastal_inland_var_importance_relevel <- coastal_inland_var_importance_summary_table %>%
  mutate(Model=as.factor(Model),
         Model=fct_relevel(Model,c("Inland model",
                                   "Coastal model"))) 

plot_var_importance(coastal_inland_var_importance_relevel,
                    "inland_coastal_var_importance_plots",
                    6.5,4.5)



### Display model fits ###

## Inland fields ##

data_preds_across_models_inland <- bind_rows(F1_F2_data_F1_F2_glmnet_F1_F2_predictions,
                                             F1_F2_data_F1_F2_glmnet_F5_predictions,
                                             F1_F5_data_F1_F5_glmnet_F1_F5_predictions,
                                             F1_F5_data_F1_F5_glmnet_F2_predictions,
                                             F2_F5_data_F2_F5_glmnet_F2_F5_predictions,
                                             F2_F5_data_F2_F5_glmnet_F1_predictions)

data_preds_across_models_inland_cross <- data_preds_across_models_inland %>%
  mutate(Training_vs_test_data=gsub("vs.","model x",Training_vs_test_data),
         Training_vs_test_data=paste(Training_vs_test_data,"data"))

data_preds_across_models_inland_relevel <- data_preds_across_models_inland_cross %>%
  mutate(Training_vs_test_data=as.factor(Training_vs_test_data),
         Training_vs_test_data=fct_relevel(Training_vs_test_data,
                                           "F1, F2 model x F1, F2 data",
                                           "F1, F2 model x F5 data",
                                           "F1, F5 model x F1, F5 data",
                                           "F1, F5 model x F2 data",   
                                           "F2, F5 model x F2, F5 data",
                                           "F2, F5 model x F1 data"))

data_preds_across_models_inland_plot <- display_model_fit(data_preds_across_models_inland_relevel)

ggsave("Manuscript_figures/inland_model_fits.png",
       data_preds_across_models_inland_plot,
       width=6.5,height=8)


## Coastal and inland fields ##

data_preds_across_models_coastal_inland <- bind_rows(coastal_data_coastal_glmnet_coastal_predictions,
                                                     coastal_data_coastal_glmnet_inland_predictions,
                                                     inland_data_inland_glmnet_inland_predictions,
                                                     inland_data_inland_glmnet_coastal_predictions)

data_preds_across_models_coastal_inland_sub <- data_preds_across_models_coastal_inland %>%
  mutate(Training_vs_test_data=gsub("F3, F4","coastal",Training_vs_test_data),
         Training_vs_test_data=gsub("F1, F2, F5","inland",Training_vs_test_data),
         Training_vs_test_data=paste(toupper(substring(Training_vs_test_data, 1,1)),
                                     substring(Training_vs_test_data, 2),
                                     sep=""))

data_preds_across_models_coastal_inland_cross <- data_preds_across_models_coastal_inland_sub %>%
  mutate(Training_vs_test_data=gsub("vs.","model x",Training_vs_test_data),
         Training_vs_test_data=paste(Training_vs_test_data,"data"))

data_preds_across_models_coastal_inland_relevel <- data_preds_across_models_coastal_inland_cross %>%
  mutate(Training_vs_test_data=as.factor(Training_vs_test_data),
         Training_vs_test_data=fct_relevel(Training_vs_test_data,
                                           "Inland model x inland data",
                                           "Inland model x coastal data",
                                           "Coastal model x coastal data",
                                           "Coastal model x inland data"))

data_preds_across_models_coastal_inland_plot <- display_model_fit(data_preds_across_models_coastal_inland_relevel)

ggsave("Manuscript_figures/inland_coastal_model_fits.png",
       data_preds_across_models_coastal_inland_plot,
       width=6.5,height=5.5)


### Create correlation heatmap ###

# reorder factors in groups

data_heatmap <- data_reordered %>%
  ungroup() %>%
  select(-FIELD,-CALENDAR_MONTH,-CPB_YIELD)

colnames(data_heatmap) <- rename_variables(colnames(data_heatmap))

data_heatmap_complete <- data_heatmap[complete.cases(data_heatmap),]

cormat <- round(cor(as.matrix(data_heatmap_complete)),2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
library(ggplot2)
p <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Offsets correspond to number of variables in each group
  # Weather box
  geom_segment(aes(x=8.5, xend=8.5, y=0.5, yend=8.5)) +
  geom_segment(aes(x=8.5, xend=8.5+7+2, y=8.5, yend=8.5)) +
  # Management box
  geom_segment(aes(x=8.5+7, xend=8.5+7, y=0.5, yend=8.5+7)) +
  geom_segment(aes(x=8.5+7, xend=8.5+7+2, y=8.5+7, yend=8.5+7)) +
  theme(axis.text=element_text(size=11))
  
file_path <- paste0("Manuscript_figures/yield_prediction_correlation_matrix.png")

png(file_path,width=8,height=6,units="in",res=100)

print(p)

dev.off()
