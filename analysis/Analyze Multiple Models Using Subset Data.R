
# Analyzing findings from multiple model comparrisons 

library(tidyverse)

# 1) Classification results
class_result_list <- "many-models-classification-results-list.rds"
reg_result_list <- "many-models-regression-results-list.rds"
v001_path <- 'data/aux data/many models results/2018-10-21-v001'
v002_path <- 'data/aux data/many models results/2018-10-21-v002'
main_path <- v002_path


classification_results_all <- read_rds(paste0(main_path,"/",class_result_list))
regression_results_all <- read_rds(paste0(main_path,"/",reg_result_list))


#1) classification results

class_results_df <- 
  classification_results_all %>% 
  map(~keep(.x,grepl("results",names(.x)))) %>%
  compact() %>% 
  map(~.x[[1]]) %>% 
  bind_rows()

class_df_print_table <- class_results_df %>% 
  mutate(Model_Group = ifelse(grepl("GLM",model),"GLM","Other")
         , Model_Group = ifelse(grepl("ANN",model),"ANN",Model_Group) 
         , Model_Group = ifelse(grepl("GBM",model),"GBM",Model_Group) 
         , Model_Group = ifelse(grepl("RF",model),"RF",Model_Group) 
  ) %>% 
  mutate(Data_Group = ifelse(grepl("Base",model),"1) Base","Other")
         , Data_Group = ifelse(grepl("Radii",model),"3) Spatial",Data_Group)
         , Data_Group = ifelse(grepl("Zip",model),"2) Zip",Data_Group)) %>% 
  select("Data" = Data_Group
         , "Model" = Model_Group
         , AUC:Gini)

write_csv(class_df_print_table, "Writing/Sections/tables and figures/classification-results-table.csv")


class_model_results_scatterplot <- 
  class_results_df %>% 
  mutate(Model_Group = ifelse(grepl("GLM",model),"GLM","Other")
         , Model_Group = ifelse(grepl("ANN",model),"ANN",Model_Group) 
         , Model_Group = ifelse(grepl("GBM",model),"GBM",Model_Group) 
         , Model_Group = ifelse(grepl("RF",model),"RF",Model_Group) 
  ) %>% 
  mutate(Data_Group = ifelse(grepl("Base",model),"1) Base","Other")
         , Data_Group = ifelse(grepl("Radii",model),"3) Spatial",Data_Group)
         , Data_Group = ifelse(grepl("Zip",model),"2) Zip",Data_Group)) %>% 
  mutate(model_label = paste0(Model_Group," ",Data_Group)
         , model_label = str_squish(gsub("[0-9]|[)]|' '","",model_label))) %>% 
  ggplot()+
  aes(x = R2, y = AUC, shape = Data_Group, color = Model_Group)+
  geom_point(size = 3)+
  ggrepel::geom_text_repel(aes(label = model_label),point.padding=0.5,show.legend = F)+
  theme_bw()+
  labs(title = "Classification Model Performance on Test Data"
       , x = "R-squared"
       , shape = "Data"
       , color = "Model")

"Writing/Sections/tables and figures/class_model_results_scatterplot.jpeg" %>% 
  jpeg(width = 480*2, height = 480*2
       , units = "px", pointsize = 10
       , quality = 100, res = 150)
class_model_results_scatterplot
dev.off()




class_var_imp_names <- 
  classification_results_all %>% 
  map(~keep(.x,grepl("var imp",names(.x)))) %>%
  compact() %>% 
  map(~{ names(.x) }) 

class_var_imp <- 
  classification_results_all %>% 
  map(~keep(.x,grepl("var imp",names(.x)))) %>%
  compact() %>% 
  map(~{.x[[1]]}) %>% 
  set_names(class_var_imp_names)

class_var_imp_gbm_spatial <- 
  class_var_imp$`Radii Prob GBM var imp` %>% 
  as_tibble() %>% 
  mutate(scaled_importance = round(scaled_importance,3)) %>% 
  mutate(`Cumulative %` = scales::percent(cumsum(percentage))) %>% 
  select("Variable" = variable, "Scaled Importance (Max = 1)" = scaled_importance
         , `Cumulative %`) %>% 
  head(10)

write_csv(class_var_imp_gbm_spatial, "Writing/Sections/tables and figures/classification_feat_impo.csv")


class_roc_df <- 
  classification_results_all %>% 
  map(~keep(.x,grepl("ROC df",names(.x)))) %>%
  compact() %>% 
  map(~.x[[1]]) %>%
  set_names(class_var_imp_names) %>% 
  bind_rows(.id = "Model") %>% 
  mutate(Model = gsub(" var imp","",Model)) %>% 
  mutate(Model_Group = ifelse(grepl("GLM",Model),"GLM","Other")
         , Model_Group = ifelse(grepl("ANN",Model),"ANN",Model_Group) 
         , Model_Group = ifelse(grepl("GBM",Model),"GBM",Model_Group) 
         , Model_Group = ifelse(grepl("RF",Model),"RF",Model_Group) 
  ) %>% 
  mutate(Data_Group = ifelse(grepl("Base",Model),"1) Base","Other")
         , Data_Group = ifelse(grepl("Radii",Model),"3) Spatial",Data_Group)
         , Data_Group = ifelse(grepl("Zip",Model),"2) Zip",Data_Group)
  ) %>% 
  left_join(select(class_results_df,model,AUC), by =c("Model"="model")) %>% 
  mutate(`AUC Text` = paste0("AUC: ",round(AUC,3)))

auc_compare_chart <- 
  class_roc_df %>% 
  #filter(Model=="Base Prob GLM") %>% 
  ggplot()+
  aes(x = FPR, y = TPR,  group = Model)+
  geom_line()+
  geom_abline(intercept = 0, slope = 1, color = "darkgrey")+
  geom_text(aes(x = 1,y = 0, label = `AUC Text`), hjust = 1, vjust = -1)+
  facet_grid(Model_Group~Data_Group)+
  theme_bw()+
  labs(title = "Comparison of Classification Model ROC Curves"
       , x = "False Positive Rate (1-Specificity)"
       , y = "True Positive Rate (Sensitivity)"
       , color = "Model")

"Writing/Sections/tables and figures/compare_model_roc_curves.jpeg" %>% 
  jpeg(width = 480*2, height = 480*2
       , units = "px", pointsize = 10
       , quality = 100, res = 150)
auc_compare_chart
dev.off()


#2) regression results

reg_results_df <- 
  regression_results_all %>% 
  map(~keep(.x,grepl("results",names(.x)))) %>%
  compact() %>% 
  map(~.x[[1]]) %>% 
  bind_rows() %>% 
  mutate(Model_Group = ifelse(grepl("GLM",model),"GLM","Other")
         , Model_Group = ifelse(grepl("ANN",model),"ANN",Model_Group) 
         , Model_Group = ifelse(grepl("GBM",model),"GBM",Model_Group) 
         , Model_Group = ifelse(grepl("RF",model),"RF",Model_Group) 
  ) %>% 
  mutate(Data_Group = ifelse(grepl("Base",model),"1) Base","Other")
         , Data_Group = ifelse(grepl("Radii",model),"3) Spatial",Data_Group)
         , Data_Group = ifelse(grepl("Zip",model),"2) Zip",Data_Group)
  ) 

print_reg_results_table <- reg_results_df %>% 
  select("Data" = Data_Group
         , "Model" = Model_Group
         , RMSE:R2)

write_csv(print_reg_results_table, "Writing/Sections/tables and figures/regression_models_summary.csv")


reg_var_imp_names <- 
  regression_results_all %>% 
  map(~keep(.x,grepl("var imp",names(.x)))) %>%
  compact() %>% 
  map(~{ names(.x) }) 

reg_var_imp <- 
  regression_results_all %>% 
  map(~keep(.x,grepl("var imp",names(.x)))) %>%
  compact() %>% 
  map(~{.x[[1]]}) %>% 
  set_names(reg_var_imp_names)

reg_var_imp_ann_spatial <- 
  reg_var_imp$`Radii Price ANN var imp` %>% 
  as_tibble() %>% 
  mutate(scaled_importance = round(scaled_importance,3)) %>% 
  mutate(`Cumulative %` = scales::percent(cumsum(percentage))) %>% 
  select("Variable" = variable, "Scaled Importance (Max = 1)" = scaled_importance
         , `Cumulative %`) %>% 
  head(10)

write_csv(reg_var_imp_ann_spatial, "Writing/Sections/tables and figures/regression_feat_impo.csv")


reg_model_rmse_compare <- 
  reg_results_df %>% 
  gather(Metric, Value, -Model_Group, -Data_Group, -model) %>% 
  filter(!Metric%in%c("AIC")) %>% 
  ggplot()+
  aes(x = Data_Group, y = Value, group = Model_Group)+
  geom_point(size = 2)+
  facet_grid(Metric~Model_Group, scales = "free")+
  theme_bw()+
  labs(title = "Comparison of Regression Metrics"
       , x = NULL
       , y = NULL)



"Writing/Sections/tables and figures/compare_reg_model_rmse.jpeg" %>% 
  jpeg(width = 480*2, height = 480*2
       , units = "px", pointsize = 10
       , quality = 100, res = 150)
reg_model_rmse_compare
dev.off()


reg_model_results_scatterplot <- 
  reg_results_df %>% 
  mutate(model_label = paste0(Model_Group," ",Data_Group)
         , model_label = str_squish(gsub("[0-9]|[)]|' '","",model_label))) %>% 
  ggplot()+
  aes(x = R2, y = MAE, shape = Data_Group, color = Model_Group)+
  geom_point(size = 3)+
  ggrepel::geom_text_repel(aes(label = model_label),point.padding=0.5,show.legend = F)+
  theme_bw()+
  labs(title = "Regression Model Performances on Test Data"
       , x = "R-squared"
       , y = "MAE"
       , shape = "Data"
       , color = "Model")


"Writing/Sections/tables and figures/reg_model_results_scatterplot.jpeg" %>% 
  jpeg(width = 480*2, height = 480*2
       , units = "px", pointsize = 10
       , quality = 100, res = 150)
reg_model_results_scatterplot
dev.off()



