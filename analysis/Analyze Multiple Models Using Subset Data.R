
# Analyzing findings from multiple model comparrisons 

library(tidyverse)

# 1) Classification results
class_result_list <- "many-models-classification-results-list.rds"
reg_result_list <- "many-models-regression-results-list.rds"
v001_path <- 'data/aux data/many models results/2018-10-21-v001'

main_path <- v001_path


classification_results_all <- read_rds(paste0(main_path,"/",class_result_list))
regression_results_all <- read_rds(paste0(main_path,"/",reg_result_list))


#1) classification results

class_results_df <- 
classification_results_all %>% 
  map(~keep(.x,grepl("results",names(.x)))) %>%
  compact() %>% 
  map(~.x[[1]]) %>% 
  bind_rows()

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
         , Data_Group = ifelse(grepl("Radii",Model),"3) Radii",Data_Group)
         , Data_Group = ifelse(grepl("Zip",Model),"2) Zip",Data_Group)
         ) %>% 
  left_join(select(class_results_df,model,AUC), by =c("Model"="model")) %>% 
  mutate(`AUC Text` = paste0("AUC: ",round(AUC,3)))

auc_compare_chart <- 
class_roc_df %>% 
  # filter(Model=="Base Prob GLM") %>% 
  ggplot()+
  aes(x = FPR, y = TPR,  group = Model)+
  geom_line()+
  geom_text(aes(x = 1,y = 0, label = `AUC Text`), hjust = 1, vjust = -1)+
  facet_grid(Model_Group~Data_Group)+
  theme_bw()+
  labs(title = "Comparrison of Model ROC Curves"
       , x = "False Positive Rate (1-Specificity)"
       , y = "True Positive Rate (Sensitivity)"
       , color = "Model")


png(filename = "Writing/Sections/tables and figures/compare_model_roc_curves.png",
    width = 480, height = 480, units = "px", pointsize = 12
    ,type = c("windows", "cairo", "cairo-png"))
auc_compare_chart
dev.off()

