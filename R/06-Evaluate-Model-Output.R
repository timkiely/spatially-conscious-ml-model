

# Evaluate ----------------------------------------------------------------
train_out %>%
  ggplot()+
  aes(x=id,color=modelName)+
  geom_point(aes(y=Test_RMSE),size=2)+
  facet_wrap(~modelName)

train_out %>% 
  select(idx, id, modelName, test.Y,y_hat) %>%
  unnest() %>% 
  ggplot()+
  aes(x = test.Y, y = y_hat)+
  geom_point()+
  geom_smooth()+
  facet_grid(id~modelName)+
  coord_cartesian(xlim = c(0,4000), ylim = c(0,4000))


varImp(train_out$modelFits[train_out$modelName=='xgbModel' & train_out$id=='vtreated'][[1]])


train_out %>% select(idx, id, modelName, contains("Test_")) %>% arrange(Test_RMSE)
