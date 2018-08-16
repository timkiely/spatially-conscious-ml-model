my_kable <- function(t, caption = NULL, label = "", latex_options = "scale_down"){
  
  label2 <- paste0("\\label{tab:",label,"} ", caption)

  t %>% 
    knitr::kable("latex"
                 , caption = label2
                 , booktabs = TRUE) %>% 
    kableExtra::kable_styling(latex_options = latex_options)
}