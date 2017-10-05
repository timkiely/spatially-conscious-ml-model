
library(tidyverse)
all_files <- dir("PLUTO_ARCHIVES")


file_we_want <- dir(paste0("PLUTO_ARCHIVES/",all_files[1]))[grep("mn",dir(paste0("PLUTO_ARCHIVES/",all_files[1])))]
pluto_tmp <- readr::read_csv(paste0("PLUTO_ARCHIVES/",all_files[1],"/",file_we_want),n_max = 10, col_names = F)


dir("PLUTO_ARCHIVES/nyc_pluto_15v1")

dir("PLUTO_ARCHIVES/nyc_pluto_16v1")

plut_2015 <- readr::read_csv("PLUTO_ARCHIVES/nyc_pluto_15v1/MN.csv")
  
plut_2016 <- readr::read_csv("PLUTO_ARCHIVES/nyc_pluto_16v1/MN.csv")

pluto_15_16<-bind_rows(list("2015"=plut_2015,"2016"= plut_2016),.id="Type")


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

percent_change_dat<-
pluto_15_16 %>% 
  select(Type,BldgClass,BBL,YearBuilt,XCoord,YCoord,AssessLand,AssessTot) %>% 
  group_by(BBL) %>% 
  arrange(Type) %>% 
  mutate(Year_change = AssessTot - lag(AssessTot,1)
         ,Perc_change = (AssessTot - lag(AssessTot,1))/AssessTot) %>% 
  filter(!is.na(Year_change),!is.na(Perc_change),is.finite(Perc_change)) %>% 
  ungroup() %>% 
  select(BldgClass,Year_change,Perc_change)
  
  percent_change_dat %>%  
    group_by(BldgClass) %>% 
    filter(Perc_change!=0) %>% 
    summarise(min(Perc_change)
              ,max(Perc_change)
              ,mean(Perc_change,na.rm=T)
              ,mode = Mode(Perc_change)
              ,sum(Perc_change==mode)
              , n()
              ) %>% View

sd(percent_change_dat$Perc_change, na.rm=T)
  
ggplot(percent_change_dat)+
  aes(x=Perc_change)+
  geom_histogram(binwidth=0.01)+xlim(c(-1,1))

  
  summarise(count=n()) %>% 
  arrange(count) %>% 
  select(count)

plut_16_dat <-
  plut_2016 %>% 
  
  
  
rawdata <- readr::read_csv("/Users/timkiely/Downloads/rawdata.csv")
  