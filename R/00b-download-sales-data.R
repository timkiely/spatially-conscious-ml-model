

library(gdata)
library(httr)
library(RCurl)
library(tidyverse)
source("helper-functions.R")



## Note: takes about 50 minutes to run
# years and boroughs to pull 
years <- c(paste(200,6:9,sep=""),paste(20,10:16,sep=""))
boroughs <- c("manhattan","bronx","brooklyn","queens","statenisland")

## Pulling data in for loop, easier to troubleshoot
df_list <- list()
name.vec <- vector()
timer.vec <- vector()

# there are multiple different versions of the download URL, so loop through each until the right file is found0
ptm.outer <- proc.time()
for(j in 1:length(years)){
  cat("begin outer loop ",j," out of ",length(years),"\n",sep="")
  ptm <- proc.time()
  for(i in 1:length(boroughs)){
    cat("  begin inner loop ",i,"\n",sep="")
    
    counter <- length(df_list) + 1
    identifier <- paste(years[j],"_",boroughs[i],".xls",sep="")
    
    
    # there are severl different URL formats. We will try each one for every borough, year combo
    url1.tmp <- "https://www1.nyc.gov/assets/finance/downloads/pdf/rolling_sales/annualized-sales/"
    url1 <- paste(url1.tmp,years[j],"/",identifier,sep="") 
    
    url2.tmp <-  "https://www1.nyc.gov/assets/finance/downloads/pdf/rolling_sales/annualized-sales/"
    url2 <- paste(url2.tmp,identifier,sep="")
    
    url3.tmp <- "https://www1.nyc.gov/assets/finance/downloads/pdf/09pdf/rolling_sales/sales_"
    url3 <- paste(url3.tmp,identifier,sep="")
    
    url4.tmp <- "https://www1.nyc.gov/assets/finance/downloads/excel/rolling_sales/sales_"
    url4 <- paste(url4.tmp,identifier,sep="")
    
    url5.tmp <- "https://www1.nyc.gov/assets/finance/downloads/sales_"
    url5 <- paste(url5.tmp,boroughs[i],"_",substr(years[j],start=3,stop=4),".xls",sep="")
    
    url.vec <- c(url1,url2,url3,url4,url5)
    
    
    # check each URL and download if there is data:
    out <- lapply(url.vec, function(x) check.get(fun.url=x))
    
    # discard the failed URL attempts:
    keep <- which(unlist(lapply(out, function(x) class(x)))=="data.frame")
    
    
    if(length(keep)==1){
      df_list[[counter]] <- try(out[[keep]],silent=T)
      name.vec[counter] <- paste(boroughs[i],substr(years[j],start=3,stop=4),sep="")
    }  
    cat("  end inner loop ",i,"\n",sep="")
  }
  timer.vec[j] <- as.numeric((proc.time() - ptm)[3])
  cat("time in outer loop ",j,": ",timer.vec[j]," seconds\n",sep="")
}
loop.time <- proc.time() - ptm.outer
# user      system  elapsed 
# 2804.116   63.576 3010.354 


cat("data download finished\n")
## data download finished


names(df_list) <- name.vec

## making column names consistent
df_list <- lapply(df_list, function(x) {
  colnames(x) <- colnames(df_list[[1]])
  return(x)
})

## Changing factors to character to avoid errors downstream
df_list <- lapply(df_list, function(df.tmp){
  change.cols <- as.numeric(
    which(
      sapply(df.tmp, class)!="character"
    )
  )
  df.tmp[,change.cols] <- sapply(df.tmp[,change.cols], as.character)
  return(df.tmp)
}
)

## putting list into single dataframe
propsale.df <- bind_rows(df_list)

# save the raw results
write_rds(propsale.df, "data/nyc-sales-data-raw.rds")




