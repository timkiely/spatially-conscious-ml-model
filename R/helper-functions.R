

######################################################
## Helper Functions
######################################################

## tidy up fields with dollar signs
dollar.fix <- function(var){
  var <- gsub("[^[:^punct:].]","",var,perl=T)
  var <- as.numeric(as.character(var))
  return(var)
}

## remove commas
comma.rem <- function(x){
  if(class(x)=="character"){
    x <- gsub(",","",x)
  }
  return(x)
}

## check if url exists and download if it does 
check.get <- function(fun.url){ 
  tmp <- url.exists(fun.url,.header=T)
  
  out <- NA
  
  if((tmp['status']=="200") | sum(tmp==T)>0){
    tmp <- tempfile()
    download.file(fun.url, 
                  destfile=tmp, method="curl", quiet=T)
    
    out <- read.xls(tmp,sheet=1,pattern="BOROUGH",as.is=T)
  }
  return(out)
}