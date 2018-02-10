# checks for project directory structure and creates it if it doesn't exist
project_makefile <- function() {
  
  # data
  if(!"data" %in% dir()){
    dir.create("data")
    message("Created directory /data")
  }
  
  # data/aux data
  if(!"aux data" %in% dir("data")){
    dir.create("data/aux data")
    message("Created directory 'data/aux data'")
  }
  
  # data/processing steps
  if(!"processing steps" %in% dir("data")){
    dir.create("data/processing steps")
    message("Created directory '/data/processing steps'")
  }
  
}