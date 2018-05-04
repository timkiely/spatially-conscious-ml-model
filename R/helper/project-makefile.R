# checks for project directory structure and creates it if it doesn't exist
project_makefile <- function() {
  
  message("Checking project directory structure...")
  
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
  
  if(!"analysis" %in% dir()){
    dir.create("analysis")
    message("Created directory 'analysis'")
  }
  
  if(!"results" %in% dir("analysis")){
    dir.create("analysis/results")
    message("Created directory 'analysis/results'")
  }
  
  if(!"prob" %in% dir("analysis/results")){
    dir.create("analysis/results/prob")
    message("Created directory 'analysis/results/prob'")
  }
  
  if(!"sales" %in% dir("analysis/results")){
    dir.create("analysis/results/sales")
    message("Created directory 'analysis/results/sales'")
  }
  
}