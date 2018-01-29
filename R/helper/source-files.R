
# source the requisite files

source("R/helper/helper-functions.R")
message("runnning makefile... ")
project_makefile()

message("Sourcing project functions...")
source("R/01-download-pluto-data.R")
source("R/02-download-pad-data.R")
source("R/03-download-sales-data.R")
source("R/04-combine-sales-and-pad.R")
source("R/05-combine-pluto-and-sales.R")
source("R/06-create-base-data.R")
source("R/07-create-zipcode-data.R")
source("R/08-create-radii-data.R")
source("R/09-run-probability-model.R")
source("R/10-run-sales-model.R")


