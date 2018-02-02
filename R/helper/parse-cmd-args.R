

parse_cmd_args <- function() {
  
  # script arguments:
  # args <- commandArgs(TRUE)
  # DL <- as.character(args[1]) # 'skip-dl' bypasses the download steps
  # PP <- as.character(args[2]) # 'skip-pp' bypasses the pre-processing steps
  # dev <- as.character(args[3]) # 'run-dev' train the models with sample data, to go much faster
  # run_radii <- as.character(args[4]) # 'run-radii' re-runs the full radii indexing operation. Else, loads from disk
  # 
  
  option_list <- list(
    make_option(c("-d", "--skip-dl"), action="store_true", default=FALSE,
                help="Skip the download script to save time"),
    make_option(c("-p", "--skip-pp"), action="store_true", default=FALSE,
                help="Skip the pre-processing steps to same time"),
    make_option(c("-r", "--run-radii"), action="store_true", default=FALSE,
                help="Should the radii indexing be run? Default is not to run (very time intensive)"),
    make_option(c("-s", "--run-sample"), action="store_true", default=FALSE,
                help="Run the model on sample data")
  )
  
  parser <- OptionParser(usage="%prog [options]", option_list=option_list)
  args <- parse_args(parser)
  args
}