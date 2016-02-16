if(Sys.getenv("USERNAME")=="SQHW2" || Sys.getenv("USER")=="travis2") {
  if(!interactive()) Sys.setenv(RGL_USE_NULL=TRUE) #disable RGL for headless machines
  library(devtools)
  library(tools)
  myPackage = "./forestFloor"
  
  #remake of devtools::run_examples
  run_examples_simple_no_Roxygenize = function(
    pkg = ".",
    topics = NULL,
    run = TRUE
  ) {
    pkg = as.package(pkg)
    if(is.null(topics)) {
      files = devtools:::rd_files(pkg)
    } else {
      files = unlist(lapply(topics,function(i) {
        fileName = devtools:::find_pkg_topic(topic=i)
        if(is.null(fileName)) stop(paste("no .Rd-file match found for[",i,"] when looking up topics"))
        return(fileName)
      }))
      path_man <- file.path(pkg$path, "man")
      files = paste0(path_man,"/",files)
      names(files) = basename(files)
    }
    lapply(files, devtools:::run_example,run=run)
    "Done"
  }
  
  #if a package check, jump out of test fold and into check source folder
  pa = getwd()
  if(!interactive()) setwd("../00_pkg_src") else setwd("../.")
  
  run_examples_simple_no_Roxygenize(pkg=myPackage, run=FALSE) #override dontrun
  setwd(pa)
}