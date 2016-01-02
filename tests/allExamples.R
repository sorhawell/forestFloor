library(devtools)
library(tools)

#remake of devtools::run_examples
run_examples_simple_no_Roxygenize = function(pkg = ".",
  topics = NULL,
  show = TRUE,
  test = FALSE,
  run = TRUE,
  reload = TRUE) {

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
    void = lapply(files, devtools:::run_example,run=run, show=show, test=test)
  "Done"
}



run_examples_simple_no_Roxygenize(run=FALSE)
