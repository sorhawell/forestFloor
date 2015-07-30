#sf8 neat function to help increase adaptability of wrappers, default args defined by wrapper. User can 
#input an alist and by this function the wrapper will append new args and overwrite conflicting arguments.
#one set of args either user or defaults are set as master
append.overwrite.alists= function(masterArgs,slaveArgs) {
  slaveArgs.to.overwrite = names(slaveArgs) %in% names(masterArgs)
  for(i in which(slaveArgs.to.overwrite))  slaveArgs[i] = masterArgs[match(names(slaveArgs[i]),names(masterArgs))]
  masterArgs.to.append = !(names(masterArgs) %in% names(slaveArgs))
  c(slaveArgs,masterArgs[masterArgs.to.append])
}
