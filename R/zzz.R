# Set namespace .LuminescenceEnv ------------------------------------------
.ESREnv <- new.env(parent = emptyenv())  

.onAttach <- function(libname,pkgname){
  try(packageStartupMessage(paste("Welcome to the R package ESR version ",
                                  packageDescription(pkg="ESR")$Version,
                                  " [Built: ",
                                  strsplit(packageDescription(pkg="ESR")$Packaged, ";")[[1]][1],
                                  "]", sep="")), silent=TRUE)                        
}
