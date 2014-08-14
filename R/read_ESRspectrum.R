read_ESRspectrum <-
structure(function(# Read in multiple associated ESR spectra
  ### Use this function to import a series of associated ESR spectra into R.
  
  # ===========================================================================
  ##author<<
  ## Christoph Burow, University of Cologne (Germany)
  
  ##section<<
  ## version 0.1 [2013-12-03]
  # ===========================================================================
  folder,
  ### \code{\link{character}} (required): directory where the spectra files are 
  ### stored. Alternatively, a compressed zip file can be provided. See details!
  file.ext = "txt",
  ### \code{\link{character}}: import only files with a certain 
  ### extension
  header = TRUE,
  ### \code{\link{logical}}: value indicating whether the file contains the names of the 
  ### variables as its first line.
  sep = ""
  ### \code{\link{character}}: the field separator character. Values on each line 
  ### of the file are separated by this character. If sep = "" (the default for read.table) 
  ### the separator is 'white space', that is one or more spaces, tabs, newlines 
  ### or carriage returns.
  
) {

  # check if zip file
  is_zip<- function(x) {
  
    sub.ext<- substring(x, first = nchar(x)-3, last = nchar(x))
    
    if(length(grep(".ZIP", sub.ext, ignore.case  = T)) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  get_filelist<- function(x) {
    if(is_zip(x) == TRUE) {
      
      temp.filelist<- unzip(x, list = TRUE)$Name
      filelist<- temp.filelist[grep(file.ext, temp.filelist, ignore.case = TRUE)]
      
      return(filelist)
   
    } else {
      # get a list of all files that match the pattern
      filelist<- list.files(folder, 
                            pattern = paste("*.", file.ext, sep=""),
                            ignore.case = TRUE)
      return(filelist)
      #"D:/R/ESR package/datasets/spectra/ba01"
      #"D:/R/ESR package/datasets/spectra/ba01.zip"
    }
  }
  
  if(is_zip(folder) == TRUE) {
  
    filelist<- get_filelist(folder)
    
    for(i in 1:length(filelist)) {
       data<- lapply(filelist,
                     function(x) read.table(unz(folder, x), header = header, sep = sep))
    }
    
  } else {
    
    filelist<- get_filelist(folder)
    
    # import all files into a list
    data<- lapply(filelist, 
                  function(x) read.table(paste(folder,"/", x, sep=""), header = header, sep = sep))
    
  }
  
  # drop index column if present
  if(length(data[[1]]) == 3) {
    data<- lapply(data,
                  function(x) {x[1]<- NULL; x})
  }
  
  
  # use filenames (w/o extension) as list names 
  names(data)<- gsub(paste(".", file.ext, sep = ""), "", filelist, ignore.case = TRUE)
  
  
  
  ##==========================================================================##
  ## CONSOLE OUTPUT
  ##==========================================================================##
  
  cat(paste("\nThe following files have successfully been imported: \n"))
  cat(paste(filelist, collapse = "\n"))
  
  ##==========================================================================##
  ## RETURN VALUE
  ##==========================================================================##
  
  
  newRLumDataCurve.read_ESRspectrum <- set_RLum.Data.Curve(
    data = t(as.matrix(data)),
    info = list(filelist),
    recordType = "ESR",
    curveType = "measured")
  
  invisible(newRLumDataCurve.read_ESRspectrum)
  
  ### Returns a terminal output. In addition an 
  ### \code{\linkS4class{RLum.Results}} object is 
  ### returned.
  ### \cr\cr
  ### The output should be accessed using the function 
  ### \code{\link{get_RLum.Results}}
  
  
  ##details<<
  ## This is a wrapper function for \code{read.table}.
  ## The function should be used to read in a series of associated ESR spectrum
  ## files. A list with all spectrum data is returned, which can be passed to
  ## \code{plot_ESRspectrum} for plotting the spectra. \cr\cr
  ## \bold{Compressed zip archives}
  ## For \code{folder} a zip archive can be provided. The file has to have
  ## a .zip file extension and must contain all spectrum files in no subfolder.
  
  ##seealso<<
  ## \code{\link{read.table}}
  
  ##references<<
  ## In progress
  
  ##note<<
  ## # 
  
}, ex = function() {
  ##no example yet
  print("SORRY, NO EXAMPLE YET")
})
