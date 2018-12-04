#' Read in single or multiple ESR spectra
#' 
#' Use this function to import a series of associated ESR spectra into R.
#' 
#' This is a wrapper function for \code{read.table} and \code{readBin}. 
#' The function should be
#' used to read in a series of associated ESR spectrum files. A list with all
#' spectrum data is returned, which can be passed to \code{plot_Spectrum}
#' for plotting the spectra.
#' 
#' **Binary formats**
#' 
#' This function is able to read in binary spectrum files produced by Bruker 
#' ESR devices. By default (\code{device = 'auto'}), the function assumes the
#' proper mode of the vector (integer or numeric), endianness and number of
#' bytes per element based on the file extension. Currently, the following 
#' devices are supported:
#' 
#' - **Bruker ESP300-E** (*.SPC* files): `what = 'int', endian = 'big', size = 4`
#' - **Bruker ELEXSYS500** (*.DTA* files): `what = 'numeric', endian = 'big', size = 8`
#' - **Bruker EMXplus (old)** (*.SPC* files): `what = 'numeric', endian = 'little', size = 4`
#'  
#' Note that the Bruker ESP300-E and EMXplus devices share a common file
#' extension (.SPC) and that `device = 'auto'` (the default) will always
#' assume that the SPC file is from a ESP300-E. If your SPC file is
#' from a EMXplus, however, you should manually specify this using
#' `device = 'EMXplus'`.
#'  
#' 
#' @param file \code{\link{character}} (**required**): 
#' file path or directory where the spectra files are stored.
#' 
#' @param device [character] (*with default*):
#' Manually specify the device the spectrum files were produced by.
#' By default, the proper binary format is deduced from the file
#' extension, which may however fail in case of ambiguous file 
#' endings. See details.
#' 
#' Allowed options are:
#' - `"auto"` (the default)
#' - `"ESP300-E"` (.SPC)
#' - `"ELEXSYS500"` (.DTA)
#' - `"EMXplus"` (.SPC)
#' 
#' @param ... further arguments 
#' (e.g., \code{n} to specify the number of datapoints; \code{sw} to specify
#' the sweep width).
#' 
#' @return Returns a terminal output. In addition an
#' \code{\link{R6Class}} object is returned. \cr
#' 
#' @export
#' 
#' @author Christoph Burow, University of Cologne (Germany)
#' 
#' @seealso \code{\link{read.table}}, \code{\link{readBin}}, \code{\link{read.csv}}
#' 
#' @references In progress
#' 
#' @examples
#' 
#' # Import ASCII text file
#' file1 <- system.file("extdata", "coral.txt", package = "ESR")
#' spec1 <- read_Spectrum(file1)
#' 
#' # Import .zip archive
#' file2 <- system.file("extdata", "mollusc.zip", package = "ESR") 
#' spec2 <- read_Spectrum(file2)
#' 
#' # Import Bruker ESP300-E raw binary spectrum
#' file3 <- system.file("extdata", "mollusc.SPC", package = "ESR")
#' spec3 <- read_Spectrum(file3)
#' 
#' # Import Bruker ELEXSYS500 spectrum (ASCII)
#' file4 <- system.file("extdata", "dpph.ASC", package = "ESR")
#' spec4 <- read_Spectrum(file4)
#' 
#' # Import Bruker ELEXSYS500 raw binary spectrum
#' file5 <- system.file("extdata", "quartz.DTA", package = "ESR")
#' spec5 <- read_Spectrum(file5)
#' 
#' # Import Bruker EMXplus raw binary spectrum
#' file6 <- system.file("extdata", "DL_alanine.spc", package = "ESR")
#' spec6 <- read_Spectrum(file6, device = "EMXplus")
#' 
#' # Import all example data sets at once by providing only the directory
#' dir <- system.file("extdata", package = "ESR")
#' specs <- read_Spectrum(dir)
#' 
#' @md
#' @export read_Spectrum
read_Spectrum <- function(file, device = "auto", ...) {
  
  ## SUPPORTED DEVICES ----
  devices <- list(bruker = c("auto", "ESP300-E", "ELEXSYS500", "EMXplus"))
  if (!is.null(device))
    if (!device %in% unlist(devices))
      stop("Unknown device. Only the following are supported: ", paste(unlist(devices), collapse = ", "), call. = FALSE)
  
  ## ADDITIONAL ARGS ----
  extraArgs <- list(...)
  verbose <- ifelse("verbose" %in% names(extraArgs), extraArgs$verbose, TRUE)
  trace <- ifelse("trace" %in% names(extraArgs), extraArgs$trace, FALSE)
  sweep_width <- ifelse("sw" %in% names(extraArgs), extraArgs$sw, NA)
  
  #### -------------------------------------------------------------------------
  ## HELPER FUNCTIONS
  #### -------------------------------------------------------------------------
  
  ## ---------------------------------------
  ## Check data type by extension
  ## ---------------------------------------
  check_type <- function(f) {
    valid_ext <- c("txt", "zip", "asc", "spc", "dta")
    ext <- substr(tolower(f), nchar(f)-2, nchar(f))
    val <- match(ext, valid_ext)
    
    if (is.na(val)) {
      file_list <- list.files(f, paste0(valid_ext, collapse = "|"), ignore.case = TRUE)
      if (length(file_list) == 0) {
        stop(paste("Invalid file extension:", ext), call. = FALSE)
      }
      ext <- list(file_list)
    }
    return(ext)
  }
  
  ## ---------------------------------------
  ## Retrieve magnetic field values
  ## (only called for .DTA and .SPC files)
  ## ---------------------------------------
  get_xval <- function(par, cf, sw, records) {
    if (!is.null(par)) {
      center_field <- as.numeric(par[par==cf, 2])
      if (is.na(sweep_width))
        sweep_width <- as.numeric(par[par==sw, 2])
      start <- center_field - sweep_width[1] / 2 # DSC can contain duplicate entries
      end <- center_field + sweep_width[1] / 2
      
      if (length(sweep_width) == 0)
        stop("Couldn't find information on sweep width, importing file cancelled.", call. = FALSE)
      xval <- seq(from = start, to = end, by = (end - start) / (records - 1))
      
    } else {
      xval <- seq(1, records, 1)
    }
  }
  
  ## ---------------------------------------
  ## Read data functions
  ## ---------------------------------------
  read_data <- function(f, type, ...) {
    
    ## TXT
    ## -----------------------------------------------
    if (type == "txt") {
      df <- fread(f, stringsAsFactors = FALSE)
      if (ncol(df) == 3) set(df, j = 1L, value = NULL)
      par <- NULL
    }#EndOf::txt
    
    
    ## ASC
    ## -----------------------------------------------
    if (type == "asc") {
      df <- as.data.frame(read.csv(f, sep = ""))
      if (ncol(df) != 2) set(df, j = c(1L, 4L, 5L), value = NULL)
      par <- NULL
    }#EndOf::asc
    
    
    ## ZIP (not supported)
    ## -----------------------------------------------
    if (type == "zip") {
      df <- NULL
      par <- NULL
      message(".zip files are currently not supported. The file was skipped.")
    }#EndOf::zip
    
    
    ## SPC
    ## -----------------------------------------------
    if (type == "spc") {
      
      if (device == "auto" || device == "ESP300-E")
        df <- as.data.frame(readBin(f, "int", n = file.info(f)$size, endian = "big", size = 4))
      else if (device == "EMXplus")
        df <- as.data.frame(readBin(f, "numeric", n = file.info(f)$size, endian = "little", size = 4))
    
      par <- tryCatch(
        read.table(gsub(".spc$", ".par", f, ignore.case = TRUE), stringsAsFactors = FALSE),
        error = function(e) { NULL },
        warning = function(w) { NULL }
      )
      xval <- get_xval(par, "HCF", "HSW", records = nrow(df))
      
      df <- cbind(xval, df)
    }#EndOf::spc
    
    
    ## DTA
    ## -----------------------------------------------
    if (type == "dta") {
      df <- as.data.frame(readBin(f, "numeric", n = file.info(f)$size, endian = "big", size = 8))

      # check if the file contains 2D data
      # secondary data is stored in a .YGF file
      if (file.exists(gsub(".dta", ".ygf", f, ignore.case = TRUE))) {
        f2 <- gsub(".dta", ".ygf", f, ignore.case = TRUE)
        secondary_data <- readBin(f2, "numeric", 
                                  n = file.info(f2)$size, 
                                  endian = "big", size = 8)
      } else {
        secondary_data <- NULL
      }
      
      # fetch parameters from DSC file
      par <- tryCatch({
        f2 <- gsub(".dta", ".dsc", f, ignore.case = TRUE)
        x <- readLines(f2)
        x <- gsub("\\*", "", x)  # delete all * symbols
        x <- gsub(".DVC", "", x)
        x <- x[x != ""]  # remove all empty strings
        x <- gsub("[,|\t|']", " ", x)
        x <- gsub(" +", " ", x)
        
        y <- lapply(x, function(s) unlist(strsplit(s, " +")))
        
        for (i in seq_along(y)) {
          y[[i]] <- y[[i]][y[[i]] != ""]
          
          if (length(y[[i]]) == 3L) {
            
            # Parameter | text | text -> 'paramter | text text'
            if (nchar(y[[i]][3]) > 3) {
              y[[i]] <- c(y[[i]][1], paste(y[[i]][2], y[[i]][3]))
            } else {
              # Parameter | value | unit -> 'parameter (unit) | value'
              y[[i]][1] <- paste0(y[[i]][1], " (", y[[i]][3], ")")
              y[[i]] <- y[[i]][1:2]
              
            }
          }
          
          if (length(y[[i]]) > 2L) {
            y[[i]] <- NA
          }
        }
        y <- y[!is.na(y)]
        df2 <- data.frame(matrix(nrow = length(y), ncol = 2))
        for (i in seq_along(y)) {
          df2[i, ] <- y[[i]]
        }
        df2 <- df2[-which(df2=="Item"),]
        df2 <- df2[-which(df2=="Documentational"),]
      },
      error = function(e) { NULL },
      warning = function(w) { NULL }
      )
      
      if (!is.null(secondary_data))
        size <- nrow(df) / length(secondary_data)
      else
        size <- nrow(df)
      
      xval <- get_xval(par, "CenterField (G)", "SweepWidth (G)", records = size)
      
      df <- cbind(xval, df)
      
      if (!is.null(secondary_data))
        df$sec <- as.vector(sapply(secondary_data, function(x) rep(x, size)))
        
    }#EndOf::dta
    
    return(list(df, par))
  }
  
  ## ---------------------------------------------------------------------------
  ## MAIN
  ## ---------------------------------------------------------------------------
  type <- check_type(file)
  
  if (is.list(type)) {
    last_char <- substr(file, start = nchar(file), nchar(file))
    if (last_char != "/") file <- paste0(file, "/")
    
    files <- lapply(unlist(type), function(x) paste0(file, x))
    
    res <- lapply(files, function(x) read_data(x, substr(tolower(x), nchar(x)-2, nchar(x))))
    
    obj <- vector("list", length = length(res))
    
    for (i in seq_along(obj)) {
      obj[[i]] <- ESR.Spectrum$new()
      obj[[i]]$set_data(res[[i]][[1]])
      obj[[i]]$set_par(res[[i]][[2]])
      origin <- ifelse(grepl("/", files[i]), gsub(".*/(?!.*/)", "", files[i], perl = TRUE), files[i])
      obj[[i]]$set_origin(origin)
    }
  }
  
  if (!is.list(type)) {
    df <- read_data(file, type)
    names(df) <- c("data", "par")
    
    if (all(sapply(df, is.null)))
      return(NULL)
    
    if (ncol(df$data) == 2) {
      obj <- ESR.Spectrum$new()
    } else if (ncol(df$data) == 3) {
      obj <- ESR.Spectrum.2D$new()
    }
    
    obj$set_data(df$data)
    obj$set_par(df$par)
    origin <- ifelse(grepl("/", file), gsub(".*/(?!.*/)", "", file, perl = TRUE), file)
    obj$set_origin(origin)
    
    if (inherits(obj, "ESR.Spectrum.2D"))
      obj$secondary_dimension <- obj$parameter[which(obj$parameter[,1] == "YNAM"), 2]
  }
  
  ## CONSOLE ----
  if (trace) {
    message("\n The following files were imported:")
    message(ifelse(exists("files"), paste(unlist(files), collapse = "\n"), file)) 
  }
  if (verbose) message("\n Job done!")
  
  ## RETURN ----
  
  invisible(obj)
}