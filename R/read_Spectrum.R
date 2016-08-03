#' Read in single or multiple ESR spectra
#' 
#' Use this function to import a series of associated ESR spectra into R.
#' 
#' This is a wrapper function for \code{read.table}. The function should be
#' used to read in a series of associated ESR spectrum files. A list with all
#' spectrum data is returned, which can be passed to \code{plot_Spectrum}
#' for plotting the spectra.
#' 
#' @param file \code{\link{character}} (required): file path or directory where
#' the spectra files are stored.
#' @param ... further arguments (e.g., \code{n} to specify the number of datapoints).
#' @return Returns a terminal output. In addition an
#' \code{\link{R6Class}} object is returned. \cr
#' @export
#' @author Christoph Burow, University of Cologne (Germany)
#' @seealso \code{\link{read.table}}, \code{\link{readBin}}, \code{\link{read.csv}}
#' @references In progress
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
#' # Import all example data sets at once by providing only the directory
#' dir <- system.file("extdata", package = "ESR")
#' specs <- read_Spectrum(dir)
#' 
#' @export read_Spectrum
read_Spectrum <- function(file, ...) {
  
  ## ... ARGS ----
  extraArgs <- list(...)
  verbose <- ifelse("verbose" %in% names(extraArgs), extraArgs$verbose, TRUE)
  trace <- ifelse("trace" %in% names(extraArgs), extraArgs$trace, FALSE)
  records <- ifelse("n" %in% names(extraArgs), extraArgs$n, 1024L)
  sweep_width <- ifelse("sw" %in% names(extraArgs), extraArgs$sw, NA)
  
  ## FUNCTIONS ----
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
  
  get_xval <- function(par, cf, sw, div = 1) {
    if (!is.null(par)) {
      center_field <- as.numeric(par[par==cf, 2])
      if (is.na(sweep_width))
        sweep_width <- as.numeric(par[par==sw, 2]) / div
      start <- center_field - sweep_width
      end <- center_field + sweep_width
      # print(paste(center_field, sweep_width))
      
      xval <- seq(from = start, to = end, by = (end - start) / (records - 1))
    } else {
      xval <- seq(1, records, 1)
    }
  }
  
  read_data <- function(f, type, ...) {
    if (type == "txt") {
      df <- fread(f, stringsAsFactors = FALSE)
      if (ncol(df) == 3) set(df, j = 1L, value = NULL)
      par <- NULL
    }#EndOf::txt
    
    if (type == "asc") {
      df <- as.data.table(read.csv(f, sep = ""))
      if (ncol(df) != 2) set(df, j = c(1L, 4L, 5L), value = NULL)
      par <- NULL
    }#EndOf::asc
    
    if (type == "zip") {
      df <- NULL
      par <- NULL
      message(".zip files are currently not supported. The file was skipped.")
    }#EndOf::zip
    
    if (type == "spc") {
      df <- as.data.table(readBin(f, "int", n = records, endian = "big", size = 4))
      
      par <- tryCatch(
        read.table(gsub(".spc", ".par", f, ignore.case = TRUE), stringsAsFactors = FALSE),
        error = function(e) { NULL },
        warning = function(w) { NULL }
      )
      xval <- get_xval(par, "HCF", "HSW")
      
      df <- cbind(xval, df)
    }#EndOf::spc
    
    if (type == "dta") {
      df <- as.data.table(readBin(f, "numeric", n = records, endian = "big", size = 8))
      
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
            y[[i]][1] <- paste0(y[[i]][1], " (", y[[i]][3], ")")
            y[[i]] <- y[[i]][1:2]
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
      
      xval <- get_xval(par, "CenterField (G)", "SweepWidth (G)", 2)
      
      df <- cbind(xval, df)
      
    }#EndOf::dta
    
    return(list(df, par))
  }
  
  ## CHECK TYPE ----
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
    
    obj <- ESR.Spectrum$new()
    obj$set_data(df$data)
    obj$set_par(df$par)
    origin <- ifelse(grepl("/", file), gsub(".*/(?!.*/)", "", file, perl = TRUE), file)
    obj$set_origin(origin)
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