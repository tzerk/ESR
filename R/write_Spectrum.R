#' Write spectrum files to disk
#' 
#' Export ESR spectrum objects as .SPC/.PAR or .TXT files.
#'
#' **SPC file format**
#' 
#' If `type = 'SPC'` this function will write a binary file as is produced
#' by a Bruker ESP300-E spectrometer. These files use a big-endian byte order
#' with 4 bytes per element in the byte stream. This format only stores integer
#' values, so exporting a floating-point number spectrum might introduce 
#' more or less severe rounding errors. To counteract this, the function
#' multiplies all intensity values by 1000 before exporting. This preserves 
#' relative differences in spectra, but absolute intensity values may no longer
#' be interpreted at face value.
#' 
#' The measurement parameters are written to a separate `<file>.PAR` file with
#' two whitespaces as field seperator and macintosh style carriage return
#' line breaks. 
#'
#' @param x `ESR.Spectrum-class` (**required**):
#' An object of class `ESR.Spectrum` that is written to the disk.
#' 
#' @param file [character] (**required**):
#' A character string naming a file for writing.
#' 
#' @param type [character] (*with default*):
#' Indicates the file type the data is exported as:
#' 
#' - `"SPC"`: (default) Binary data format as produced by the ESP300-E spectrometer. 
#' Parameters are written to a seperate `.PAR` file. See details about encoding.
#' - `"TXT"`: Plain text file of the spectrum's x,y-values
#' 
#' @param sep [character] (*with default*): 
#' The field separator string. Values within each row of x are separated by 
#' this string. Only used if `type = 'TXT'`.
#' 
#' @param gvalue [logical] (*with default*):
#' Export the spectrum with g-values instead of magnetic field values. Requires
#' all necessary information in the `ESR.Spectrum` object (microwave frequency,
#' magnetic field values, center field, sweep width). Only used if `type = 'TXT'`.
#' 
#' @param ... currently not used.
#'
#' @return
#' Write files to the disk, no further output provided.
#'
#' @examples
#' 
#' \dontrun{
#' 
#' ## Load example data set
#' file <- system.file("inst/extdata/quartz.DTA", package = "ESR")
#' 
#' ## Import the file
#' x <- read_Spectrum(file, verbose = FALSE) 
#' 
#' ## Export as ESP300-E .SPC/.PAR file set
#' write_Spectrum(x = x, 
#'                file = "~/exported.SPC", 
#'                type = "SPC")
#' 
#' ## Export as plain text
#' write_Spectrum(x = x, 
#'                file = "~/exported.txt", 
#'                type = "TXT", 
#'                gvalue = TRUE, 
#'                sep = ",")
#' }
#' 
#' @author Christoph Burow, University of Cologne (Germany)
#' 
#' @seealso [writeBin], [write.table]
#' 
#' @md
#' @export
write_Spectrum <- function(x, file, type = c("SPC", "TXT")[1], sep = "\t", gvalue = FALSE, ...) {
  
  # Check input ----
  type <- toupper(type)
  
  if (!match(type, c("SPC", "TXT"), nomatch = FALSE))
    stop("Argument 'type' currently only supports 'SPC' and 'TXT'.", call. = FALSE)
  
  if (type == "SPC" && !grepl(".spc$", file, ignore.case = TRUE))
    file <- paste0(file, ".SPC")
  
  if (type == "TXT" && !grepl(".txt$", file, ignore.case = TRUE))
    file <- paste0(file, ".txt")
  
  ## ---------------------------------------------------------------------------
  ## HELPER FUNCTIONS 
  ## ---------------------------------------------------------------------------
  
  ## This function checks all the parameter entries for a match and returns
  ## its value. 'p' is the parameter list inherited from the enclosing
  ## environment.
  check_par <- function(match, nomatch) {
    
    # take the value of the first match
    for (i in 1:length(match)) {
      if (match[i] %in% names(p)) {
        r <- p[match[i]]
        break
      } else {
        r <- NA
      }
    }
    
    # if there are no matches return the default value
    if (is.na(r))
      r <- nomatch
    
    return(r)
  }
  
  
  ## ---------------------------------------------------------------------------
  ## SPC 
  ## ---------------------------------------------------------------------------
  if (type == "SPC") {
    
    # a - open
    f <- file(file, open = "wb")
    
    ## b - write
    # (intensity values multiplied by 1000 to have large enough numbers to
    # minimise rounding errors)
    tryCatch({
      writeBin(
        as.integer(round(x$data$y * 1000, 0)),
        con = file, 
        endian = "big", 
        size = 4)
    }, warning = function(e)  {
      stop(paste("Unable to write file", file), call. = FALSE)
    }
    )
    
    # c - close
    close(f)
    
    ## Write: PAR (parameters, ascii)
    p <- setNames(x$parameter[,2], x$parameter[,1])
    
    # Title
    TITL <- check_par(c("TITL"), "unknown")
    # Date
    JDA <- check_par(c("JDA", "DATE"), "01-JAN_1950")
    # Time
    JTM <- check_par(c("JTM", "TIME"), "00:00:00")
    # No. Scans
    JNS <- check_par(c("JNS", "NbScansAcc", "NbScansDone"), -1)
    # Microwave Frequency
    MWFQ <- gsub("e+.*", "", check_par(c("MWFQ", "FrequencyMon (GHz)"), -1))
    # Microwave Power
    MWPW <- check_par(c("Power (mW)", "MWPW"), -1)
    # Magnetic field lowest
    GST <- check_par(c("XMIN", "GST"), -1)
    # Center Field
    HCF <- check_par(c("CenterField (G)", "HCF"), 0)
    # Sweep Width
    HSW <- check_par(c("GSI", "SweepWidth (G)", "HSW", "GSI"), -1)
    # Sweep width
    GSI <- HSW
    # Modulation Amplitude
    RMA <- check_par(c("ModAmp (G)", "RMA"), -1)
    # Conversion Time
    RCT <- check_par(c("ConvTime (ms)", "RCT"), -1)
    # Time Constant
    RTC <- check_par(c("TimeConst (ms)", "RTC"), -1)
    # No. data points
    XPTS <- check_par(c("XPTS", "Resolution", "A1RS"), -1)
    
    # build PAR data.frame
    par <- data.frame(
      JSS = 2,
      ADEV = 1,
      VERS = 772,
      TITL = TITL,
      JRE = "elexsys500",
      JDA = JDA,
      JTM = JTM,
      JNS = JNS,
      JSD = 1,
      JAR = "REP",
      MWFQ = MWFQ,
      MF = MWFQ,
      MWPW = MWPW,
      MP = MWPW,
      GST = GST,
      GSI = GSI,
      HCF = HCF,
      HSW = HSW,
      RMA = RMA,
      RCT = RCT,
      RTC = RTC,
      PPL = "ON",
      XPTS = XPTS,
      stringsAsFactors = FALSE
    )
    
    # transpose the data frame and column names as a separate column
    par <- data.frame(
      x = colnames(par),
      y = as.character(par[1, ])
    )
    
    ## a - open
    # open in binary write mode to allow non-native line breaks
    f <- file(gsub(".spc$", ".PAR", file, ignore.case = TRUE), open = "wb")
    
    ## b - write
    # use carriage return ('\\r') for macintosh style line breaks
    # (required by WinEPR 2.11b)
    write.table(
      par,
      file = f,
      sep = "  ",
      eol = "\r",
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
    
    ## c - close
    close(f)
  }
  
  ## ---------------------------------------------------------------------------
  ## TXT 
  ## ---------------------------------------------------------------------------
  if (type == "TXT") {
    
    ## try to calculate g-values. if it fails, use regular magnetic field 
    ## values instead
    if (gvalue) {
      
      df <- try(x$get_gvalues())
      
      if (inherits(df, "try-error")) {
        df <- x$data
        warning("Unable to convert to g-values.", call. = FALSE)
      }
      
    } else {
      df <- x$data
    }
    
    # write 
    write.table(df, file = file, 
                quote = FALSE, 
                row.names = FALSE, 
                col.names = FALSE,
                sep = sep)
  }
  
}