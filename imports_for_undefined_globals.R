txt <- "abline adjustcolor axis coef curve density grid hist layout legend
  lines lm median mtext na.exclude na.omit nls nls.control
packageDescription par plot points qqline qqnorm read.csv read.table
rect rgb rnorm sd segments setTxtProgressBar smooth.spline text title"
        

imports_for_undefined_globals <-
  function(txt, lst, selective = TRUE)
  {
    if(!missing(txt))
      lst <- scan(what = character(), text = txt, quiet = TRUE)
    nms <- lapply(lst, find)
    ind <- sapply(nms, length) > 0L
    imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
    if(selective) {
      sprintf("importFrom(%s)",
              vapply(Map(c, names(imp), imp),
                     function(e)
                       paste0("\"", e, "\"", collapse = ", "),
                     ""))
    } else {
      sprintf("import(\"%s\")", names(imp))
    }
  }                       