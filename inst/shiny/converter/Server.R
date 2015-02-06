## Server.R
## MAIN FUNCTION
shinyServer(function(input, output, session) {
  
  rval <- reactiveValues(conv_files = NULL)
  
  datGet <- reactive({
    inFile <- input$files
    if (is.null(inFile)) return(NULL)
    return(inFile)
  })

  observeEvent(input$btn, {
    file <- datGet()
    if (is.null(file)) return(NULL)
    
    new_paths <- vector(mode = "character", length = length(file$name))
    
    for (i in seq_along(new_paths)) {
      new_paths[i] <- paste0(gsub("/[^/]*$", "", file$datapath[i]), "/", file$name[i])
      file.rename(file$datapath[i], new_paths[i])
    }
    # strip .PAR and .DSC files
    rm1 <- grep("(.par)|(.dsc)", new_paths, ignore.case = TRUE)
    if (length(rm1) != 0) new_paths <- new_paths[-rm1]
    rm2 <- grep("(.par)|(.dsc)", file$name, ignore.case = TRUE)
    if(length(rm2) != 0) new_names <- file$name[-rm2]
    else new_names <- file$name
    
    res <- lapply(new_paths, read_Spectrum)
    
    for (i in seq_along(res)) {
      data <- as.data.frame(res[[i]]$data)
      dest <- paste0(input$todir, input$prefix, gsub("....$", "", new_names[i]), input$suffix, ".txt")
      write.table(data, dest, row.names = FALSE, quote = FALSE)
    }
    rval$conv_files <- new_names
  })
  
 output$js <- renderText({
     if (!is.null(rval$conv_files))
       paste(rval$conv_files, collapse = "</br>")
 })

})##EndOf::shinyServer(function(input, output)