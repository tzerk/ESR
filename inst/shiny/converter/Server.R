## Server.R



## MAIN FUNCTION
shinyServer(function(input, output, session) {
  
  datGet <- reactive({
    inFile <- input$files
    
    if (is.null(inFile))
      return(NULL)
    
    return(inFile)
  })

  
 output$js <- renderText({
    input$btn
    
    isolate({
    file <- datGet()
    
    new_paths <- vector(mode = "character", length = length(file$name))
    
    print(file$datapath)
    
    for (i in seq_along(new_paths)) {
      new_paths[i] <- paste0(gsub("/[^/]*$", "", file$datapath[i]), "/", file$name[i])
      file.rename(file$datapath[i], new_paths[i])
    }
    print(new_paths)
    # strip .PAR and .DSC files
    new_paths <- new_paths[-grep("(.par$)|(.dsc$)", new_paths, ignore.case = TRUE)]
    new_names <- file$name[-grep("(.par$)|(.dsc$)", file$name, ignore.case = TRUE)]
    
    res <- lapply(new_paths, read_ESRspectrum)
    
    for (i in seq_along(res)) {
      data <- as.data.frame(res[[i]]$data)
      dest <- paste0(input$todir,"/conv_", gsub("...$", "", new_names[i]), "txt")
      
      write.table(data, dest, row.names = FALSE, quote = FALSE)
    }
    
    })
 })

})##EndOf::shinyServer(function(input, output)