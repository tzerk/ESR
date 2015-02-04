## Server.R

# load required packages
library(Luminescence)
library(ESR)
library(googleVis)

# load example data
data(ExampleData.ESRspectra)


## MAIN FUNCTION
shinyServer(function(input, output, session) {
  
  # toggle example data
  data<- reactive({
    data<- ExampleData.ESRspectra[[input$exdata]]
    if(input$exdata == "Ba01") {
      updateSelectInput(session = session, inputId = "")
    }
    return(data)
  })
  
  # check and read in file (DATA SET 1)
  datGet<- reactive({
    inFile<- input$file1
    if(is.null(inFile)) return(NULL) # if no file was uploaded return NULL
    
    
    # check if zip file
    is_zip<- function(x) {
      
      sub.ext<- substring(x, first = nchar(x)-3, last = nchar(x))
      
      if(length(grep(".ZIP", sub.ext, ignore.case  = T)) == 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    
    if(is_zip(inFile$name) == TRUE) {
      
      temp.filelist<- unzip(inFile$datapath, list = TRUE)$Name
      filelist<- temp.filelist[grep(input$ext, temp.filelist, ignore.case = TRUE)]
      
      for(i in 1:length(filelist)) {
        data<- lapply(filelist,
                      function(x) { read.table(unz(inFile$datapath, x), 
                                               header = input$headers, 
                                               sep = input$sep) }
        )
      }
      
      # drop index column if present
      if(length(data[[1]]) == 3) {
        data<- lapply(data,
                      function(x) {x[1]<- NULL; x})
      }
      
      # use filenames (w/o extension) as list names 
      names(data)<- gsub(paste(".", input$ext, sep = ""), "", filelist, ignore.case = TRUE)
      
      data <- set_RLum.Data.Curve(
        data = t(as.matrix(data)),
        info = list(filelist),
        recordType = "ESR",
        curveType = "measured")
      
      
    } else {
      data<- read.table(file = inFile$datapath, # inFile[1] contains filepath 
                        sep = input$sep, 
                        quote = "", 
                        header = input$headers) # else return file
      
      
      if(length(data) == 3) {
        data[,1]<- NULL
      }
    }
    return(data)
  })
  
  output$peakrange<- renderUI({
    
    if(!is.null(datGet())) {
      data<- datGet()
    } else {
      data<- data()
    }
    
    if(is(data, "RLum.Data.Curve") == FALSE) {
      
      sliderInput(inputId = "peakrange", 
                  label = "Range", 
                  min = min(data[,1]),
                  max = max(data[,1]),
                  value = c(min(data[,1]), max(data[,1])*0.9999))
    }
  })
  
  
  # dynamically inject sliderInput for x-axis range
  output$xlim<- renderUI({
    
    if(!is.null(datGet())) {
      data<- datGet()
    } else {
      data<- data()
    }
    
    if(is(data, "RLum.Data.Curve") == TRUE) {
      data<- get_RLum.Data.Curve(data)
      data<- data[[1]]
    }
    
    xlim.plot<- range(pretty(c(min(data[,1]), max(data[,1]))))
    
    sliderInput(inputId = "xlim", 
                label = "Range",
                min = xlim.plot[1]*0.995, 
                max = xlim.plot[2]*1.005,
                value = c(xlim.plot[1], xlim.plot[2]), round=FALSE, step=0.0001)
    
  })## EndOf::renderUI()
  
  
  # dynamically inject sliderInput for y-axis range
  output$ylim<- renderUI({
    
    if(!is.null(datGet())) {
      data<- datGet()
    } else {
      data<- data()
    }
    
#     if(is(data, "RLum.Data.Curve") == TRUE) {
#       
#       data<- get_RLum.Data.Curve(data)
#       
#       # difference
#       if(input$diff == TRUE) {
#         temp<- lapply(data, function(x) {diff(x[,2]) })
#         
#         for(i in 1:length(data)) {
#           data[[i]]<- as.data.frame(
#             cbind(
#               data[[i]][1:length(data[[i]][,1])-1,1],
#               temp[[i]]
#             )
#           )
#         }
#       }
#       
#       ymin<- min(unlist((lapply(data, function(x) { min(x[2])}))))
#       ymax<- max(unlist((lapply(data, function(x) { max(x[2])}))))
#       ymax<- ymax*1.2
#       
#       if(ymin<0) { 
#         ymin<- ymin*1.2
#       } else {
#         ymin<- ymin*0.8
#       }
#       ylim.plot<- c(ymin, ymax)
#       
#     } else {
#       if(input$diff == TRUE) {
#         ylim.plot<- range(diff(data[,2]))
#       } else {
#         ylim.plot<- range(data[,2])
#       }
#       if(ylim.plot[1]<0) { 
#         ylim.plot[1]<- ylim.plot[1]*1.2
#       } else {
#         ylim.plot[1]<- ylim.plot[1]*0.8
#       }
#       
#       ylim.plot[2]<- ylim.plot[2]*1.2
#     }


  ylim.plot<- plot_Spectrum(input.data = data,
                               difference = input$diff,
                               smooth.spline = input$useSpline, 
                               smooth.spline.df = input$splinedf,
                               mooth.spline.diff.df = input$splinedf_diff,
                               integrate = input$integrate,
                               add = input$add,
                               overlay = input$overlay,
                               output.console = FALSE,
                               output.plot = FALSE)$plot.par$ylim
    
    sliderInput(inputId = "ylim", 
                label = "Range",
                min = if(ylim.plot[1]<0){ylim.plot[1]*1.5}else{ylim.plot[1]*0.5}, 
                max = ylim.plot[2]*2.5,
                value = c(ylim.plot[1], ylim.plot[2]), round=FALSE, step=0.0001)
  })## EndOf::renderUI()


output$main_plot <- renderPlot({
  
  # refresh plot on button press
  input$refresh
  
  # progress bar
  progress<- Progress$new(session, min = 0, max = 2)
  progress$set(message = "Calculation in progress",
               detail = "Retrieve data")
  on.exit(progress$close())
  
  # make sure that input panels are registered on non-active tabs.
  # by default tabs are suspended and input variables are hence
  # not available
  outputOptions(x = output, name = "xlim", suspendWhenHidden = FALSE)
  outputOptions(x = output, name = "ylim", suspendWhenHidden = FALSE)
  outputOptions(x = output, name = "peakrange", suspendWhenHidden = FALSE)
  
  
  # check if file is loaded and overwrite example data
  if(!is.null(datGet())) {
    data<- datGet()
  } else {
    data<- data()
  }
  
  # update progress bar
  progress$set(value = 1)
  progress$set(message = "Calculation in progress",
               detail = "Get values")
  
  # if custom datapoint color get RGB code from separate input panel
  if(input$col == "custom") {
    color<- input$rgb
  } else {
    color<- input$col
  }
  
  
  # if custom datapoint style get char from separate input panel
  if(input$pch == "custom") {
    pch<- input$custompch
  } else {
    pch<- as.integer(input$pch)-1 #-1 offset in pch values
  }
  
  if(input$details == TRUE) {
    info<- list(receiver=c(input$recgain, input$phase, input$harmonic, input$modfrequency, input$modamplitude),
                sigchan=c(input$conversion, input$timeconstant, input$sweeptime, input$nruns),
                field=c(input$centerfield, input$sweepwidth, input$resolution),
                microwave=c(input$frequency, input$power))
  } else {
    info<- NULL
  }
  
  
  if(input$amplitude == TRUE) {
    amplitudes<- c(input$amp_min, input$amp_max)
  } else {
    amplitudes<- NULL
  }
  
  progress$set(value = 3)
  progress$set(message = "Calculation in progress",
               detail = "Ready to plot")
  
  # validate(need()) makes sure that all data are available to
  # renderUI({}) before plotting and will wait until there
  validate(
    need(expr = input$xlim, message = 'Waiting for data... Please wait!'),
    need(expr = input$ylim, message = 'Waiting for data... Please wait!'),
    need(expr = input$peakrange, message = 'Waiting for data... Please wait!')
  )
  
  
  plot_Spectrum(input.data = data,
                   difference = input$diff,
                   smooth.spline = input$useSpline, 
                   smooth.spline.df = input$splinedf,
                   mooth.spline.diff.df = input$splinedf_diff,
                   integrate = input$integrate,
                   add = input$add,
                   overlay = input$overlay,
                   auto.shift = input$shift,
                   find.peaks = input$findpeaks,
                   peak.range = input$peakrange,
                   peak.threshold = input$peakth,
                   peak.information = input$peakinfo,
                   xlim = input$xlim,
                   ylim = input$ylim,
                   xlab = input$xlab,
                   ylab = input$ylab,
                   main = input$main,
                   type = input$type,
                   pch = pch,
                   col = color,
                   lty = as.integer(input$lty),
                   lwd = input$lwd,
                   cex = input$cex,
                   legend = input$showlegend,
                   legend.pos = input$legend.pos,
                   info = info,
                   amplitudes = amplitudes,
                   output.console = FALSE,
                   output.plot = TRUE,
                   id = TRUE)
  
  
  # nested downloadHandler() to print plot to file
  output$exportFile <- downloadHandler(
    filename = function() { paste(input$filename, ".", input$fileformat, sep="") },
    content = function(file) {
      
      # determine desired fileformat and set arguments
      if(input$fileformat == "pdf") {
        pdf(file, 
            width = input$imgwidth, 
            height = input$imgheight, 
            paper = "special",
            useDingbats = FALSE, 
            family = input$fontfamily)
      }
      if(input$fileformat == "svg") {
        svg(file, 
            width = input$imgwidth, 
            height = input$imgheight, 
            family = input$fontfamily)
      }
      if(input$fileformat == "eps") {
        postscript(file, 
                   width = input$imgwidth, 
                   height = input$imgheight, 
                   paper = "special", 
                   family = input$fontfamily)
      }
      
      plot_Spectrum(input.data = data,
                       difference = input$diff,
                       smooth.spline = input$useSpline, 
                       smooth.spline.df = input$splinedf,
                       smooth.spline.diff.df = input$splinedf_diff,
                       integrate = input$integrate,
                       overlay = input$overlay,
                       auto.shift = input$shift,
                       find.peaks = input$findpeaks,
                       peak.range = input$peakrange,
                       peak.threshold = input$peakth,
                       peak.information = input$peakinfo,
                       xlim = input$xlim,
                       ylim = input$ylim,
                       xlab = input$xlab,
                       ylab = input$ylab,
                       main = input$main,
                       type = input$type,
                       pch = pch,
                       col = color,
                       lty = as.integer(input$lty),
                       lwd = input$lwd,
                       cex = input$cex,
                       legend = input$showlegend,
                       legend.pos = input$legend.pos,
                       info = info,
                       amplitudes = amplitudes,
                       output.console = FALSE,
                       output.plot = TRUE)
      
      dev.off()
      
    },#EO content =,
    contentType = "image"
  )#EndOf::dowmloadHandler()
})##EndOf::renderPlot({})


myOptionsPlot<- reactive({
  
  options<- list(
    legend = "none",
    lineWidth = 2, 
    pointSize = 0,
    title = "", 
    selectionMode = "single",
    explorer = "{maxZoomOut: 2}",
    vAxis = "{ title: 'ESR intensity [a.u.]', format: '00.E0', baselineColor: 'red', textPosition: 'none'}",
    hAxis = "{ title: 'Magnetic field [G]', baselineColor: 'white', slantedText:'true', slantedTextAngle: 45, baselineColor: 'white', gridlines: {color: 'white'}, textPosition: 'none' }",
    aggregationTarget = 'auto',
    chartArea="{left:50,top:30, width:'70%',height:'75%'}",
    fontSize = "12",
    width=input$gvisWidth, height=input$gvisHeight
  )
  
  if(input$editor == TRUE) {
    editor<- list(gvis.editor = "Edit me!")
    options<- append(options, editor)
  }
  
  return(options)
})


# renderGvis() for an alternative googleVis plot
output$gvis_plot<- renderGvis({
  
  # refresh on button press
  input$refresh
  
  validate(
    need(expr = input$peakSwitch, message = 'Waiting for data... Please wait!')
  )
  
  if(input$peakSwitch == 1) {
    peak<- "$('input#amp_min')"
  } else {
    peak<- "$('input#amp_max')"
  }
  # some serious JS code that updates the numeric inputs 
  # for amplitude plotting 
  jscode <- paste("var sel = chart.getSelection();",
                  "var row = sel[0].row;",
                  "var text = data.getValue(row, 0);",
                  peak,".val(text);",
                  peak,".trigger('change');",
                  sep="")
                    
  
  #alert(text);
  
  js.listener<- list(gvis.listener.jscode=jscode)
  
  if(!is.null(datGet())) {
    data<- datGet()
  } else {
    data<- data()
  }
  
  if(input$diff == TRUE || input$useSpline == TRUE) {
    temp<- plot_Spectrum(input.data = data,difference = input$diff,smooth.spline = input$useSpline,
                            smooth.spline.df = input$splinedf,auto.shift = input$shift,
                            output.console = FALSE,output.plot = FALSE)
    
    if(input$useSpline == TRUE) {
      if(length(temp$data) > 1) {
        data<- data.frame(temp$splines[[1]]$x)
        for(i in 1:length(temp$data)) {
          data<- cbind(data, i = temp$splines[[i]]$y)
        }
        colnames(data)<- 0:length(temp$data)-1
        
      } else {
        data<- as.data.frame(cbind(temp$splines[[1]]$x,
                                   temp$splines[[1]]$y))  
      }

    } else { # difference == TRUE && useSpline == FALSE
      if(length(temp$data) > 1) {
        data<- data.frame(temp$data[[1]]$x)
        for(i in 1:length(temp$data)) {
          data<- cbind(data, i = temp$data[[i]]$y)
        }
        colnames(data)<- 0:length(temp$data)-1
      } else {
        data<- as.data.frame(temp$data) 
      }
    }
  }
  
  if(is(data, "RLum.Data.Curve") == TRUE) {
    temp<- get_RLum.Data.Curve(data)
    data<- NULL
    for(i in 1:length(temp)) {
      data[i]<- temp[[i]][2]
    }
    data<- cbind(temp[[1]][1], data)
    colnames(data)<- 0:(length(data)-1)-1
  }
  
  gvisPlot<- gvisScatterChart(data = data, options=append(js.listener,
                                                          myOptionsPlot()))
  return(gvisPlot)
})##EndOf::renterGvis()

# renderGvis() that prints the peak data to the first tab
output$peaks<- renderGvis({
  if(!is.null(datGet())) {
    data<- datGet()
  } else {
    data<- data()
  }
  
  if(input$findpeaks == TRUE && is(data, "RLum.Data.Curve") == FALSE) {
    peak.data<- plot_Spectrum(input.data = data,
                                 difference = input$diff,
                                 smooth.spline = input$useSpline, 
                                 smooth.spline.df = input$splinedf,
                                 overlay = input$overlay,
                                 auto.shift = input$shift,
                                 find.peaks = input$findpeaks,
                                 peak.range = input$peakrange,
                                 peak.threshold = input$peakth,
                                 peak.information = input$peakinfo,
                                 xlim = input$xlim,
                                 ylim = input$ylim,
                                 xlab = input$xlab,
                                 ylab = input$ylab,
                                 main = input$main,
                                 type = input$type,
                                 lty = as.integer(input$lty),
                                 lwd = input$lwd,
                                 output.console = FALSE,
                                 output.plot = TRUE,
                                 id = TRUE)$auto.peaks
    
    peak.data<- cbind(1:length(peak.data[,1]), peak.data)
    
    colnames(peak.data)<- c("#","Field","Intensity")
    peak.data[,2]<- round(peak.data[,2], 2)
    peak.data[,3]<- round(peak.data[,3], 0)
    
    gvisTable(data = peak.data, options=list(
      page="enable",
      alternatingRowStyle = FALSE,
      gvis.editor="Edit me!"))
  } else {
  }
})##EndOf::renterGvis()


myOptionsTable<- reactive({
  list(page="enable",
       width="300px",
       pageSize = 20,
       gvis.editor = "Edit me!")
})

# renderGvis() that prints the data to the second tab
output$dataset<- renderGvis(
  if(!is.null(datGet())) {
    data<- datGet()
    if(is(data, "RLum.Data.Curve") == TRUE) {
      #empty
    } else {
      colnames(data)<- c("Magnetic field (G)","ESR intensity (a.u.)")
      gvisTable(data = data, options=myOptionsTable())
    }
    
  } else {
    data<- data()
    if(is(data, "RLum.Data.Curve") == TRUE) {
      #empty
    } else {
      colnames(data)<- c("Magnetic field (G)","ESR intensity (a.u.)")
      gvisTable(data = data, options=myOptionsTable())
    }
  }
)##EndOf::renterGvis()

})##EndOf::shinyServer(function(input, output)