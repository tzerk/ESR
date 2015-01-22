## UI.R

# load required packages
library(Luminescence)
library(ESR)

# pageWithSidebar contains three panels:
# 1 - headerPanel: basically just for a header
# 2 - sidebarPanel: data input
# 3 - mainPanel: data output

pageWithSidebar(  
  # 1 - title = NULL -> Panel will not be shown
  headerPanel(title = NULL),
  
  
  
  sidebarPanel(width = 4,
               
               # include a tabs in the input panel for easier navigation
               tabsetPanel(id = "tabs", type = "pills", selected = "Data",
                           
                           # Tab 1: Data input
                           tabPanel("Data",
                                    
                                    # toggle example data
                                    selectInput(inputId = "exdata", label = "Example data", selected = "ba01_00", 
                                                choices = c("DPPH" = "dpph",
                                                            "Mollusc (natural)" = "Ba01_00",
                                                            "Mollusc (series)" = "Ba01")),
                                    
                                    # informational text
                                    helpText("Here you can upload your own data set.", br(),
                                             "Requirements:", br(),
                                             "a - ASCII .txt or .zip file of multiple .txt files",br(),
                                             "b - Two columns with De and De error"),
                                    
                                    tags$hr(),
                                    
                                    # file upload button (data set 1)
                                    fileInput(inputId = "file1", 
                                              label = strong("Data set"), 
                                              accept="text/plain"),
                                    
                                    # character: file extension?
                                    textInput(inputId = "ext", 
                                              label = "File extension", 
                                              value = "txt"),
                                    
                                    
                                    # logical: file contains headers?
                                    checkboxInput(inputId = "headers", 
                                                  label = "File(s) contains headers", 
                                                  value = TRUE),
                                    
                                    # char: columns separated by tab, space, comma
                                    radioButtons("sep", "Separator", selected = "\t",
                                                 c("Tab" = "\t",
                                                   "Space" = " ",
                                                   "Comma" = ",",
                                                   "Semicolon" = ";")),
                                    
                                    tags$hr(),
                                    
                                    actionButton(inputId = "refresh", label = "Refresh Plots", icon = icon("refresh"))
                                    
                           ),##EndOf::Tab_1
                           
                           # Tab 2: Statistical information
                           tabPanel("Details",
                                    
                                    checkboxInput(inputId = "details",
                                                  label = "Show experimental details",
                                                  value = TRUE),
                                    
                                    br(),
                                    
                                    fluidRow(
                                      column(width = 5,
                                             
                                             div(HTML("<strong>Receiver</strong>"), align = "center" ),
                                             # Receiver
                                             textInput(inputId = "recgain", 
                                                       label = "Receiver Gain", 
                                                       value = "1.00e+04"),
                                             
                                             textInput(inputId = "phase", 
                                                       label = "Phase", 
                                                       value = "0.0 deg"),
                                             
                                             textInput(inputId = "harmonic", 
                                                       label = "Harmonic", 
                                                       value = "1"),
                                             
                                             textInput(inputId = "modfrequency", 
                                                       label = "Modulation Frequency", 
                                                       value = "100 kHz"),
                                             
                                             textInput(inputId = "modamplitude", 
                                                       label = "Modulation Amplitude", 
                                                       value = "0.485 G")
                                      ),
                                      column(width = 5, offset = 1,
                                             
                                             div(HTML("<strong>Signal Channel</strong>"), align = "center" ),
                                             
                                             # Signal Channel
                                             textInput(inputId = "conversion", 
                                                       label = "Conversion Time", 
                                                       value = "20.48 ms"),
                                             
                                             textInput(inputId = "timeconstant", 
                                                       label = "Time Constant", 
                                                       value = "163.84 ms"),
                                             
                                             textInput(inputId = "sweeptime", 
                                                       label = "Sweep Time", 
                                                       value = "20.972 s"),
                                             
                                             textInput(inputId = "nruns", 
                                                       label = "Number of Runs", 
                                                       value = "10")
                                      )),
                                    
                                    hr(),
                                    
                                    fluidRow(
                                      
                                      column(width = 5,
                                             
                                             div(HTML("<strong>Field</strong>"), align = "center" ),
                                             
                                             # Field parameters
                                             textInput(inputId = "centerfield", 
                                                       label = "Center Field", 
                                                       value = "3345 G"),
                                             
                                             textInput(inputId = "sweepwidth", 
                                                       label = "Sweep Width", 
                                                       value = "20 G"),
                                             
                                             textInput(inputId = "resolution", 
                                                       label = "Resolution", 
                                                       value = "1024 points")
                                             
                                      ),
                                      column(width = 5, offset = 1,
                                             
                                             div(HTML("<strong>Microwave</strong>"), align = "center" ),
                                             
                                             # Microwave parameters
                                             textInput(inputId = "frequency", 
                                                       label = "Frequency", 
                                                       value = "9.7916440 GHz"),
                                             
                                             textInput(inputId = "power", 
                                                       label = "Power", 
                                                       value = "2.53 mW")
                                      ))
                                    
                           ),##EndOf::Tab_2
                           
                           # Tab 2: Statistical information
                           tabPanel("Peaks",
                                    
                                    div(align="center", h5("General")),
                                    
                                    checkboxInput(inputId = "shift",
                                                  label = "Shift peaks",
                                                  value = FALSE),
                                    
                                    br(),
                                    
                                    div(align="center", h5("Automatic peak finding")),
                                    
                                    
                                    fluidRow(
                                      column(width = 6,
                                             checkboxInput(inputId = "findpeaks", 
                                                           label = "Find Peaks", 
                                                           value = TRUE)
                                             ),
                                      column(width = 6,
                                             checkboxInput(inputId = "peakinfo", 
                                                           label = "Show peak IDs", 
                                                           value = FALSE)
                                             )
                                      ),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             uiOutput(outputId = "peakrange")
                                      ),
                                      column(width = 6,
                                             numericInput(inputId = "peakth",
                                                          label =  "Threshold value", 
                                                          value = 10)
                                      )
                                    ),
                                                   
                                    br(),
                                    
                                    div(align="center", h5("Manual peak picking")),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             checkboxInput(inputId = "amplitude",
                                                           label = "Show amplitude",
                                                           value = TRUE)
                                      ),
                                      column(width = 6,
                                             radioButtons(inputId = "peakSwitch", 
                                                          label = "Select Peak", 
                                                          inline = TRUE,
                                                          selected = 1, 
                                                          choices = c("1" = 1,
                                                                      "2" = 2))
                                      )
                                    ),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             numericInput(inputId = "amp_min", 
                                                          label = "Peak 1",
                                                          value = 3350.25,
                                                          step = 0.01)
                                             ),
                                      column(width = 6,
                                             numericInput(inputId = "amp_max",
                                                          label = "Peak 2",
                                                          value = 3345.60,
                                                          step = 0.01)
                                             ))
                            
                           ),##EndOf::Tab_2
                           
                           # Tab 1: Data input
                           tabPanel("Plots",
                                    
                                    div(align="center", h5("R Plot")),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             textInput(inputId = "main", 
                                                       label = "Title", 
                                                       value = "ESR spectrum")
                                             ),
                                      column(width = 6,
                                             sliderInput(inputId = "cex", 
                                                         label = "Scaling factor",
                                                         min = 0.5, max = 2, 
                                                         value = 1.0, step = 0.1)
                                             )
                                      ),
                                  
                                    fluidRow(
                                      column(width = 6,
                                             selectInput(inputId = "col", label = "Color", 
                                                         selected = "#428bca",
                                                         choices = list("Black" = "black",
                                                                        "Grey" = "grey50",
                                                                        "Red" = "#b22222", 
                                                                        "Green" = "#6E8B3D", 
                                                                        "Blue" = "#428bca",
                                                                        "Custom" = "custom"))
                                      ),
                                      column(width = 6,
                                             # show only if custom color is desired
                                             conditionalPanel(condition = "input.col == 'custom'",
                                                              textInput(inputId = "rgb",
                                                                        label = "Color name or RGB Code",
                                                                        value = "#428bca"))
                                      )
                                    ),
                                    
                                    radioButtons(inputId = "type", 
                                                 label = "Style",
                                                 selected = "l", 
                                                 inline = TRUE, 
                                                 choices = c("Line" = "l",
                                                             "Points" = "p")),
                                    
                                    conditionalPanel(condition = "input.type == 'l'",
                                                     
                                                     fluidRow(
                                                       column(width = 6,
                                                              
                                                              selectInput(inputId = "lty", 
                                                                          label = "Line type",
                                                                          selected = 1,
                                                                          choices = list("Blank" = 0,
                                                                                         "Solid" = 1,
                                                                                         "Dashed" = 2,
                                                                                         "Dotted" = 3,
                                                                                         "Dot dash" = 4,
                                                                                         "Long dash" = 5,
                                                                                         "Two dash" = 6))
                                                       ),
                                                       column(width = 6,
                                                              numericInput(inputId = "lwd", 
                                                                           label = "Line width", 
                                                                           value = 1,
                                                                           min = 0,
                                                                           max = 20)
                                                       )
                                                     )
                                    ),
                                    
                                    conditionalPanel(condition = "input.type == 'p'",
                                                     
                                                     fluidRow(
                                                       column(width = 6,
                                                              selectInput(inputId = "pch",
                                                                          label = "Style",
                                                                          selected = "21",
                                                                          choices = c("Square"= "1",
                                                                                      "Circle"="2",
                                                                                      "Triangle point up"="3",
                                                                                      "Plus"="4",
                                                                                      "Cross"="5",
                                                                                      "Diamond"="6",
                                                                                      "Triangle point down"="7",
                                                                                      "Square cross"="8",
                                                                                      "Star"="9",
                                                                                      "Diamond plus"="10",
                                                                                      "Circle plus"="11",
                                                                                      "Triangles up and down"="12",
                                                                                      "Square plus"="13",
                                                                                      "Circle cross"="14",
                                                                                      "Square and Triangle down"="15",
                                                                                      "filled Square"="16",
                                                                                      "filled Circle"="17",
                                                                                      "filled Triangle point up"="18",
                                                                                      "filled Diamond"="19",
                                                                                      "solid Circle"="20",
                                                                                      "Bullet (smaller Circle)"="21",
                                                                                      "Custom"="custom"))
                                                       ),
                                                       column(width = 6,
                                                              
                                                              # show only if custom symbol is desired
                                                              conditionalPanel(condition = "input.pch == 'custom'",
                                                                               textInput(inputId = "custompch", 
                                                                                         label = "Insert character", 
                                                                                         value = "?"))
                                                       )
                                                     )
                                    ),
                                    
                                    
                                    br(), 
                                    
                                    div(align="center", h5("googleVis Plot")),
                                    
                                    checkboxInput(inputId = "editor", 
                                                  label = "Enable googleVis Editor", 
                                                  value = FALSE),
                                    
                                    br(),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             sliderInput(inputId = "gvisHeight", label = "Height",
                                                         value = 400, 
                                                         min = 100, 
                                                         max = 1000, 
                                                         step = 10)
                                      ),
                                      column(width = 6,
                                             sliderInput(inputId = "gvisWidth", label = "Width",
                                                         value = 700, 
                                                         min = 100, 
                                                         max = 1000, 
                                                         step = 10)
                                      )
                                    )
                                    
                           ),##EndOf::Tab_9
                           
                           
                           # Tab 4: modify axis parameters
                           tabPanel("Axis",
                                    
                                    div(align="center", h5("X-axis")),
                                    
                                    textInput(inputId = "xlab", 
                                              label = "Label",
                                              value = "Magnetic field [G]"),
                                    
                                    
                                    # inject sliderInput from Server.R
                                    uiOutput(outputId = "xlim"),
                                    
                                    br(),
                                    
                                    div(align="center", h5("Y-axis")),
                                    
                                    textInput(inputId = "ylab", 
                                              label = "Label",
                                              value = "ESR intensity [a.u.]"),
                                    
                                    # inject sliderInput from Server.R
                                    uiOutput(outputId = "ylim")
                                    
                           ),##EndOf::Tab_4
                           
                           tabPanel("Legend",
                                    
                                    helpText("Note that a legend will only be shown when more than one spectrum is loaded."),
                                    
                                    br(),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             checkboxInput(inputId = "showlegend", 
                                                           label = "Show legend", 
                                                           value = TRUE)
                                             ),
                                      column(width = 6,
                                             selectInput(inputId = "legend.pos",
                                                         label = "Legend position",
                                                         selected = "bottomleft",
                                                         choices = c("Top" = "top",
                                                                     "Top left" = "topleft",
                                                                     "Top right"= "topright",
                                                                     "Center" = "center",
                                                                     "Bottom" = "bottom",
                                                                     "Bottom left" = "bottomleft",
                                                                     "Bottom right" = "bottomright"))
                                             )
                                      
                                      )
                                    
                                    
                                   
                           ),##EndOf::Tab_8
                           
                           # Tab 5: modify data point representation
                           tabPanel("Analyse",
                                    
                                    div(align="center", h5("General settings")),
                                    
                                    checkboxInput(inputId = "add",
                                                  label = "Always show original spectrum",
                                                  value = TRUE),
                                    
                                    div(align="center", h5("Differentiation")),
                                    
                                    checkboxInput(inputId = "diff",
                                                  label = "Calculate first derivative",
                                                  value = FALSE),
                                    
                                    div(align="center", h5("Integration")),
                                    
                                    checkboxInput(inputId = "integrate",
                                                  label = "Calculate integrand",
                                                  value = FALSE),
                                    
                                    div(align="center", h5("Smooth Spline")),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             checkboxInput(inputId = "useSpline",
                                                           label = "Smooth spline",
                                                           value = FALSE)
                                             ),
                                      column(width = 6,
                                             checkboxInput(inputId = "overlay",
                                                           label = "Overlay curves",
                                                           value = FALSE)
                                             )
                                      ),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             numericInput(inputId = "splinedf", 
                                                          label = "Degrees of freedom", 
                                                          value = 10, 
                                                          min = 0, max = Inf, 
                                                          step = 1)
                                      ),
                                      column(width = 6,
                                             numericInput(inputId = "splinedf_diff", 
                                                          label = "Degrees of freedom", 
                                                          value = 10, 
                                                          min = 0, max = Inf, 
                                                          step = 1)
                                      )
                                    )
                                    
                                    
                           ),##EndOf::Tab_5
                           
                           
                           # Tab 9: save plot as pdf, wmf or eps
                           tabPanel("Export",
                                    
                                    radioButtons(inputId = "fileformat", 
                                                 label = "Fileformat", 
                                                 selected = "pdf",
                                                 choices = c("PDF   (Portable Document Format)" = "pdf",
                                                             "SVG   (Scalable Vector Graphics)" = "svg",
                                                             "EPS   (Encapsulated Postscript)" = "eps")),
                                    
                                    textInput(inputId = "filename", 
                                              label = "Filename", 
                                              value = "ESR spectrum"),
                                    
                                    fluidRow(
                                      column(width = 6,
                                             numericInput(inputId = "imgheight",
                                                          label =  "Image height", 
                                                          value = 7)
                                      ),
                                      column(width = 6,
                                             numericInput(inputId = "imgwidth",
                                                          label = "Image width", 
                                                          value = 10)
                                      )
                                    ),
                                    
                                    selectInput(inputId = "fontfamily", 
                                                label = "Font", 
                                                selected = "Helvetica",
                                                choices = c("Helvetica" = "Helvetica",
                                                            "Helvetica Narrow" = "Helvetica Narrow",
                                                            "Times" = "Times",
                                                            "Courier" = "Courier",
                                                            "Bookman" = "Bookman",
                                                            "Palatino" = "Palatino")),
                                    
                                    tags$hr(),
                                    
                                    downloadButton(outputId = "exportFile", 
                                                   label = "Download plot")
                                    
                                    
                           ),##EndOf::Tab_8
                           
                           # Tab 10: further information
                           tabPanel("About",
                                    hr(),
                                    div(align = "center",
                                        # HTML code to include a .png file in the tab; the image file must be in
                                        # a subfolder called "wwww"
                                        img(src="RE_Logo.png", height = 100, width = 100, alt = "R.Lum"),
                                        br(),
                                        a(href = "https://zerk.canopus.uberspace.de/ESR/", "https://zerk.canopus.uberspace.de/ESR/", target="_blank"),
                                        br(),br(),hr(),
                                        p("Get the R package 'ESR' from GitHub:"),
                                        a(href = "https://github.com/tzerk/ESR",
                                          "https://github.com/tzerk/ESR", target="_blank")
                                    )#/div
                           )##EndOf::Tab_9
               )##EndOf::tabsetPanel
  ),##EndOf::sidebarPanel
  
  
  # 3 - output panel
  mainPanel(width = 8,
            
            # insert css code inside <head></head> of the generated HTML file:
            # allow open dropdown menus to reach over the container
            tags$head(tags$style(type="text/css",".tab-content {overflow: visible;}")),
            tags$head(tags$style(type="text/css"," #gvis_plot{}")),
            
            # divide output in separate tabs via tabsetPanel
            # 1 - R Plot of the ESR spectrum
            # 2 - googleVis Plot of the ESR spectrum
            # 3 - dataset
            tabsetPanel(
              
              tabPanel("Plot", 
                       fluidRow(
                         column(width = 12,
                                fluidRow(
                                  column(width = 8,
                                         plotOutput(outputId = "main_plot", height = "400px")),
                                  column(width = 2,
                                         htmlOutput(outputId = "peaks")
                                         
                                  )
                                )
                         ),
                         
                         column(width = 12,
                                htmlOutput(outputId = "gvis_plot"))
                       )),
              tabPanel("Data set", htmlOutput("dataset"))
            )###EndOf::tabsetPanel
  )##EndOf::mainPanel
)##EndOf::shinyUI(pageWithSidebar)
