## UI.R

# pageWithSidebar contains three panels:
# 1 - headerPanel: basically just for a header
# 2 - sidebarPanel: data input
# 3 - mainPanel: data output

pageWithSidebar(  
  # 1 - title = NULL -> Panel will not be shown
  headerPanel(title = "ESR file converter"),
  sidebarPanel(width = 4,
               
               fileInput('files', 'Choose spectrum files', multiple = TRUE,
                         accept=c(".txt", ".dta", ".dsc", ".spc", ".par", ".asc")),
               selectInput("device", "Device", 
                           choices = list(
                             "Automatic" = "auto",
                             "Bruker ESP300-E" = "ESP300-E",
                             "Bruker ELEXSYS500" = "ELEXSYS500",
                             "Bruker EMXplus (old)" = "EMXplus_old"
                           ), selected = "auto"),
               fluidRow(
                 column(width = 6, 
                        textInput("prefix", "Prefix", "")
                        ),
                 column(width = 6,
                        textInput("suffix", "Suffix", "_conv")
                        )
               ),
               textInput("todir", "Save in directory...", value = file.path(Sys.getenv("USERPROFILE"),"Desktop")),
               
               actionButton("btn_preview", "Preview"),
               actionButton("btn", "Convert!"),
               hr(),
               div(align = "center",
                   img(src="RE_Logo.png", height = 100, width = 100, alt = "R.Lum"),
                   br(),
                   a(href = "http://www.r-luminescence.de", "http://www.r-luminescence.de", target="_blank"),
                   br(),
                   a(href = "https://forum.r-luminescence.de", "https://forum.r-luminescence.de", target="_blank"),
                   br(),br(),hr(),
                   HTML("<img src='GitHub-Mark-32px.png' width='32px' height='32px'></img>"),
                   p("See the R code of this app on GitHub:"),
                   a(href = "https://github.com/tzerk/ESR/", "https://github.com/tzerk/ESR/", target="_blank")
               )#/div
               ),##EndOf::sidebarPanel
  
  # 3 - output panel
  mainPanel(width = 8,
            helpText(HTML("<h4><b>A simple file converter for Bruker spectrum files</b></h4></br>
                          File formats of the following Bruker ESR spectrometers are currently supported:</br>
                          <li>ESP300-E X-band-Spectrometer (<code>.SPC</code> and <code>.PAR</code> extensions)</li>
                          <li>EMXplus Spectrometer (<code>.SPC</code> and <code>.PAR</code> extensions)</li>
                          <li>ELEXSYS500 X-band Spectrometer (<code>.DAT</code> and <code>.DSC</code> extensions)</li>")
            ),
            hr(),
            htmlOutput("text"),
            plotOutput("preview"),
            
            # insert css code inside <head></head> of the generated HTML file:
            # allow open dropdown menus to reach over the container
            tags$head(tags$style(type="text/css",".tab-content {overflow: visible;}"))
            
  )##EndOf::mainPanel
)##EndOf::shinyUI(pageWithSidebar)
