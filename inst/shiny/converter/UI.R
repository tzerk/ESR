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
               fluidRow(
                 column(width = 6, 
                        textInput("prefix", "Prefix", "")
                        ),
                 column(width = 6,
                        textInput("suffix", "Suffix", "_conv")
                        )
               ),
               textInput("todir", "Save in directory...", value = file.path(Sys.getenv("USERPROFILE"),"Desktop")),
               
               actionButton("btn", "Convert!")
               ),##EndOf::sidebarPanel
  
  # 3 - output panel
  mainPanel(width = 8,
            helpText(HTML("<h4><b>A simple file converter for Bruker spectrum files</b></h4></br>
                          File formats of the following Bruker ESR spectrometers are currently supported:</br>
                          <li>ESP300-E X-band-Spectrometer (<code>.SPC</code>&<code>.PAR</code> extension)</li>
                          <li>ELEXSYS500 X-band Spectromete (<code>.DAT</code>&<code>.DSC</code> extension)</li>")
            ),
            hr(),
            htmlOutput("js"),
            
            # insert css code inside <head></head> of the generated HTML file:
            # allow open dropdown menus to reach over the container
            tags$head(tags$style(type="text/css",".tab-content {overflow: visible;}"))
            
  )##EndOf::mainPanel
)##EndOf::shinyUI(pageWithSidebar)
