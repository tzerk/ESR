## UI.R

# pageWithSidebar contains three panels:
# 1 - headerPanel: basically just for a header
# 2 - sidebarPanel: data input
# 3 - mainPanel: data output

pageWithSidebar(  
  # 1 - title = NULL -> Panel will not be shown
  headerPanel(title = "File converter"),
  sidebarPanel(width = 4,
               
               fileInput('files', 'Choose spectrum files', multiple = TRUE,
                         accept=c(".txt", ".dta", ".dsc", ".spc", ".par", ".asc")),
               
               textInput("todir", "Save in directory...", value = file.path(Sys.getenv("USERPROFILE"),"Desktop")),
               
               actionButton("btn", "Convert!")
               ),##EndOf::sidebarPanel
  
  # 3 - output panel
  mainPanel(width = 8,
            
            textOutput("js"),
            
            # insert css code inside <head></head> of the generated HTML file:
            # allow open dropdown menus to reach over the container
            tags$head(tags$style(type="text/css",".tab-content {overflow: visible;}"))
            
  )##EndOf::mainPanel
)##EndOf::shinyUI(pageWithSidebar)
