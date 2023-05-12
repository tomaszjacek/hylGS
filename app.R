# Libs
library(shiny)
library(shinydashboard)
#setwd("/media/hylGS/shinyLearningProjects/layoutAndModules/test4/")
setwd("/home/vboxuser/hylGS/")

# Source module
source("modules/noDependencyModule/noDependencyModuleServer.R")
source("modules/noDependencyModule/noDependencyModuleUi.R")

source("modules/withDependencyModule/withDependencyModuleServer.R")
source("modules/withDependencyModule/withDependencyModuleUi.R")

source("modules/gitModule/gitModuleServer.R")
source("modules/gitModule/gitModuleUi.R")

remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

ui <- dashboardPage(
  dashboardHeader(title = "Dynamic sidebar"),
  #tags$style(type='text/css', '#txt_out {white-space: pre-wrap;}'),
  dashboardSidebar(sidebarMenuOutput("menu")),
  dashboardBody(tabItems(
    tabItem(tabName = "tab_1", withDependencyModule_UI("module1")),
    tabItem(tabName = "tab_2", noDependencyModule_UI("module2")),
    tabItem(tabName = "tab_3", gitModule_UI("module3"))
  ))
)

server <- function(input, output) {
  active_modules <- reactiveVal(value = NULL)
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_1"){
      #callModule(sampleModuleServer, "module1")
      noDependencyModule_Server(id = "module1")
      active_modules(c("module1", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_2"){
      #callModule(sampleModuleServer, "sampleModule")
      withDependencyModule_Server(id = "module2")
      active_modules(c("module2", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_3"){
      #callModule(sampleModuleServer, "sampleModule")
      withDependencyModule_Server(id = "module3")
      active_modules(c("module3", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  
  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
                menuItem(
                  "genotypesUtils",
                  icon = icon("calendar"),
                  tabName = "tab_1"
                ),
                menuItem(
                  "systemConfiguration",
                  icon = icon("globe"),
                  tabName = "tab_2"
                ),
                menuItem(
                  "versionControl",
                  icon = icon("globe"),
                  tabName = "tab_3"
                )
    )
  })
}

shinyApp(ui, server)



#####################!!!!###############################

# ui <- fluidPage(
#   titlePanel("Download reprex"),
#   
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       shinyWidgets::downloadBttn(
#         outputId = "download",
#         label = "Download data as .RData",
#         style = "bordered",
#         color = "default")
#       ,
#       shiny::downloadButton(
#         outputId = "download",
#         label = "Download data as .RData")
#     )
#     
#   )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   data <- data.frame(x1 = runif(100), r2 = runif(100))
#   output$download <- shiny::downloadHandler(
#     filename = function() {
#       paste("file_to_dl.RData")
#     },
#     content = function(file) {
#       save(data, file = file)
#     }
#   )
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
