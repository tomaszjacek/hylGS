# Libs
library(shiny)
library(shinydashboard)
library(jsonlite)
#setwd("/media/hylGS/shinyLearningProjects/layoutAndModules/test4/")
setwd("/home/vboxuser/hylGS/")

# Source module
source("modules/noDependencyModule/noDependencyModuleServer.R")
source("modules/noDependencyModule/noDependencyModuleUi.R")

source("modules/withDependencyModule/withDependencyModuleServer.R")
source("modules/withDependencyModule/withDependencyModuleUi.R")

source("modules/gitModule/gitModuleServer.R")
source("modules/gitModule/gitModuleUi.R")

source("modules/systemConfigurationModule/systemConfigurationModuleServer.R")
source("modules/systemConfigurationModule/systemConfigurationModuleUi.R")

source("modules/genotypesExportModule/genotypesExportModuleServer.R")
source("modules/genotypesExportModule/genotypesExportModuleUi.R")

source("modules/genotypesUtilsModule/genotypesUtilsModuleServer.R")
source("modules/genotypesUtilsModule/genotypesUtilsModuleUi.R")

source("modules/phenotypesExportModule/phenotypesExportModuleServer.R")
source("modules/phenotypesExportModule/phenotypesExportModuleUi.R")

source("modules/globalVariablesModule.R")

hylGsSettingsFileName <- "hylGsSettings.json"

#funkcja do usowania zmiennych dynamicznei ladowanych modulow. chyba...
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
    tabItem(tabName = "tab_phenotypesExport", phenotypesExportModule_UI("phenotypesExport")),
    tabItem(tabName = "tab_genotypesUtils", genotypesUtilsModule_UI("genotypesUtils")),
    tabItem(tabName = "tab_genotypesExport", genotypesExportModule_UI("genotypesExport")),
    tabItem(tabName = "tab_git", gitModule_UI("git")),
    tabItem(tabName = "tab_systemConfiguration", systemConfigurationModule_UI("systemConfiguration"))
  ))
)

server <- function(input, output) {
  # Function to load settings from file

  
  active_modules <- reactiveVal(value = NULL)
  #globalData <- reactiveValues( hylGsSettingsFileName = "hylGsSettings.json", hylGsSettings=NULL)
  globalData <- callModule(globalVariablesModule, "globals")
  
   # reactive({
   #   print(globalData)
   # })
   # 
  # 
  # reactive({
  #   globalData$hylGsSettings <- loadSystemSettings(globalData$hylGsSettingsFileName)
  #   #print(globalData$hylGsSettings)
  # })

  #print(globalData$hylGsSettingsFileName)
  #print(globalData)
  
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_phenotypesExport"){
      phenotypesExportModule_Server(id = "phenotypesExport")
      active_modules(c("phenotypesExport", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_genotypesUtils"){
      genotypesUtilsModule_Server(id = "genotypesUtils")
      active_modules(c("genotypesUtils", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_genotypesExport"){
      genotypesExportModule_Server(id = "genotypesExport")
      active_modules(c("genotypesExport", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_git"){
      gitModule_Server(id = "git",globalData)
      active_modules(c("git", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_systemConfiguration"){
      systemConfigurationModule_Server(id = "systemConfiguration",dat=globalData)
      active_modules(c("systemConfiguration", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
                menuItem(
                  "phenotypesExport",
                  icon = icon("calendar"),
                  tabName = "tab_phenotypesExport"
                ),
                menuItem(
                  "genotypesUtils",
                  icon = icon("calendar"),
                  tabName = "tab_genotypesUtils"
                ),
                menuItem(
                  "genotypesExport",
                  icon = icon("calendar"),
                  tabName = "tab_genotypesExport"
                ),
                menuItem(
                  "systemConfiguration",
                  icon = icon("globe"),
                  tabName = "tab_systemConfiguration"
                ),
                menuItem(
                  "versionControl",
                  icon = icon("globe"),
                  tabName = "tab_git"
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
