# Libs
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#library(shinyjs)
library(jsonlite)
#setwd("/media/hylGS/shinyLearningProjects/layoutAndModules/test4/")
setwd("/home/vboxuser/hylGS/")



source("modules/gitModule/gitModuleUi.R")
source("modules/gitModule/gitModuleServer.R")


source("modules/phenotypes/phenotypesMainTabsetPanelUi.R")
source("modules/phenotypes/phenotypesMainTabsetPanelServer.R")


#source("modules/globalVariablesModule.R")

hylGsSettingsFileName <- "hylGsSettings.json"



ui <- dashboardPage(
  dashboardHeader(title = "Dynamic sidebar"),
  dashboardSidebar(sidebarMenuOutput("menu")),
  dashboardBody(tabItems(
    tabItem(tabName = "tab_phenotypes", phenotypesMainTabsetPanel_ui("phenotypes")),
    tabItem(tabName = "tab_git", gitModule_UI("git"))

  ))
)

server <- function(input, output) {
  # Function to load settings from file
  
  
  active_modules <- reactiveVal(value = NULL)

  #globalData <- callModule(globalVariablesModule, "globals")
  

  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_phenotypes"){
      phenotypesMainTabsetPanel_server(id = "phenotypes")
      active_modules(c("phenotypes", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  

  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_git"){
      gitModule_Server(id = "git",globalData)
      active_modules(c("git", active_modules()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)


  
  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
                menuItem(
                  "phenotypes",
                  icon = icon("calendar"),
                  tabName = "tab_phenotypes"
                ),
                menuItem(
                  "versionControl",
                  icon = icon("globe"),
                  tabName = "tab_git"
                )
    )
  })
}

shinyApp(ui, server, options = list(display.mode = "showcase"))
