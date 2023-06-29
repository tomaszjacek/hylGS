dirListSelectionModule_UI <- function(id) {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  fluidRow(
    column(width = 12,
           panel(
             heading = paste("Information for dataset :"),
             #selectInput(ns("SI_dataset"), label = "Dataset", choices = datasets(), selected = "iris"),
             #selectInput(ns("SI_var"), label = "Choose variable", choices = NULL),
             shinyjs::disabled(actionButton(ns("AB_load"), class = "btn-success btn-xs",label = "load"))
           )
    )
  )
}

dirListSelectionModule <- function(input, output, session, dir=NULL) {
  
  ns <- session$ns
  
  # Define the ReactiveValue to return : "toReturn"
  # with slots "variable", "variable_name" & "trigger"
  toReturn    <-  reactiveValues(
    dirSelected = NULL
  )
  

  

  # (Re)load button
  observeEvent(input$AB_load, {
    toReturn$dirSelected       <- dir
    #print(paste0("dir:",toReturn$dirSelected))
  })
  
  return(toReturn)
}
