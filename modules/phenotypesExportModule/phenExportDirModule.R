phenExportDirModule_UI <- function(id) {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  fluidRow(
    column(width = 12,
           panel(
             heading = paste("Information for dataset :"),
    #verbatimTextOutput(ns("phenExportDirMetadataSummary")),
    #verbatimTextOutput("phenExportDirMetadataSummary"),
    uiOutput(ns("phenExportDirMetadataSummary"))
  )
  )
  )
  #if(interactive()){
  #  print("interactive from noDependencyModuleUi")
  #}
}

phenExportDirModule <- function(input,output,session,dirToProcess=NULL){
  
  print(paste0("dirToProcess",dirToProcess))
  
  output$phenExportDirMetadataSummary <- renderUI(
    tags$div(
      tags$ul(HTML(paste0("localPhenExport in dir: ",dirToProcess)))
    )
  )

}
