library(R6)

qcWidgetsTypes <- c("chartIMG", "tableHTML")
# Simple R6 object
objR6 <- R6::R6Class(
  "objR6",
  public = list(
    identifier = NULL,
    selected_value = NULL,
    trait_name = NULL,
    file_name_extension = NULL,
    table_or_chart_command = NULL,
    output_png_file_name = NULL,
    
    initialize = function(identifier) {
      self$identifier <- identifier
    }
  )
)

# Module Ui
obj_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("value"), "Chose Value", qcWidgetsTypes),
    #selectInput("value", "Chose Value", letters),
    textAreaInput(
      inputId = ns("traitName"),
      label = "traitName",
      value = ,
      width = 100,
      height = 1,
      cols = NULL,
      rows = NULL,
      placeholder = NULL,
      resize = NULL
    ),
    textAreaInput(
      inputId = ns("fileNameExtension"),
      #inputId = "fileNameExtension",
      label = "fileNameExtension",
      value = "",
      width = 40,
      height = 10,
      cols = NULL,
      rows = NULL,
      placeholder = NULL,
      resize = NULL
    ),
    textAreaInput(
      inputId = ns("tableOrChartCommand"),
      #inputId = "tableOrChartCommand",
      label = "tableOrChartCommand",
      value = "",
      width = 100,
      height = 1,
      cols = NULL,
      rows = NULL,
      placeholder = NULL,
      resize = NULL
    ),    
    textAreaInput(
      inputId = ns("outputPngFileName"),
      #inputId = "outputPngFileName",
      label = "outputPngFileName",
      value = "",
      width = NULL,
      height = NULL,
      cols = 100,
      rows = 1,
      placeholder = NULL,
      resize = NULL
    ),  
  )
}

# Module Server
obj_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    print("obj_Server:START")
print(paste0("obj_Server:",input$first_value," ", input$first_traitName))
        
    obj <- reactiveVal(objR6$new(id))

    observeEvent(input$value, {
      print(paste0("obj_Server:input$first_value:",input$first_value))
      newObj <- obj()$clone()
      newObj$selected_value <- input$first_value
      newObj$trait_name <- input$first_traitName
      newObj$file_name_extension <- input$first_fileNameExtension
      newObj$table_or_chart_command <- input$first_tableOrChartCommand
      newObj$output_png_filename <- input$first_outputPngFileName
      obj(newObj)
    })
    
    print("obj_Server:END")
    return(reactive(obj()))
    
  })
}


# Shiny App
phenQcLayoutBuilderModule_UI<- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width = 1,
             selectInput(ns("first_objSelection"), "Select Object",
                choices = "",
                selectize = FALSE,
                size = 10)
      ),
      column(width = 7,
             textAreaInput(
               inputId = ns("first_tableOrChartCommand"),
               #inputId = "tableOrChartCommand",
               label = "tableOrChartCommand",
               value = "",
               width = NULL,
               height = NULL,
               cols = 40,
               rows = 10,
               placeholder = NULL,
               resize = NULL
             ),   
      ),
      column(width=3,
             selectInput(ns("first_value"), "Chose Value", qcWidgetsTypes),
             textAreaInput(
               inputId = ns("first_traitName"),
               label = "traitName",
               value = "",
               width = NULL,
               height = NULL,
               cols = 100,
               rows = 1,
               placeholder = NULL,
               resize = NULL
             ),
             textAreaInput(
               inputId = ns("first_fileNameExtension"),
               #inputId = "fileNameExtension",
               label = "fileNameExtension",
               value = "",
               width = NULL,
               height = NULL,
               cols = 100,
               rows = 1,
               placeholder = NULL,
               resize = NULL
             ),

             textAreaInput(
               inputId = ns("first_outputPngFileName"),
               label = "outputPngFileName",
               value = "",
               width = NULL,
               height = NULL,
               cols = 100,
               rows = 1,
               placeholder = NULL,
               resize = NULL
             ) 
        
      )
    ),
    fluidRow(
      actionButton(ns("addObject"), "Add Object"),
      actionButton(ns("rmvObject"), "Remove Object"),
      tabsetPanel(id = ns("objTP")),
      verbatimTextOutput(ns("displayValues"))
    )
  )

}

phenQcLayoutBuilderModule_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    print("start: phenQcLayoutBuilderModule_Server")
    
    
  objCount <- reactiveVal(0)
  
  
  #objCollection<-NULL

  objCollection <- reactiveValues(objects = list())

  # Reaction on action button "addObject"
  observeEvent(input$addObject, {
    print("addObject")
    # Add another item
    objCount(objCount() + 1)
    newObjName <- paste0("Object_", objCount())
    updateSelectInput(session, "objSelection", choices = c(paste0("Object_", 1:objCount())))
    

    
    # Add the server component of the module
    observeEvent(obj_Server(newObjName), {
      objCollection$objects[[newObjName]] <- obj_Server(newObjName)
    })
    
    # Append the object tabset panel
    appendTab("objTP", tabPanel(newObjName, obj_UI(newObjName )), select = TRUE)
    
    tmp1<-objCollection$objects[[newObjName]]
    print(paste0("aloha:",tmp1$obj$output_png_filename))
    print("---------------------------")
    print(tmp1)
    print("-----------------------------")
  })
  
  # Reaction on action button "rmvObject"
  observeEvent(input$rmvObject, {
    delObjName <- paste0("Object_", objCount())
    if (objCount() > 0) {
      objCount(objCount() - 1)
      removeTab("objTP", target = delObjName)
      objCollection$objects[[delObjName]] <- NULL
      if (objCount() > 0) {
        updateSelectInput(session, "objSelection", choices = c(paste0("Object_", 1:objCount())))
      } else {
        updateSelectInput(session, "objSelection", choices = "")
      }
    }
  })
  
  # Ouput the selected values
  output$displayValues <- renderPrint({
    lapply(reactiveValuesToList(objCollection)$objects, function(i) {i()})
  })
  
  
  print("phenQcLayoutBuilderModule_Server END")
  })
  
}
# observeEvent(input$saveData,{
#   x <- vec()
#   save(x, file = 'vec.RData')
# })
