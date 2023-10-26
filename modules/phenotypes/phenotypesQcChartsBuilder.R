library(R6)
library ("purrr")
source("/media/hylGS/shinyTests/hylGS_vWithModulesAsObjectsR6/modules/phenotypes/phenotypesModulesUtils/qcVisualizationBuilderUtils.R")
#library(shinyjs)

#js <- "shinyjs.refresh = function() { location.reload(); }"
#js <- 


qcWidgetsTypes <- c("chartIMG", "tableHTML")

traitObject <- R6::R6Class(
  classname = "traitObject",
  public = list(
    chartsObjectsList = list(),
    htmlSkin = "",
    traitName = ""
  )
)
chartsObjectsManager <- R6::R6Class(
  classname = "chartsObjectsManager",
  
  public = list(
    traitObjects = list(),
    #htmlSkins = "",
    #triggers = reactiveValues(plot = 0),
    #qcObjectsCount = 1,
    # initialize = function() {
    #   qcObjects = list()
    #   triggers = reactiveValues(plot = 0)
    #   qcObjectsCount = 1
    #   htmlSkins = ""
    # },
    #trigger_plot = function() {
    #  self$triggers$plot <- self$triggers$plot + 1
    #},
    #aloha = reactiveValues(varY = NULL,varX=NULL),
    
    add_qcObject = function(traitName,qcObject) {
      print(paste0("add_qcObject ",traitName))
      if(is.null(qcObject$idName) | qcObject$idName == ""){
        qcObject$idName <- paste0("qcObj_", self$qcObjectsCount)
      }
      qcObjectId <- qcObject$idName
      
      self$qcObjects[[traitName]][[qcObjectId]] <- qcObject
      #self$servers[[traitName]][[qcObjectId]] <- qcObjectServer
      self$qcObjectsCount <- self$qcObjectsCount + 1
    },
    add_trait = function(traitName,htmlSkin) {
      self$qcObjects[[traitName]] <- list()
      self$htmlSkins[[traitName]] <- htmlSkin
    },
    rm_qcObject = function(traitName,qcObjectId) {
      self$qcObjects[[traitName]][[qcObjectId]] <- NULL
    },
    rm_trait = function(traitName) {
      print(paste0("rm_trait ",traitName))
      #print("rm_trait ",names(self$qcObjects))
      self$qcObjects[[traitName]] <- NULL
      self$htmlSkins[[traitName]] <- ""
      print("rm_trait end")
    },
    update_trait_htmlSkin = function(traitName, htmlSkin){
      self$htmlSkins[[traitName]] <- htmlSkin
    },
    get_traits_names = function() {
      
      if(!is_empty(names(self$qcObjects))){
        return(names(self$qcObjects))
      }else{
        return(NULL)

      }
    },
    get_trait_qcObjectsNames= function(traitName) {
      if(length(self$qcObjects[[traitName]])){
        return(names(self$qcObjects[[traitName]]))
      }else{
        return(NULL)
      }
    }
  )
)
# Simple R6 object
qcFormVariablesObjR6 <- R6::R6Class(
  classname = "qcFormVariablesObjR6",
  public = list(
    triggers = reactiveValues(plot = 0),
    trigger_plot = function() {
      self$triggers$plot <- self$triggers$plot + 1
    },
    idName = NULL,
    selected_value = NULL,
    trait_name = NULL,
    file_name_extension = NULL,
    table_or_chart_command = NULL,
    output_png_file_name = NULL,
    
    set_variables = function(ident,sv,tn,fne,tocc,opfn) {
      self$idName <- ident
      self$selected_value <- sv
      self$trait_name <- tn
      self$file_name_extension <-fne
      self$table_or_chart_command <- tocc
      self$output_png_file_name <- opfn
    }
  )
)

qcFormVariablesEdit_ui <- function(id,qcoName) {
  ns <- NS(id)
  #qcForm imported from phenotypesExportModuleUtils.R
  
  fluidPage(
    #useShinyjs(),
    #extendShinyjs(text = jscode, functions = "refresh"),
    fluidRow(
      column(width = 8,
             selectInput(ns("value"), "Chose Value", qcWidgetsTypes),
             textAreaInput(
               inputId = ns("qcObjName"),
               label = "qcObjName",
               value = qcoName,
               width = NULL,
               height = NULL,
               cols = 80,
               rows = 1,
               placeholder = NULL,
               resize = NULL
             )
      )
    )

  )
}

# Module Server
qcFormVariablesEdit_server <- function(id, traitName, varManager) {
  moduleServer(id, function(input, output, session) {
    print("qcFormVariablesEdit_server:START")
    
    # print(varManager$get_traits_names())
    # observeEvent(input$addQcObject, {
    #   print("addQcObject")
    #   # Add another item
    #   newObj <- qcFormVariablesObjR6$new()
    #   #newObjName <- paste0("Object_", objCount())
    #   #newObj$set_variables(input$ident,input$sv,input$tn,input$fne,input$tocc,input$opfn)
    #   varManager$add_qcObject(input$objSelection,newObj,qcFormVariablesEdit_server(newObj))
    # })
    
    print("qcFormVariablesEdit_server:END")
  })
}





traitTab_ui <- function(id) {
  ns <- NS(id)
  #qcForm imported from phenotypesExportModuleUtils.R

  fluidPage(
    tabsetPanel(id = ns("qcFormsMainPanel")),
    fluidRow(
      hr(style = "border-top: 1px solid #000000;")
    ),
    fluidRow(
       column(width = 12,
                     textAreaInput(
                       inputId = ns("qcObjName"),
                       label = "qcObjName",
                       value = ,
                       width = NULL,
                       height = NULL,
                       cols = 80,
                       rows = 1,
                       placeholder = NULL,
                       resize = NULL
                     )
              ,
              actionButton(ns("addQcObject"), "Add QcObject"),
              actionButton(ns("rmQcObject"), "Remove QcObject")
       ))
    )
}


#newObj <- qcFormVariablesObjR6$new()
#newObjName <- paste0("Object_", objCount())
#
#variablesManager$add_qcObject(input$traitName,newObj,)
# Module Server
traitTab_server <- function(id, varManager) {
  moduleServer(id, function(input, output, session) {
    traitName<-id
    print("traitTab_server:START")
    ns <- session$ns
    qcObjectsServers <- list()
    traitQcObjectsList <- varManager$get_trait_qcObjectsNames(id)
    print(paste0("varManager$get_trait_qcObjectsNames() :",traitQcObjectsList))
    
    
    
    
    if(length(traitQcObjectsList) >0){
      for(o in 1:length(traitQcObjectsList)){
        name <- traitQcObjectsList[o]
      
        qcObjectsServers[[name]] <- qcFormVariablesEdit_server(name,traitName,varManager)
        appendTab("qcFormsMainPanel", tabPanel(name, qcFormVariablesEdit_ui(ns(name),name)), select = TRUE)
      }
    }
    
    print(paste0("varManager$get_traits_names() :",varManager$get_traits_names()))
    observeEvent(input$addQcObject, {
      print("addQcObject")
      # Add another item

      newObj <- qcFormVariablesObjR6$new()
      #print("0.1")
      if(input$qcObjName == "" | is.null(input$qcObjName)){
        newObj$set_variables("","","","","","")
      }else{
        newObj$set_variables(input$qcObjName,"","","","","")
      }
      varManager$add_qcObject(traitName,newObj)

      newName <- newObj$idName

      qcObjectsServers[[newName]] <- qcFormVariablesEdit_server(newName,traitName,varManager)

      appendTab("qcFormsMainPanel", tabPanel(newName, qcFormVariablesEdit_ui(ns(newName),newName)), select = TRUE)

    })
    
    print("traitTab_server:END")
  })
}


scatter_tabset <- function(traitsList, traitSelected,nsId ){
  print(paste0("scatter_tabset_start: ",traitsList))

  tp<-NULL
  if(length(traitsList)>0){
    tp <- 
      do.call(tabsetPanel, c(id=nsId('objTP'), lapply(1:length(traitsList), function(i) {
        tabPanel(traitsList[i], traitTab_ui(nsId(traitsList[i])))
      }),selected = traitSelected))
  }
  print(paste0("scatter_tabset_end: ",traitsList))
  return(tp)
}

scatter_traitSelection <- function(traitsList, traitSelected, nsId ){
  
               selectInput(nsId("objSelection"), "Select Object",
                                        choices = traitsList,
                                        selectize = FALSE,
                           selected = traitSelected,
                                        size = 10)
    

}

# Shiny App

phenotypesQcChartsBuilder_ui<- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      verbatimTextOutput(ns("saveConfigString"))
    ),
    fluidRow(
        actionButton(ns("save"), "Save Configuration"),    
        
      ),
    fluidRow(
    hr(style = "border-top: 1px solid #000000;")
    ),
    fluidRow(
      column(width = 1,
             uiOutput(ns("mainSelection"))
      ),

      column(width = 7,
             #selectInput(ns("value"), "Chose Value", qcWidgetsTypes),
             # textAreaInput(
             #   inputId = ns("traitName"),
             #   label = "traitName",
             #   value = ,
             #   width = "100%",
             #   height = "1%",
             #   cols = NULL,
             #   rows = NULL,
             #   placeholder = NULL,
             #   resize = NULL
             # ),
             textAreaInput(ns("traitName"), "traitName", value = "", rows = 1, resize = "both",width= "50%") ,
             fluidRow(
               hr(style = "border-top: 1px solid #000000;")
             ),
             # textAreaInput(
             #   inputId = ns("traitHtmlSkin"),
             #   label = "traitHtmlSkin",
             #   value = ,
             #   width = "100%",
             #   height = "80%",
             #   cols = NULL,
             #   rows = NULL,
             #   placeholder = NULL,
             #   resize = NULL
             # )
             textAreaInput(ns("traitHtmlSkin"), "traitHtmlSkin", value = "", rows = 16, resize = "both",width= "100%"),
      )
    ),
    fluidRow(
      actionButton(ns("addTrait"), "Add Trait"),
      actionButton(ns("rmTrait"), "Remove Trait"),
      uiOutput(ns("mainTabsetpanel")),
      verbatimTextOutput(ns("displayValues"))
    )
  )
  
}

phenotypesQcChartsBuilder_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    print("start:phenotypesQcBuilder_server")
    print(getwd())
    
    output$saveConfigString <- renderText(
      "configuration saved"
    )
    
    loadConfiguration <- reactiveVal(0)
    activeTraitTab<-NULL
    traitsServers <- list()
    objCount <- reactiveVal(0)
    #traitFormServersCollection<- reactiveValues(objects = list())
    variablesManager <- NULL
    
    if(file.exists("qcBuilderConfig.rda")){
      print("loading qcBuilderConfig.rda")
      load("qcBuilderConfig.rda")
      #loadConfiguration<-1
      traitsList <- variablesManager$get_traits_names()
      print(paste0("loaded traits1 :",traitsList) )
      if(length(traitsList)>0){
        print(paste0("loaded traits2 :",traitsList) )
        
        output$mainSelection <- renderUI({
            scatter_traitSelection(variablesManager$get_traits_names(),variablesManager$get_traits_names()[1], ns)
        })
        
        output$mainTabsetpanel <- renderUI({
          print(paste0("renderUI_1: ",variablesManager$triggers$plot))
          scatter_tabset(variablesManager$get_traits_names(),variablesManager$get_traits_names()[1], ns)
          #tabsetPanel(type = "objTP",

        #for(t in 1:length(traitsList)){        print(paste0("loaded traits3 :",traitsList[t]) )
          #appendTab("objTP", tabPanel(traitsList[t], traitTab_ui(ns(traitsList[t]))), select = TRUE)
          #tabPanel(traitsList[t], traitTab_ui(ns(traitsList[t])))
          #traitsServers[[traitsList[t]]] <- traitTab_server(traitsList[t],variablesManager)
          #updateTabsetPanel(inputId = "objTP", selected = traitsList[t])
          
          #updateSelectInput(session, "objSelection", choices = traitsList,selected =traitsList[t])
        #}
        #)
        })
        #js$refresh();
        #})
         for(t in 1:length(traitsList)){        print(paste0("loaded traits :",traitsList[t]) )
           traitsServers[[traitsList[t]]] <- traitTab_server(traitsList[t],variablesManager)
           updateSelectInput(session, "objSelection", choices = traitsList,selected =traitsList[t])
         }
        #refresh()
        
        #updateSelectInput(session, "objSelection", choices = traitsList,selected =traitsList[1])
      }
      
    }else{
      print("cant find qcBuilderConfig.rda")
      variablesManager <- chartsObjectsManager$new()
    }
    
    
    
    # observeEvent(print(isolate(loadConfiguration)),{
    # 
    # })
    # 
    
    print(paste0("!!!!!!!!!!!!",names(traitsServers)))
    
    
    observeEvent(input$save,{
      print("save config input$saveConfigString")
      save(variablesManager,file = "qcBuilderConfig.rda")
    })

    
    observeEvent(input$addTrait, {
      
      newTraitName <- input$traitName
      newTraitHtmlSkin <- input$traitHtmlSkin
      print(paste0("addTrait0 ",newTraitName," ",newTraitHtmlSkin))
      traitsList <- variablesManager$get_traits_names()
      
      failedCondition <- 0
      print(paste0("addTrait1 ",typeof(traitsList)))
      if(!is_empty(traitsList) & newTraitName %in% traitsList){
        failedCondition <- 1
      }
      
      print(paste0("addTrait2 ",newTraitName," ",newTraitHtmlSkin))
      if(newTraitName == "" | is_empty(newTraitName)){
        failedCondition <- 1
      }
      print(paste0("addTrait3 ",newTraitName," ",newTraitHtmlSkin))
      if(!failedCondition){
        print(paste0("addTrait ",newTraitName))
        variablesManager$add_trait(newTraitName,newTraitHtmlSkin)
      
        #updateSelectInput(session, "objSelection", choices = traitsList,selected =newTraitName)
        #appendTab("objTP", tabPanel(newTraitName, traitTab_ui(ns(newTraitName))), select = TRUE)
        output$mainTabsetpanel <- renderUI({
          print(paste0("renderUI_1: ",variablesManager$triggers$plot))
          scatter_tabset(variablesManager$get_traits_names(),newTraitName, ns)
        })
        output$mainSelection <- renderUI({
          scatter_traitSelection(variablesManager$get_traits_names(),newTraitName, ns)
        })
        traitsServers[[newTraitName]] <- traitTab_server(newTraitName,variablesManager)
      }
    })


    
    observeEvent(input$rmTrait, {
      print(paste0("rmvTrait ",input$objSelection))


      objName<-input$objSelection

      variablesManager$rm_trait(input$objSelection)
      tList <- variablesManager$get_traits_names()
      print(paste0("variablesManager$get_traits_names() :",length(tList)))
      if(length(tList)>0){
        updateSelectInput(session, "objSelection", choices = variablesManager$get_traits_names(),selected = tList[1])
      }else{
        updateSelectInput(session, "objSelection", choices = character(0))
      }
      #removeTab(inputId = "objTP", target = objName)
      output$mainTabsetpanel <- renderUI({
        print(paste0("renderUI_1: ",variablesManager$triggers$plot))
        scatter_tabset(variablesManager$get_traits_names(),variablesManager$get_traits_names()[1], ns)
      })
      output$mainSelection <- renderUI({
        scatter_traitSelection(variablesManager$get_traits_names(),variablesManager$get_traits_names()[1], ns)
      })
      traitsServers[[input$objSelection]] <- NULL

    })
    

     observeEvent(input$objTP, {
       #updateSelectInput(session, "objSelection", choices = variablesManager$get_traits_names(),selected = input$objTP)
       print(paste0("input$objTP", input$objTP))
       output$mainSelection <- renderUI({
         scatter_traitSelection(variablesManager$get_traits_names(),input$objTP, ns)
       })
       
     })
    
    observeEvent(input$objSelection, {
      objName<-input$objSelection
      print(paste0("obj_Server:input$objSelection:",objName," "))
      #updateTabsetPanel(session, "objTP", selected=input$objSelection)
      output$mainTabsetpanel <- renderUI({
        scatter_tabset(variablesManager$get_traits_names(),input$objSelection, ns)
      })
      
    })

  })
  
}

# observeEvent(input$saveData,{
#   x <- vec()
#   save(x, file = 'vec.RData')
# })
