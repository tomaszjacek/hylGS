library(R6)
library ("purrr")
source("/home/vboxuser/hylGS/modules/phenotypes/phenotypesModulesUtils/qcVisualizationBuilderUtils.R")
#library(shinyjs)

#js <- "shinyjs.refresh = function() { location.reload(); }"
#js <- 


qcWidgetsTypes <- c("chartIMG", "tableHTML")

# Simple R6 object
#qcFormVariablesObjR6 <- R6::R6Class(
chartObject <- R6::R6Class(
  classname = "chartObject",
  public = list(
    #triggers = reactiveValues(plot = 0),
    #trigger_plot = function() {
    #  self$triggers$plot <- self$triggers$plot + 1
    #},
    idName = NULL,
    selected_value = NULL,
    traitName = NULL,
    file_name_extension = NULL,
    table_or_chart_command = NULL,
    output_png_file_name = NULL,
    
    set_variables = function(ident,sv,tn,fne,tocc,opfn) {
      self$idName <- ident
      self$selected_value <- sv
      self$traitName <- tn
      self$file_name_extension <-fne
      self$table_or_chart_command <- tocc
      self$output_png_file_name <- opfn
    }
  )
)

traitObject <- R6::R6Class(
  classname = "traitObject",
  public = list(
    chartsObjectsList = list(),
    htmlSkin = "",
    traitName = "",
    add_chartObject = function(chObject) {
      
      chIdName <- chObject$idName
      trait <- chObject$traitName
      print(paste0("add_qcObject R6 of",trait," | ",chIdName))
      if(is.null(chIdName) | chIdName == ""){
      }else{
        self$chartsObjectsList[[chIdName]] <- chObject
      }
      print(paste0("add_qcObject R6 names ",names(self$chartsObjectsList)))
    },
    rm_chartObject = function(qcObjectId) {
      self$chartsObjectsList[[qcObjectId]] <- NULL
    },
    get_charts_names = function() {
      
      if(!is_empty(names(self$chartsObjectsList))){
        return(names(self$chartsObjectsList))
      }else{
        return(NULL)
        
      }
    }
  )
)
chartsObjectsManager <- R6::R6Class(
  classname = "chartsObjectsManager",
  
  public = list(
    traitsObjectsList = list(),


    add_trait = function(tObject) {
      traitName <- tObject$traitName
      self$traitsObjectsList[[traitName]] <- tObject
    },

    rm_trait = function(traitName) {
      print(paste0("rm_trait ",traitName))
      #print("rm_trait ",names(self$qcObjects))
      self$traitsObjectsList[[traitName]] <- NULL
      print("rm_trait end")
    },
    update_trait_htmlSkin = function(traitName, htmlSkin){
      self$traitsObjectsList[[traitName]]$htmlSkin <- htmlSkin
    },
    get_traits_names = function() {
      
      if(!is_empty(names(self$traitsObjectsList))){
        return(names(self$traitsObjectsList))
      }else{
        return(NULL)

      }
    },
    get_trait_chartsObjectsNames= function(traitName) {
      tmp <- self$traitsObjectsList[[traitName]]
      chartsNamesList <- tmp$get_charts_names()
      if(length(chartsNamesList)){print(paste0("get_trait_chartsObjectsNames: ",chartsNamesList))
        return(chartsNamesList)
      }else{
        return(NULL)
      }
    },
    add_chartObject_toTrait= function(tName,ob) {
      if(tName %in% names(self$traitsObjectsList)){
        self$traitsObjectsList[[tName]]$add_chartObject(ob)
      }else{
        return(NULL)
        
      }
    }
  )
)


chartForm_ui <- function(id,qcoName) {
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
chartForm_server <- function(id, traitName, varManager) {
  moduleServer(id, function(input, output, session) {
    print("chartForm_server:START")
    
    # print(varManager$get_traits_names())
    # observeEvent(input$addQcObject, {
    #   print("addQcObject")
    #   # Add another item
    #   newObj <- qcFormVariablesObjR6$new()
    #   #newObjName <- paste0("Object_", objCount())
    #   #newObj$set_variables(input$ident,input$sv,input$tn,input$fne,input$tocc,input$opfn)
    #   varManager$add_qcObject(input$objSelection,newObj,qcFormVariablesEdit_server(newObj))
    # })
    
    print("chartForm_server:END")
  })
}





traitTab_ui <- function(id) {
  ns <- NS(id)
  #qcForm imported from phenotypesExportModuleUtils.R

  fluidPage(
    #tabsetPanel(id = ns("qcFormsMainPanel")),
    uiOutput(ns("chartsTabsetpanel")),
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


scatter_chart_objects <- function(chartsList, chartSelected,nsId ){
  print(paste0("scatter_chart_objects_sart: ",chartsList))
  
  tp<-NULL
  if(length(chartsList)>0){
    tp <- 
      do.call(tabsetPanel, c(id=nsId('qcFormsMainPanel'), lapply(1:length(chartsList), function(i) {
        tabPanel(chartsList[i], chartForm_ui(nsId(chartsList[i]),chartsList[i]))
      }),selected = chartSelected))
  }
  print(paste0("scatter_chart_objects_end: ",chartsList))
  return(tp)
}

#newObj <- qcFormVariablesObjR6$new()
#newObjName <- paste0("Object_", objCount())
#
#variablesManager$add_qcObject(input$traitName,newObj,)
# Module Server
traitTab_server <- function(id, varManager) {
  moduleServer(id, function(input, output, session) {
    traitName<-id
    print(paste0("traitTab_server:START | ",id))
    ns <- session$ns
    qcObjectsServers <- list()
    traitQcObjectsList <- varManager$get_trait_chartsObjectsNames(id)
    print(paste0(" varManager$get_trait_chartsObjectsNames(id) :",traitQcObjectsList))
    
    
    output$chartsTabsetpanel <- renderUI({
      if(length(traitQcObjectsList) >0){ 
        scatter_chart_objects(traitQcObjectsList,traitQcObjectsList[1], ns)
      }
    })
    for(o in 1:length(traitQcObjectsList)){
      name <- traitQcObjectsList[o]
      
      qcObjectsServers[[name]] <- chartForm_server(name,traitName,varManager)
      #appendTab("qcFormsMainPanel", tabPanel(name, chartForm_ui(ns(name),name)), select = TRUE)
    }
    
    print(paste0("varManager$get_traits_names() :",varManager$get_traits_names()))
    observeEvent(input$addQcObject, {
      #print("addQcObject")
      # Add another item
      
      
      #print("0.1")
      if(input$qcObjName == "" | is.null(input$qcObjName)){
        #newObj$set_variables("","","","","","")
      }else{
        newName<-input$qcObjName
        print("addQcObject inelse0")
        newObj <- chartObject$new()
        print("addQcObject inelse1")
        newObj$set_variables(newName,"",traitName,"","","")
        print(paste0("addQcObject inelse2 ::",traitName))
        varManager$add_chartObject_toTrait(traitName,newObj)

        newtraitQcObjectsList<-varManager$get_trait_chartsObjectsNames(traitName)
        print(paste0("varManager$get_trait_chartsObjectsNames(traitName)!!!!!!!!!!!!! :",varManager$get_trait_chartsObjectsNames(traitName)))
        output$chartsTabsetpanel <- renderUI({
          scatter_chart_objects(newtraitQcObjectsList,newName, ns)
        })
        print("addQcObject inelse3")
        qcObjectsServers[[newName]] <- chartForm_server(newName,traitName,varManager)
        #print("addQcObject inelse4")
        #appendTab("qcFormsMainPanel", tabPanel(newName, chartForm_ui(ns(newName),newName)), select = TRUE)
      }
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
    
    #loadConfiguration <- reactiveVal(0)
    activeTraitTab<-NULL
    traitsServers <- list()
    #objCount <- reactiveVal(0)
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
          #print(paste0("renderUI_1: ",variablesManager$triggers$plot))
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
        newTraitObject  <- traitObject$new()
        newTraitObject$htmlSkin <- newTraitHtmlSkin
        newTraitObject$traitName <- newTraitName 
        variablesManager$add_trait(newTraitObject)
      
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
