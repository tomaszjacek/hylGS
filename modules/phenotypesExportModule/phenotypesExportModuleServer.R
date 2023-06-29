phenDir <- "/media/hylGS/crunch/phenotypeDataFromResoraDB/"

phenotypesExportModule_Server <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
      
      rv <- reactiveValues(selectedButton = NULL,tmp=NULL)
      
      {
      metaFilesList<-list.files(phenDir, pattern='meta')
      phenExportFoldersList <- stringr::str_replace_all(metaFilesList,pattern = ".meta", replacement = "")
      
      # 
      # lapply(phenExportFoldersList, function(m) {  
      #   phenExportDirModule_UI(ns(paste("phenExportDirModule_", m, sep = "")))
      # })
      
      
      output$phen_export_dir_module <- renderUI({
        if (length(phenExportFoldersList)==0)
          return()
        
        ns <- session$ns
        
        lapply(phenExportFoldersList, function(m) {  
          
          dirListSelectionModule_UI(ns(paste("dirListSelectionModule", m, sep = "")))
        })
        
      })
      
      
      }
      
      {
        # Call module show_data
      output$phen_qc_static_results<-renderUI({
        ns <- session$ns
       # selectedButton <-  lapply(phenExportFoldersList, function(m) {  
        phenExportDirModule_UI(ns("phenDirSelection"))
                           # })
      })
    }
        
        {
          lapply(phenExportFoldersList, function(m) {  
            # dirListSelectionModule(ns(paste("phenDirSelection", m, sep = "")),m)
            rv$selectedButton[[paste("dirListSelectionModule", m, sep = "")]]<- callModule(module = dirListSelectionModule, id = paste("dirListSelectionModule", m, sep = ""),m)
          })
          
        }
      
      {         
        

          lapply(rv$selectedButton, function(i) { 
        # Call module funHistory
            observeEvent(rv$selectedButton[[paste("dirListSelectionModule", i$dirSelected, sep = "")]],{
               callModule(module = phenExportDirModule, id = "phenDirSelection", dirToProcess = i$dirSelected)
          })
        })
        }
      
      
      


            
      
        # Call module show_data
        output$phen_qc_layout_builder<-renderUI({
          ns <- session$ns
          phenQcLayoutBuilderModule_UI(ns("phenQcBuilder"))
        })
      
        # observeEvent(input$tabsetPanelID,{
        #   if(input$tabsetPanelID=="phenQcBuilder"){
             phenQcLayoutBuilderModule_Server(id = "phenQcBuilder")
        #   print("alloha")
        #   }
        # }, ignoreNULL = TRUE, ignoreInit = TRUE)
        

      
    }
  )
}

