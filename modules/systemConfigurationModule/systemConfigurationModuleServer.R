systemConfigurationModule_Server <- function(id, dat) {
  moduleServer(
    id,
    function(input,output,session){
      
      returnLastAfterSplit<-function(tmpString) {
        tmp<-strsplit(tmpString, "_", fixed=TRUE)
        return(tail(tmp[[1]],1))
      }
      
      saveSettings <- function(settings,savePath) {
          tryCatch(
            {
              write_json(settings, savePath)
              return(TRUE)
            },
            error = function(e) {
              print(e)
              return(FALSE)
            }
          )
      }
      #print(globalData$hylGsSettings)
      # Update form fields based on loaded settings
      output$formFields <- renderUI({
        
        settingsData<-dat()
        #settingsData <- dat$GetStash$hylGsSettings()
        #settingsFileName <- dat$GetStash$hylGsSettingsFileName()
        # print("----------------------------")
        # print(settingsData)
        # print("----------------------------")

        settingsData<-dat()
        if (is.null(settingsData))
          return(NULL)
        
        fields <- lapply(names(settingsData), function(var_name) {
          if (returnLastAfterSplit(var_name)=="numeric") {
            numericInput(var_name, var_name, value = settingsData[[var_name]])
          } else if (returnLastAfterSplit(var_name)=="text") {
            textInput(var_name, var_name, value = settingsData[[var_name]])
          } else if(returnLastAfterSplit(var_name)=="textSelect"){
            # Handle other input types as needed (e.g., selectInput, checkboxInput)
            textInput(var_name, var_name, value = settingsData[[var_name]])
          } else if(returnLastAfterSplit(var_name)=="numericSelect"){
            # Handle other input types as needed (e.g., selectInput, checkboxInput)
            textInput(var_name, var_name, value = settingsData[[var_name]])
          }else{
            renderText("unknown Variable Type")
          }
          
        })
        
        do.call(tagList, fields)
      })

      out <- reactiveValues()
      # out$data <- eventReactive(input$saveGsSettings_button,
      #   saveSettings(settingsData,"hylGsSettings_new.json")
      # )
      observeEvent(input$saveGsSettings_button,{
          out$data<-saveSettings(settingsData,"hylGsSettings_new.json")
      })
      output$resultSaveSettings <-renderTable({ out$data })
      
      #output$localTagVersion_string <- renderText({paste0("local hylGS version: ",system2("ls", "-al", stdout = TRUE, stderr = TRUE))})
      
      
    }
  )
}


#--------------------------------------------
# lapply(names(settingsData), function(var_name) {
#   if (returnLastAfterSplit(var_name)=="numeric") {
#     print(paste0("numeric ",var_name," ",settingsData[[var_name]]))
#   } else if (returnLastAfterSplit(var_name)=="text") {
#     print(paste0("text ",var_name," ",settingsData[[var_name]]))
#   } else if(returnLastAfterSplit(var_name)=="textSelect"){
#     print(paste0("textSelect ",var_name," ",settingsData[[var_name]]))
#   } else if(returnLastAfterSplit(var_name)=="numericSelect"){
#     print(paste0("numericSelect ",var_name," ",settingsData[[var_name]]))
#   }else{
#     print("unknown Variable Type")
#   }
#   
# })
# 
# lapply(names(settingsData), function(var_name) {
#   print(paste0("varName ",var_name))
# })

