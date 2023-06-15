
loadSystemSettings <- function(filepath) {
  if (is.null(filepath))
    return(NULL)
  
  settings <- tryCatch(
    {
      read_json(filepath)
    },
    error = function(e) {
      return(NULL)
    }
  )
  
  return(settings)
}

globalVariablesModule <- function(input, output, session) {
  
  #globalData$hylGsSettingsFileName <- "hylGsSettings.json"
  globalData <- loadSystemSettings("hylGsSettings_new.json")
  #print(globalData)
  #globalData <- reactiveValues( hylGsSettingsFileName = "hylGsSettings.json", hylGsSettings=NULL)
  #globalData <- reactive(loadSystemSettings("hylGsSettings.json"))
  
   # reactive({
   #   globalData <- loadSystemSettings(globalData$hylGsSettingsFileName)
   # })
  
  # print("globalVariablesModule inside start")
  # print(globalData)
  # print("globalVariablesModule inside end")


  
  #return(list(GetGlobalVal1 = reactive(stash$GlobalVal1))
  ##return(list(GetStash=globalData))
  return(reactive(globalData))
         
  #return(globalData)
}