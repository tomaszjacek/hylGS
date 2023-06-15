source("/home/vboxuser/hylGS/modules/phenotypesExportModule/phenExportDirModule.R")
source("/home/vboxuser/hylGS/modules/phenotypesExportModule/dirListSelectionModule.R")

phenotypesExportModule_UI <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    #tags$style(type='text/css', '#txt_out {white-space: pre-wrap;}'),
    #plotOutput(ns("plot1")),
    #actionButton(ns("refreshTab1_id"), "Refresh Tab 1"),
    #verbatimTextOutput(ns("pullBranch_string_mainPanel")),
    #selectInput(inputId = ns("remoteBranches_list_mainPanel"),
    #          label = "chose remote branch to download",
    #          "Names"),
    #actionButton(ns("pull_button_mainPanel"), "download hylGS"),
    #verbatimTextOutput(ns("resultPull"))
    #htmlOutput(ns("resultPull")),
    
    hr(style = "border-top: 1px solid #000000;"),
    uiOutput(ns("phen_export_dir_module")),
    
    hr(style = "border-top: 1px solid #000000;"),
    uiOutput(ns("phen_qc_static_results"))
    #phenExportDirModule_UI()
  )
  #if(interactive()){
  #  print("interactive from noDependencyModuleUi")
  #}
}

