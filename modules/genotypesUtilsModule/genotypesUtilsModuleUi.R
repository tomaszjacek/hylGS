genotypesUtilsModule_UI <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    #tags$style(type='text/css', '#txt_out {white-space: pre-wrap;}'),
    #plotOutput(ns("plot1")),
    actionButton(ns("refreshTab1_id"), "Refresh Tab 1"),
    verbatimTextOutput(ns("pullBranch_string_mainPanel")),
    selectInput(inputId = ns("remoteBranches_list_mainPanel"),
              label = "chose remote branch to download",
              "Names"),
    actionButton(ns("pull_button_mainPanel"), "download hylGS"),
    #verbatimTextOutput(ns("resultPull"))
    htmlOutput(ns("resultPull"))
  )
  #if(interactive()){
  #  print("interactive from noDependencyModuleUi")
  #}
}