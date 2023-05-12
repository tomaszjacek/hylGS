gitModule_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("hylGS local version managing page"),
    hr(),
    
    verbatimTextOutput(ns("localTagVersion_string_mainPanel")),
    verbatimTextOutput(ns("localBranchList_string_mainPanel")),
    verbatimTextOutput(ns("remoteBranchList_string_mainPanel")),
    verbatimTextOutput(ns("branchDiffInfo_string_mainPanel")),
    verbatimTextOutput(ns("pullBranch_string_mainPanel")),
    selectInput(inputId = ns("remoteBranches_list_mainPanel"),
                label = "chose remote branch to download",
                "Names"),
    actionButton(ns("pull_button_mainPanel"), "download hylGS"),
    verbatimTextOutput("resultPull"),
    
    selectInput(inputId = ns("localBranchesTags_list_mainPanel"),
                label = "chose branch:tag version to switch",
                "Names"),
    actionButton(ns("switch_button_mainPanel"), "switch version hylGS"),
    verbatimTextOutput(ns("resultSwitch"))
  )

}