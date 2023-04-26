library("shiny")

ui <- bootstrapPage(


  verbatimTextOutput("localTagVersion_string_mainPanel"),
  verbatimTextOutput("localBranchList_string_mainPanel"),
  verbatimTextOutput("remoteBranchList_string_mainPanel"),
  
  verbatimTextOutput("pullBranch_string_mainPanel"),
  selectInput(inputId = "remoteBranches_list_mainPanel",
              label = "chose remote branch to download",
              "Names"),
  actionButton("pull_button_mainPanel", "download hylGS"),
  verbatimTextOutput("resultPull"),
  
  selectInput(inputId = "localBranchesTags_list_mainPanel",
              label = "chose branch:tag version to switch",
              "Names"),
  actionButton("pull", "download hylGS"),
  verbatimTextOutput("resultPull"),
)

server <- function(input, output, session) {
  
  #check hylGS's branches present in local repository
  localBranchList<-system2("git", "branch -l", stdout = TRUE, stderr = TRUE)
  localBranchListClean <- unique(sub("^.*/", "", localBranchList))
  localBranchListClean <- sub("\\* ", "", localBranchListClean)
  output$localBranchList_string <- renderText(paste0("local hylGS branches: ",paste(localBranchListClean, collapse=", ")))  
  
  
  #check hylGS's branches present in remote repository (https://github.com/tomaszjacek/hylGS.git)
  remoteBranchList<-system2("git", "branch -r", stdout = TRUE, stderr = TRUE)
  remoteBranchListClean <- unique(sub("^.*/", "", remoteBranchList))
  output$remoteBranchList_string <- renderText(paste0("remote hylGS branches: ",paste(remoteBranchListClean, collapse=", ")))
  #update selection list on webpage
  updateSelectizeInput(session, "remoteBranches_list_mainPanel",choices = remoteBranchListClean)
  
  
  #list all tags in main branch on remote repository
  tagList<-system2("git", "show-ref --tags", stdout = TRUE, stderr = TRUE)
  #clean the strings like "4d722717d2579f02b2f9c311513d355e36774aef refs/tags/v0.0.0.1" "b06d7e1b9cdd4a88a00989ebec75181bef6c6043 refs/tags/v0.0.0.2"  to "v0.0.0.1" "v0.0.0.2" 
  tagListClean <- sub("^.*/", "", tagList)
  #update selection list on webpage
  updateSelectizeInput(session, "localBranchesTags_list_mainPanel",choices = tagListClean)
  
  resultPull <- eventReactive(input$pull_button_mainPanel, {
    system2("git", "pull origin main", stdout = TRUE, stderr = TRUE)
  })

  output$localTagVersion_string <- renderText({paste0("local hylGS version: ",system2("git", "describe --tags", stdout = TRUE, stderr = TRUE))})
  
  

  

  
  localVsRemoteBranchesDiff <- localBranchList[!(localBranchList %in% remoteBranchList)]
  
  if(length(localVsRemoteBranchesDiff>0)){
    output$ <- renderText(paste0("local hylGS branches: ",paste(localBranchListClean, collapse=", ")))
  }
  
  #output$user <- renderTable({
  #  info <- Sys.info()
  #  data.frame(variable = names(info), values = unname(info))
  #})

  output$result <- renderPrint({
    result()
  }) 

}

#git switch --detach v0.0.0.1

shinyApp(ui, server)

#####################!!!!###############################

# ui <- fluidPage(
#   titlePanel("Download reprex"),
#   
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       shinyWidgets::downloadBttn(
#         outputId = "download",
#         label = "Download data as .RData",
#         style = "bordered",
#         color = "default")
#       ,
#       shiny::downloadButton(
#         outputId = "download",
#         label = "Download data as .RData")
#     )
#     
#   )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   data <- data.frame(x1 = runif(100), r2 = runif(100))
#   output$download <- shiny::downloadHandler(
#     filename = function() {
#       paste("file_to_dl.RData")
#     },
#     content = function(file) {
#       save(data, file = file)
#     }
#   )
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
