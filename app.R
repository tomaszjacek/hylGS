library("shiny")

ui <- bootstrapPage(


  verbatimTextOutput("localTagVersion"),
  selectInput(inputId = "mainWin_tagList",
              label = "chose main branch version to download",
              "Names"),
  actionButton("pull", "download hylGS"),
  verbatimTextOutput("result"),
)

server <- function(input, output, session) {
  #list all tags in main branch on remote repository
  tagList<-system2("git", "show-ref --tags", stdout = TRUE, stderr = TRUE)
  #clean the strings like "4d722717d2579f02b2f9c311513d355e36774aef refs/tags/v0.0.0.1" "b06d7e1b9cdd4a88a00989ebec75181bef6c6043 refs/tags/v0.0.0.2"  to "v0.0.0.1" "v0.0.0.2" 
  tagListClean <- sub("^.*/", "", tagList)
  
  updateSelectizeInput(session, "mainWin_tagList",choices = tagListClean)
  
  result <- eventReactive(input$pull, {
    system2("git", "pull origin main", stdout = TRUE, stderr = TRUE)
  })

  output$localTagVersion <- renderText({paste0("local hylGS version: ",system2("git", "describe --tags", stdout = TRUE, stderr = TRUE))})
  
  #output$user <- renderTable({
  #  info <- Sys.info()
  #  data.frame(variable = names(info), values = unname(info))
  #})

  output$result <- renderPrint({
    result()
  })

}


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
