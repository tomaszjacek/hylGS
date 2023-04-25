library("shiny")

ui <- bootstrapPage(
  actionButton("pull", "Pull Master Branch"),
  verbatimTextOutput("result"),
  tableOutput("user")
)

server <- function(input, output) {

  result <- eventReactive(input$pull, {
    system2("git", "pull origin main", stdout = TRUE, stderr = TRUE)
  })

  output$user <- renderTable({
    info <- Sys.info()
    data.frame(variable = names(info), values = unname(info))
  })

  output$result <- renderPrint({
    result()
  })

}


shinyApp(ui, server)

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
