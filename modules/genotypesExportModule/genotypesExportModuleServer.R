genotypesExportModule_Server <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
      #output$plot1 <- renderPlot({
      #  plot(mtcars)
      #})
      pwd<-getwd()
      output$pullBranch_string_mainPanel<-renderText(pwd)
      
      remoteBranchListClean<-c("hello","aloha")
      updateSelectizeInput(session, "remoteBranches_list_mainPanel",choices = remoteBranchListClean)
      #output$remoteBranchList_string <- renderText(paste0("remote hylGS branches: ",paste(remoteBranchListClean, collapse=", ")))
      
      lsResult<-system2("ls", "-al", stdout = TRUE, stderr = TRUE)
      lsResult<- paste0("<div class=\"white-space-pre-line\" style=\"background: green\">", lsResult,"</div>")
      #output$resultPull<-renderText(lsResult)
      
      
      
      # observe({
      #   if (input$refreshTab1_id) {
      #     session$reload()
      #   }
      # })
      output$resultPull <- eventReactive(input$pull_button_mainPanel,
        system2("ls", "-al", stdout = TRUE, stderr = TRUE)
      )

      #output$localTagVersion_string <- renderText({paste0("local hylGS version: ",system2("ls", "-al", stdout = TRUE, stderr = TRUE))})
      
      
    }
  )
}

