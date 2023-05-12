gitModule_Server <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
      #get local version
      output$localTagVersion_string_mainPanel <- renderText({paste0("local hylGS version: ",system2("git", "describe --tags", stdout = TRUE, stderr = TRUE))})
      
      
      #check hylGS's branches present in local repository
      localBranchList<-system2("git", "branch -l", stdout = TRUE, stderr = TRUE)
      localBranchListClean <- unique(sub("^.*/", "", localBranchList))
      localBranchListClean <- sub("\\* ", "", localBranchListClean)
      output$localBranchList_string_mainPanel <- renderText(paste0("local hylGS branches: ",paste(localBranchListClean, collapse=", ")))  
      
      
      #check hylGS's branches present in remote repository (https://github.com/tomaszjacek/hylGS.git)
      remoteBranchList<-system2("git", "branch -r", stdout = TRUE, stderr = TRUE)
      remoteBranchListClean <- unique(sub("^.*/", "", remoteBranchList))
      output$remoteBranchList_string_mainPanel <- renderText(paste0("remote hylGS branches: ",paste(remoteBranchListClean, collapse=", ")))
      #update selection list on webpage
      updateSelectizeInput(session, "remoteBranches_list_mainPanel",choices = remoteBranchListClean)
      
      
      #list all tags in main branch on remote repository
      tagList<-system2("git", "show-ref --tags", stdout = TRUE, stderr = TRUE)
      #clean the strings like "4d722717d2579f02b2f9c311513d355e36774aef refs/tags/v0.0.0.1" "b06d7e1b9cdd4a88a00989ebec75181bef6c6043 refs/tags/v0.0.0.2"  to "v0.0.0.1" "v0.0.0.2" 
      tagListClean <- sub("^.*/", "", tagList)
      #update selection list on webpage
      updateSelectizeInput(session, "localBranchesTags_list_mainPanel",choices = tagListClean)
      
      resultSwitch <- eventReactive(input$switch_button_mainPanel, {
        print(paste0("git ", "switch --detach ",input$localBranchesTags_list_mainPanel))
        system2("git", paste0("switch --detach ",input$localBranchesTags_list_mainPanel), stdout = TRUE, stderr = TRUE)
      })
      
      
      
      
      
      
      
      #localVsRemoteBranchesDiff <- localBranchListClean[!(localBranchListClean %in% remoteBranchListClean)]
      localVsRemoteBranchesDiff <- remoteBranchListClean[!(remoteBranchListClean %in% localBranchListClean)]
      if(length(localVsRemoteBranchesDiff>0)){
        output$branchDiffInfo_string_mainPanel <- renderText(paste0("missing hylGS branches : ",paste(localVsRemoteBranchesDiff, collapse=", ")))
        resultPull <- eventReactive(input$pull_button_mainPanel, {
          system2("git", "pull origin main", stdout = TRUE, stderr = TRUE)
        })
      }
      
      #output$user <- renderTable({
      #  info <- Sys.info()
      #  data.frame(variable = names(info), values = unname(info))
      #})
      
      output$result <- renderPrint({
        result()
      }) 
      
    }
  )
}

