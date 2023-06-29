source("/home/vboxuser/hylGS/modules/phenotypesExportModule/phenExportDirModule.R")
source("/home/vboxuser/hylGS/modules/phenotypesExportModule/dirListSelectionModule.R")
source("/home/vboxuser/hylGS/modules/phenotypesExportModule/phenQcLayoutBuilderModule.R")

phenotypesExportModule_UI <- function(id) {
  ns <- NS(id)
          tabsetPanel(
            id = ns("tabsetPanelID"),
            tabPanel("phenExportViewer",
                     
               fluidRow(
                 column(width = 3,
                        hr(style = "border-top: 1px solid #000000;"),
                        uiOutput(ns("phen_export_dir_module")),  
                 ),
                 column(width = 9,
                        hr(style = "border-top: 1px solid #000000;"),
                        uiOutput(ns("phen_qc_static_results"))
                 )
               )         
                       
          ),
          tabPanel("phenQcBuilder",
            fluidRow(
              column(width = 12,
                     hr(style = "border-top: 1px solid #000000;"),
                     uiOutput(ns("phen_qc_layout_builder"))
              )
            )    
          ),
          tabPanel("doc")
          )

  
}


