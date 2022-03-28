shinyCooc <- function(){
 shiny::shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Analyzing cooccurrence from spoken data"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Analyze singe file", tabName = "singleFile")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "singleFile",
                fluidRow(
                  box(shiny::fileInput("file","Choose EXB File", accept = ".exb")),
                  box(shiny::checkboxGroupInput("annotation", "Choose way of annotation", choices = c("linear", "multilayer"), selected = "linear")),
                  box(shiny::checkboxInput("MetaData", "Read Metadata"), value= TRUE),
                  box(shiny::actionButton("build", "Read exb file"))
                ),
                fluidRow(
                  dataTableOutput("exbData"),
                  selectInput("VariableName", "Choose Column where Variable Name is defined",choices = NULL ),
                  selectInput("Variants", "Choose Columns where Variants and contextual factors are defined",choices = NULL, multiple = TRUE)
                ),

                )
      )
    )
  ),
  server = function(input, output, session) {
    dataInput <- eventReactive(input$build,{

      dataExb <- ExmaraldaR::read_exb_file(path = input$file$datapath, annotation = input$annotation,addMetaData = input$MetaData )
      varN <- names(dataExb)
      updateSelectInput(session, "VariableName", "Choose Column where Variable Name is defined", choices = varN)
      updateSelectInput(session, "Variants", "Choose Columns where Variants and contextual factors are defined", choices = varN)
      dataExb
    })
    output$exbData <- renderDataTable({dataInput()})
  }
 )
 # shinyApp(ui, server)
}


