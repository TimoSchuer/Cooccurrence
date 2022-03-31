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
                  selectInput("Variants", "Choose Columns where Variants and contextual factors are defined",choices = NULL, multiple = TRUE),
                  selectInput( "Values", "Select Variables to plot",choices = NULL, multiple = TRUE)
                ),
                fluidRow(
                  actionButton("count", "Count Variables"),
                  actionButton("plotPerVar", "Plot Cooccurrence per Variable"),
                  actionButton("plotCooc", "Plot Cooccurence of all Vars")

                ),
                fluidRow(
                  plotOutput("CountVars")
                )

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
    observe({
      x <- input$VariableName
      Values <- dataInput() %>% select(input$VariableName) %>% unique()
      updateSelectInput(session, "Values", "Select Variables to plot", choices = Values)
    })
    output$exbData <- renderDataTable({dataInput()})
    plotCounts <- eventReactive(input$count,{
      dataInput() %>% countVars(Variable = input$VariableName, Variante = input$Variants) %>% ggplot2::ggplot(aes(x=Variable, y=n, fill= Variante))+ ggplot2::geom_col( position= "stack")
    })
    output$CountVars <- renderPlot({plotCounts()})
  }
 )
 # shinyApp(ui, server)
}


