library(shiny)

shinyUI(
  fluidPage(
    title="U025 Report",
    titlePanel(h3("U025 Study Progress Report - Observational Arm")),
      mainPanel(  h5(strong("Accrual"))),
    fluidRow(
      column(12,
            # DT::dataTableOutput('report1')
            tableOutput('report1')
            )
    ),
    
    mainPanel( h5(strong("Pathology"))),
    fluidRow(
      column(12,
             tableOutput('report2')
      )
    ),
    
    mainPanel( h5(strong("Visits"))),
    fluidRow(
      column(12,
             tableOutput('report3')
      )
    ),
    
    mainPanel( h5(strong("Study Completion Status"))),
    fluidRow(
      column(12,
             tableOutput('report4')
      )
    )
    
  )
)
