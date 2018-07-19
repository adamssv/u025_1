library(shiny)

shinyServer(function(input, output) {
 

        output$report1 <- renderTable(report1, colnames=FALSE, bordered=TRUE, spacing='xs',align='lr')
        output$report2 <- renderTable(report2, colnames=FALSE, bordered=TRUE,spacing='xs',align='llr')
        output$report3 <- renderTable(report3, colnames=FALSE, bordered=TRUE,spacing='xs',align='llr')
        output$report4 <- renderTable(report4 , colnames=FALSE, bordered=TRUE, spacing='xs',align='llr')
        

  
}
)