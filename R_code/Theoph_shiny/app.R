#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(tidyverse)
library(rsconnect)
library(shinythemes)
th_dm <- readRDS("th_dm.rds")

ui <- navbarPage( 
  theme = shinytheme("cerulean"),
  "Theoph PK Experiment",
  tabPanel("Subjects Overall Information",
           titlePanel("Subject overall information"),
           sidebarLayout(sidebarPanel(
             radioButtons("method", "Choose a method:",
                          c("Point", "Loess Line"), "Point")
           ),
           
           mainPanel( verbatimTextOutput("message"),
                      plotOutput(outputId = "ind_plot",click = "plot_click",
                                 hover = hoverOpts(id = "plot_hover") ),
                      tableOutput("id_info")
           )
           )
  ),
  tabPanel("Each Subject Information",
           titlePanel("All Subject information"),
           sidebarLayout(sidebarPanel(
             sliderInput("obs","Choose the subject:",
                         min = 1, max = 12, value = 1)
           ),
           mainPanel(
             plotOutput(outputId = "plot2"),
             tableOutput("info2"))
           
           )
  )
)

# Define server 
server <- function(input, output) {
  output$message <- renderText(if (input$method == "Point"){
    {"Click points on the plot below to see detail informations"}
  }
  )
  
  output$ind_plot <- renderPlot({
    if (input$method == "Point") {
      ggplot(th_dm, aes(Time, conc)) +
        geom_point(aes(group = SUBJECT, colour = SUBJECT)) +
        labs(x = "Time since drug administration when the sample was drawn (hr)",
             y = "theophylline concentration in the sample (mg/L)")
      
    } else {
      ggplot(th_dm, aes(Time, conc)) + 
        geom_smooth(color = "orange", method = "loess")+
        labs(x = "Time since drug administration when the sample was drawn (hr)",
             y = "theophylline concentration in the sample (mg/L)")
    }
  })
  
  output$id_info <- renderTable({ 
    if (input$method == "Point"){
      nearPoints(th_dm, input$plot_click, addDist = TRUE)
    } 
  })
  
  output$plot2 <- renderPlot({
    th_dm %>% filter(SUBJECT == input$obs) %>%
      ggplot(aes(Time, conc)) + 
      geom_point(aes(group = SUBJECT, colour = SUBJECT)) +
      geom_line(aes(group = SUBJECT, colour = SUBJECT))+
      labs(x = "Time since drug administration when the sample was drawn (hr)",
           y = "theophylline concentration in the sample (mg/L)")
  })
  
  output$info2 <- renderTable({
    th_dm %>% filter(SUBJECT == input$obs)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

