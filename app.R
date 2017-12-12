#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggthemes)
library(shiny)
library(pwr)
library(tidyverse)
library(plotly)
# effect = d
# group_n = n

#range of effect sizes
d <- seq(.1,1,.1)

# sample values
n <- seq(10,110,10)

df <- expand.grid(d = d, n = n) %>% as_tibble %>% 
    mutate(pwr = pwr.t.test(n, d, sig.level=0.05, power=NULL,
                            type="two.sample",
                            alternative="two.sided")$power,
           d = as.factor(d))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Power estimation for 2 sample t-test"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("d",
                     "effect size:",
                     min = 0,
                     max = 1,
                     step = 0.1,
                     value = 0.5),
         sliderInput("pwr",
                     "power:",
                     min = 0,
                     max = 1,
                     step = 0.1,
                     value = 0.8),
         br(),
         h4("Sample size needed IN EACH group"),
         h1(textOutput("n"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          plotlyOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlotly({
      # generate bins based on input$bins from ui.R
       
      p <- df %>% filter(d == input$d) %>% 
           ggplot(aes(n, pwr, group = d, color = d)) + geom_line() +
           labs(title ="",
                subtitle = "",
                color = "effect size") +
           xlab("Sample Size IN EACH GROUP (n)") +
           ylab("Power") +
          theme_minimal() + ylim(0, max(d))

      
      #print(ggplotly(p))
      
      p
      

   })
   output$n <- renderText({
       pwr.t.test(n = NULL, d = input$d, sig.level=0.05, power=input$pwr,
                  type="two.sample",
                  alternative="two.sided")$n %>% as.integer()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

