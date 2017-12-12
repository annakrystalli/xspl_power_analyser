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
library(shinydashboard)

# effect = d
# group_n = n

#range of effect sizes
d <- seq(.1,1,.1)

# sample values
n <- seq(10,110,10)

df <- expand.grid(d = d, n = n) %>% as_tibble %>% 
    mutate(pwr = pwr.t.test(n, d, sig.level = 0.05, power = NULL,
                            type="two.sample",
                            alternative="two.sided")$power,
           d = as.factor(d))

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
    dashboardHeader(title = "Power estimation for 2 sample t-test",
                    titleWidth = 450),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"),
            menuItem("Raw data", tabName = "rawdata")
        ),
        sliderInput("d",
                    "effect size:",
                    min = 0,
                    max = 1,
                    step = 0.1,
                    value = 0.5)),
    dashboardBody(
        tabItems(
            tabItem("dashboard",
                    fluidRow(valueBoxOutput("n", width = 5),
                             valueBoxOutput("savings", width = 5)),
                    sliderInput("pwr",
                                "power:",
                                min = 0,
                                max = 1,
                                step = 0.1,
                                value = 0.8),
                    fluidRow(box(title = "Power trend ~ effect size", background = "teal", solidHeader = TRUE,
                        plotOutput("plot"), width = 10))
                    ),
            
            tabItem("rawdata")
            )
        )
    )
    
    
    
 

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    
    output$plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        p <- df %>% filter(d == input$d) %>% 
            ggplot(aes(n, pwr, group = d, color = d)) + geom_line() +
            labs(title ="",
                 subtitle = "",
                 color = "effect size") +
            xlab("Sample Size IN EACH GROUP (n)") +
            ylab("Power")  + ylim(0, max(d)) +
            theme_hc(bgcolor = "darkunica") +
            scale_colour_hc("darkunica") + 
            theme(axis.text = element_text(colour = "white"),
                  panel.grid.major = element_line(colour = "grey50")) +
            geom_vline(xintercept = pwr.t.test(n = NULL, d = input$d, sig.level = 0.05, 
                                               power=input$pwr, type="two.sample",
                                               alternative="two.sided")$n %>% as.integer(),
                       colour = "white")
        
        
        #print(ggplotly(p, tooltip = c("n", "d", "pwr")))
        
        print(p)
        
        
    })
    output$n <- renderValueBox({
        valueBox(subtitle = "min n required for EACH group", color  = "teal", 
                 value = pwr.t.test(n = NULL, d = input$d, sig.level = 0.05, 
                            power=input$pwr, type="two.sample",
                            alternative="two.sided")$n %>% as.integer(),
            icon = icon("users")
        )
    })
    output$savings <- renderValueBox({
        valueBox(subtitle = "Savings", color  = "lime", 
                 value = paste("£", pwr.t.test(n = NULL, d = input$d, sig.level = 0.05, 
                                    power=input$pwr, type="two.sample",
                                    alternative="two.sided")$n %>% as.integer()*0.2 *100),
                 icon = icon("money")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

