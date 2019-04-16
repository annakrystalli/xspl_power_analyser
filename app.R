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
library(tidyverse)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)

load(here::here("data", "powersim.rda"))

p <- list(y = "power",
          x_choices = setNames(c("effect_size", "n"), c("effect size", "sample size")),
          z_choices = purrr::map(c("effect_size", "n"), ~ sort(unique(powersim[[.x]]))) %>% 
              setNames(c("effect_size", "n"))
)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Power estimation for 2 sample t-test",
                                    titleWidth = 450),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Dashboard", tabName = "dashboard"),
                            menuItem("Raw data", tabName = "rawdata")
                        ),
                        selectInput("x", "Select x-axis variable", choices = p$x_choices,
                                    selected = p$x_choices["sample size"]),
                        gradientBox(
                            title = "About",
                            closable = FALSE,
                            collapsible = FALSE,
                            boxToolSize = "lg",
                            width=12,
                            footer = "We simulate accuracy and reaction times for known differences between two groups. These graphs show the sensitivity (statistical power) across different sample sizes (for a known effect size), or for different effect sizes (for a known sample size). The three lines show how the sensitivity varies if you test for a difference using (just) accuracy, (just) reaction time or if you combine them using decision modelling to recover a drift pararameter",
                            "This is a gradient box"
                        ),
                        gradientBox(
                            title = "More",
                            closable = FALSE,
                            collapsible = FALSE,
                            boxToolSize = "lg",
                            width=12,
                            footer = "Pre-print: <a href=\"\">link</a>"
                        )
                        ),
                    dashboardBody(
                        tabItems(
                            tabItem("dashboard",
                                    fluidRow(box(title = "Statistical power functions for different measures", background = "teal", solidHeader = TRUE,
                                                 uiOutput("z_slider"),
                                                 plotOutput("plot"), width = 10)),
                                    fluidRow(valueBoxOutput("n", width = 5),
                                             valueBoxOutput("savings", width = 5)))),
                        tabItem("rawdata")
                    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_z <- reactive({
        p$x_choices[input$x  != p$x_choices]
    })
    
    subset_dat <- reactive({
        col <- if(v$z == "effect_size"){v$z}else{v$z}
        powersim %>%  
            filter((!!rlang::sym(col)) == v$z_value)
    })
    
    v <- reactiveValues(
        z = "effect_size",
        z_choices = p$z_choices[["effect_size"]],
        z_value = 3
    )
    
    observe({
        v$z <- get_z()
        v$z_choices <- p$z_choices[[v$z]]
    })
    
    output$z_slider <- renderUI({
        selectInput("z_value", paste("select", names(v$z)), choices = v$z_choices)
    })
    
    output$plot <- renderPlot({
     
        # generate bins based on input$bins from ui.R
        v$z_value <- input$z_value
        
        p <- subset_dat() %>%
            ggplot2::ggplot(aes_(as.name(input$x), as.name(p$y),
                                 colour = as.name("condition"))) + 
            geom_point(alpha = 1) + geom_line(alpha = 1,size=3) +
            labs(title ="",
                 subtitle = "",
                 color = "Measure") +
            #theme_hc(bgcolor = "darkunica") +
            scale_colour_hc("darkunica") + 
            ylim(0, 1) +
            theme(axis.text = element_text(colour = "black",size=12),
                  panel.grid.major = element_line(colour = "grey50")#, legend.position="right"
                  ) 
            #geom_vline(xintercept = pwr.t.test(n = NULL, d = input$d, sig.level = 0.05, 
            #                                  power=input$pwr, type="two.sample",
            #                                 alternative="two.sided")$n %>% as.integer(),
            #          colour = "white")
            
            
            #print(ggplotly(p, tooltip = c("n", "d", "pwr")))
            
            print(p)
        
        
    })
    output$n <- renderValueBox({
        valueBox(subtitle = "Sample size required for 80% power", color  = "teal", 
                 value = 56,
                 icon = icon("users")
        )
    })
    output$savings <- renderValueBox({
        valueBox(subtitle = "Saving", color  = "lime", 
                 value = paste("£", 10),
                 icon = icon("money")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

