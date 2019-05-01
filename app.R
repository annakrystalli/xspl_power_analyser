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
library(shinyWidgets)

load(here::here("data", "powersim.rda"))

p <- list(y = "power",
          x_choices = setNames(c("effect_size", "n"), c("effect size", "sample size")),
          z_choices = purrr::map(c("effect_size", "n"), ~ sort(unique(powersim[[.x]]))) %>% 
              setNames(c("effect_size", "n")),
          z_selected = setNames(c(2, 40), c("effect_size", "n")),
          x_axis_label = setNames(c("sample size per group", "true effect in drift"), 
                                c("effect_size", "n")),
          box_z_var = setNames(c("effect size", "sample size"), c("effect_size", "n"))
)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Power estimation for 2 sample t-test",
                                    titleWidth = 450),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Dashboard", tabName = "dashboard")
                            #menuItem("Raw data", tabName = "rawdata")
                        ),
                        selectInput("x", "Select x-axis variable", choices = p$x_choices,
                                    selected = p$x_choices["sample size"]),
                        gradientBox(
                            title = "About",
                            closable = FALSE,
                            collapsible = FALSE,
                            boxToolSize = "lg",
                            width=12,
                            p("We simulate accuracy and reaction times for known differences between two groups. These graphs show the sensitivity (statistical power) across different sample sizes (for a known effect size), or for different effect sizes (for a known sample size). The three lines show how the sensitivity varies if you test for a difference using (just) accuracy, (just) reaction time or if you combine them using decision modelling to recover a drift pararameter",
                            "This is a gradient box",style="color:black")
                        ),
                        gradientBox(
                            title = "More",
                            closable = FALSE,
                            collapsible = FALSE,
                            boxToolSize = "lg",
                            width=12,
                            p("Pre-print: ", style="color:black"),
                            a("Quantifying the benefits of using decision models with response time
and accuracy data", href="http://psyarxiv.com/",style="color:blue") 
                            #footer = "Pre-print: <a href=\"\">link</a>"
                        )
                        ),
                    dashboardBody(
                        tabItems(
                            tabItem("dashboard",
                                    fluidRow(box(title = "Statistical power functions for different measures", background = "teal", solidHeader = TRUE,
                                                 uiOutput("z_slider"),
                                                 plotOutput("plot"), width = 10)),
                                    fluidRow(valueBoxOutput("n", width = 10)),
                                    fluidRow(valueBoxOutput("savings", width = 10)))
                                    ),
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
        v$selected <- p$z_selected[[v$z]]
        v$x_axis_label <- p$x_axis_label[[v$z]]
        v$box_z_var <- p$box_z_var[[v$z]]
    })
    
    output$z_slider <- renderUI({
        # sliderInput("z_value", paste("select", names(v$z)), min = min(v$z_choices),
        #             max = max(v$z_choices), 
        #             value = v$selected, animate = T
        #             ,step=diff(v$z_choice)
        #             ,round=FALSE
        #             )
        shinyWidgets::sliderTextInput(inputId = "z_value", 
                                      label = paste("select", names(v$z)), 
                                      choices = v$z_choices,
                                      selected = v$selected,
                                      animate=T)
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
                 color = "Measure",
                 x = v$x_axis_label) +
            #theme_hc(bgcolor = "darkunica") +
            scale_colour_hc("darkunica") + 
            ylim(0, 1) +
            theme(axis.text = element_text(colour = "black",size=12), 
                  legend.text=element_text(size=14), 
                  legend.position=c(0.8, 0.2)
                  #,plot.background = element_blank()
                  #,panel.grid.major = element_line(colour = "grey50"), 
                  ,panel.grid.major = element_line(colour = "grey70")
                  ,panel.grid.minor = element_line(colour = "grey70")
                  #,panel.border = element_blank()
                  ) +
            geom_hline(yintercept = 0.8,linetype = "dashed")
            #geom_vline(xintercept = pwr.t.test(n = NULL, d = input$d, sig.level = 0.05, 
            #                                  power=input$pwr, type="two.sample",
            #                                 alternative="two.sided")$n %>% as.integer(),
            #          colour = "white")
            
            
            #print(ggplotly(p, tooltip = c("n", "d", "pwr")))
            
            print(p)
            

    })
    output$n <- renderValueBox({
        dft_min<-30 #hard coding until we can work out how to pass correctly from above (which uses reactive values for z)
        acc_min<-50
        rts_min<-min(subset(powersim, condition=="reaction_time" & n==v$z & power>0.8)$effect_size)
        if(input$x == "n"){
            titletext<-"Participants required"
        }else{
            titletext<-"Detectable effect size"
        }
        titletext<-"!!This value should update" #delete this line when I figure it out
        infoBox(title=paste(titletext,v$selected),
                subtitle = "... for 80% power", color  = "teal", 
                 value = HTML(paste(dft_min,"measuring Drift", br(), acc_min, "measuring Accuracy",br(),rts_min, "measuring Reaction Time")),
                 icon = icon("users")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

