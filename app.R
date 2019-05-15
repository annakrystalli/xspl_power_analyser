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
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

load("data/powersim.rda")

# define slider css (see: https://stackoverflow.com/questions/47116236/change-colour-for-sliderinput)
mycss <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: #38CBCC;
  border-color: #38CBCC;
}
.irs-single {
  color: black;
  font-weight: bold;
  font-size: 14px;
}
.irs-grid {
  font-weight: bold;
  font-size: 12px;
}
.input {
  background: red;
}
a {
    color: #38CBCC;
}
.skin-blue .main-header .navbar, 
.skin-blue .main-header .logo{
  background: #38CBCC;
  color: #222d32;
}

@media (max-width: 767px){
  .main-header {
  position: absolute;
  top: 0;
  width: 100%;
  max-height: 150px;
  }
  
  .content-wrapper{
  padding-top: 150px; 
  }

  .main-header .logo {
  height: 100px;
  line-height:45px; 
  }

}

@media (min-width: 768px){
  .main-header {
  position: absolute;
  top: 0;
  width: 100%;
  }
  
  .content-wrapper{
  padding-top: 50px; 
  }
}
"

p <- list(y = "power",
          x_choices = setNames(c("effect_size", "n"), c("true effect size", "sample size per group")),
          z_choices = purrr::map(c("effect_size", "n"), ~ sort(unique(powersim[[.x]]))) %>% 
            setNames(c("effect_size", "n")),
          z_selected = setNames(c(2, 40), c("effect_size", "n")),
          x_axis_label = setNames(c("sample size per group", "true effect in drift"), 
                                  c("effect_size", "n")),
          box_z_var = setNames(c("effect size", "sample size"), c("effect_size", "n"))
)

### function to detect mobile #### approach source: https://github.com/g3rv4/mobileDetect
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}



# Define UI for application that draws a histogram
ui <- tagList(
  mobileDetect('isMobile'),
  dashboardPage(skin = "blue", title = "Enhanced sensitivity to group differences with decision modelling",
                dashboardHeader(title = strong("Enhanced sensitivity to group differences with decision modelling"),
                                titleWidth = 800),
                dashboardSidebar(width = 350,
                                 sidebarMenu(
                                   menuItem("Dashboard", tabName = "dashboard"),
                                   menuItem("Raw data", tabName = "rawdata")
                                 ),
                                 gradientBox(
                                   title = "About",
                                   closable = FALSE,
                                   collapsible = FALSE,
                                   boxToolSize = "lg",
                                   width=12,
                                   p("We simulate accuracy and reaction times for known differences between two groups. These graphs show the sensitivity (statistical power) across different sample sizes (for a known effect size), or for different effect sizes (for a known sample size). The three lines show how the sensitivity varies if you test for a difference using (just) accuracy, (just) reaction time or if you combine them using decision modelling to recover a drift pararameter",
                                     style="color:black")
                                 ),
                                 gradientBox(
                                   title = "Pre-print",
                                   closable = FALSE,
                                   collapsible = FALSE,
                                   boxToolSize = "lg",
                                   width=12,
                                   p("PDF: ", style="color:black"),
                                   em(a("Quantifying the benefits of using decision models with response time
                            and accuracy data", href="http://psyarxiv.com/",style="color:red;font-weight:bold")),
                                   p("")
                                 ),
                                 div(
                                   id = "logo-tuos",
                                   class = "col-sm-12",
                                   img(src = "tuos_rev_logo.png",
                                       width = "100%")
                                 )
                ),
                dashboardBody(
                  tabItems(
                    tabItem("dashboard",
                            box(title = strong("Statistical power functions for different measures"), background = "black", 
                                style="background:#222D31",
                                tags$style(mycss),
                                fluidRow(column(3, radioButtons("x", 
                                                                h4(strong("select x-axis variable")), 
                                                                choices = p$x_choices,
                                                                selected = p$x_choices["sample size per group"])),
                                         column(8, uiOutput("z_slider"))), 
                                width = 10),
                            box(title = "", background = "teal", solidHeader = TRUE,
                                plotOutput("plot"), width = 10),
                            fluidRow(valueBoxOutput("n", width = 10))),
                    tabItem("rawdata",
                            DT::dataTableOutput("data"))
                  )
                  
                )
  ),#end dashboardPage
  tags$footer(tags$p(a(href = "https://github.com/annakrystalli/xspl_power_analyser/blob/master/LICENSE", 
                       "MIT"), 
                     " Copyright ", icon(name = "cr", class = "fa-copyright")," 2019",
                     em(" Anna Krystalli"), 
                     tags$a(href="https://twitter.com/annakrystalli",
                            icon(name = "ak-twitter", class = "fa-twitter")),
                     tags$a(href="https://github.com/annakrystalli",
                            icon(name = "ak-gh", class = "fa-github-alt")),
                     " & ", em("Tom Stafford"), 
                     tags$a(href="https://twitter.com/tomstafford",
                            icon(name = "ts-twitter", class = "fa-twitter")),
                     tags$a(href="https://github.com/tomstafford",
                            icon(name = "ts-gh", class = "fa-github-alt")),
                     " - DOI:", 
                     tags$a(href="10.15131/shef.data.8109161", "10.15131/shef.data.8109161"),
                     tags$a(href="https://github.com/annakrystalli/xspl_power_analyser",
                            icon(name = "sc-gh", class = "fa-github"))), 
              align = "center", style = "
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: #222D31;
              z-index: 1000;")
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  # create reactive values vector
  v <- reactiveValues(
    z = "effect_size",
    z_choices = p$z_choices[["effect_size"]],
    z_value = 2
  )
  
  # ---- Define reactive functionality ----
  # get z variable in response to selected x
  get_z <- reactive({
    p$x_choices[input$x  != p$x_choices]
  })
  
  # subset data according to z value 
  subset_dat <- reactive({
    powersim %>%  
      filter((!!rlang::sym(v$z)) == v$z_value)
  })
  
  # update settings in respons to change in x
  observeEvent(input$x, {
    v$z <- get_z()
    v$z_choices <- p$z_choices[[v$z]]
    v$selected <- p$z_selected[[v$z]]
    v$x_axis_label <- p$x_axis_label[[v$z]]
    v$box_z_var <- p$box_z_var[[v$z]]
  })
  
  # update settings in respons to change in z_value
  observeEvent(input$z_value, {
    v$z_value <- input$z_value
    v$data <- subset_dat()
  })
  
  # plot in response to change in data subset
  plot_data <- eventReactive(v$data,{
    
    p <- v$data %>%
      ggplot2::ggplot(aes_(as.name(input$x), as.name(p$y),
                           colour = as.name("condition"))) + 
      geom_point(alpha = 1) + geom_line(alpha = 1, size = 2) +
      labs(title ="",
           subtitle = "",
           color = "Measure",
           x = v$x_axis_label)  +
      theme_linedraw() +
      scale_colour_hc("darkunica") + 
      ylim(0, 1) +
      theme(axis.text = element_text(colour = "black",size=12), 
            legend.text=element_text(size=14)) +
      geom_hline(yintercept = 0.8,linetype = "dashed")
    
    if(input$isMobile){
      p <- p + theme(legend.position="bottom", legend.box = "horizontal")
    }
    print(p)
  })
  
  # get 0.8 power approximation in response to change in data subset
  get_approx_80 <- eventReactive({v$data},{ 
    v$approx_80 <- v$data %>%
      split(.$condition) %>%
      purrr::map_df(~ approx(y = .x[[input$x]], 
                             x = .x$power, xout = 0.8) %>% .$y)
    
    if(input$x == "effect_size"){ #user selection of x-axis variable
      v$titletext<-"Minimal detectable true effect size"
      v$approx_80 <- round(v$approx_80, 1)
      v$icon <- icon("search-minus")
    }else{
      v$titletext<-"Total participants required"
      v$approx_80 <- round(v$approx_80, 0) * 2
      v$icon <- icon("users")
    }
  })
  
  # ---- Define outputs ----   
  output$z_slider <- renderUI({
    
    shinyWidgets::sliderTextInput(inputId = "z_value", 
                                  label = h4(strong(paste("select", names(v$z)))), 
                                  choices = v$z_choices,
                                  selected = v$selected,
                                  grid = T, 
                                  animate=T)
  })
  
  output$plot <- renderPlot({
    shiny::req(v$data)
    plot_data()
  })
  
  output$n <- renderValueBox({
    shiny::req(input$x)
    shiny::req(v$z)
    shiny::req(v$z_value)
    
    get_approx_80()
    
    infoBox(title = paste(v$titletext),
            subtitle = "... for 80% power", color  = "teal", 
            value = HTML(paste(v$approx_80$drift, em("measuring Drift"), br(), 
                               v$approx_80$accuracy, em("measuring Accuracy"),br(),
                               v$approx_80$`reaction time`, em("measuring Reaction Time"))),
            icon = v$icon
    )
  })
  
  output$data <- DT::renderDataTable(powersim, width = "80%")
}

# Run the application 
shinyApp(ui = ui, server = server)

