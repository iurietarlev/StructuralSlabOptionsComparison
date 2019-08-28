# rm(list=ls())
# 
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

library(shiny)          #shiny allows tranlation of R script into HTML
library(pracma)         #library for bi-linear interpolation fucntion
library(shinydashboard, warn.conflicts = FALSE) #shiny dashboard builder
#library(plotly)         #for plotting interactive plots
library(readxl)         #library for reading excel files


#IMPORTING ALL THE NECESSARY FUNCTIONS CREATED IN ANOTHER FILE
source("Functions_str_elements.R")


#SOME STYLING PARAMTERS
css_style_head = "font-size: 20px; color: white" #font-weight: bold
choice_label <- "select input type"
choice_excel <- "Excel file"
choice_m_sd <- "Mean and Standard Deviation"
choice_const <- "Constant"
default_values <- "Default values"

mean_label <- "Mean"
sd_label <- "Standard Deviation"

tabs_panel_title <- "Probability Paper"
width_tab_box_prob_paper <- 12
#height_tab_box_prob_paper <- "350px"

distributions_list <- c("", "Uniform", "Normal", "Lognormal", "Gumbel")
sidebarpanel_width = 12



#UI PART OF THE SCRIPT
ui <- dashboardPage(skin = "yellow", # yellow color of the header
  dashboardHeader(title = "Structural Material Comparison", titleWidth = 350,
                  tags$li(div(img(src = 'BH LOGO.pdf',
                                  title = "Company Home", height = "40px"),
                              style = "padding-top:5px; padding-bottom:5px;
                              margin-right:5px;"),
                          class = "dropdown")),  # title width and name
    dashboardSidebar(width = 350,
      tags$style(HTML(".sidebar-menu li a { font-size: 20px; }",
                      " .skin-blue .sidebar-menu > li.active > a {
                        border-left-color: #ff0000;
                      }")),
      #adds scroll bar only for the sidebar panel
      tags$head(
        tags$style(HTML(".sidebar {
                        height: 100vh; overflow-y: auto;
                        }"
               ) # close HTML       
        )            # close tags$style
        ), 
      sidebarMenu(
        #----------------- number of iterations ----------------
        menuItem(startExpanded = TRUE, text = tags$p(style = css_style_head,"Slab comparison"), tabName = "slab_comparison",
                 radioButtons(inputId="slab_input_choice", label=choice_label, choices=c(default_values, choice_excel), 
                              selected = default_values),
                 uiOutput("slab_load_tables"), h5(HTML("&nbsp;"), align = "left")),
        
    
        
        #------------------ authors names and link to documentation ------------------
        h5(withMathJax("$$$$"), align = "left"), # create a gap between the formula and what's below
        h5(HTML("&nbsp;"), align = "left"),
        tags$div(class="Header", checked=NA,
                 tags$a(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Author: Iurie Tarlev"))
                  ),
        tags$div(class="Header", checked=NA,
                 tags$a(href="https://github.com/osk849/str_elements_comparison.git", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Click here to access the documentation"))
        ))
      ),
      
    
  dashboardBody(
    fluidRow(
      box(title = "RC Slab", width = 4,
          sliderInput(inputId = "il_rc", label = "Imposed Loading (kN/m^2)", value = 4, min = 2.5, max = 10, step = 0.1),  
          sliderInput(inputId = "sdl_rc", label = "Superimposed Dead Loading (kN/m^2)", value = 1.5, min = 1, max = 4, step = 0.1),
          numericInput(inputId = "span_rc", label = "Span (m)", value = 4, min = 4, max = 9.9, step = 0.1),
          helpText("Depth of slab (mm)"),
          verbatimTextOutput(outputId = "depth", placeholder = T)),

      box(title = "Hollowcore Slab", width = 4,
          sliderInput(inputId = "il_hollowcore", label = "Imposed Loading (kN/m^2)", value = 4, min = 0.75, max = 15, step = 0.1),
          helpText("Superimposed Dead Loading (kN/m^2)"),
          verbatimTextOutput(outputId = "sdl_hollowcore", placeholder = T),
          helpText("Span (m)"),
          verbatimTextOutput(outputId = "span_hollowcore", placeholder = T),
          selectInput(inputId = "depth_hollowcore", label = "Depth of slab (mm)", selected = 250, choices = c(150,200,250,260,300,350,400,450))),
      
      box(title = "CLT SLab",width = 4,
          sliderInput(inputId = "il_clt", label = "Imposed Loading (kN/m^2)", value = 4, min = 0.75, max = 15, step = 0.1),
          helpText("Superimposed Dead Loading (kN/m^2)"),
          verbatimTextOutput(outputId = "sdl_clt", placeholder = T),
          helpText("Span (m)"),
          verbatimTextOutput(outputId = "span_clt", placeholder = T),
          selectInput(inputId = "depth_clt", label = "Depth of slab (mm)", selected = 200, choices = c(60,100,160,200,240)))
    )
  ))
  
  
  


#SERVER PART OF THE SCRIPT
server <- function(input, output, session) {
  
  
  

  # ----- select the right load/span tables according to user's choice of excel or default values -------
  output$slab_load_tables = renderUI({
      input_type_selector_excel_dflt(input_choice = input$slab_input_choice, excel = choice_excel, dflt = default_values,
                                     rc_table_tag = "rc_table", hollowcore_table_tag = "hollowcore_table", clt_table_tag = "clt_table")
  })
  
  
  ##### -----------> RC SLAB CALCULATIONS AND RENDERING OUTPUT <--------------------######
  
  #calculate the RC Slab depth, based on the IL, SDL and SPAN
  rc_depth <- reactive({interp_bh(input$rc_table$datapath, x_i = input$span_rc, y_i = input$il_rc, method_i = "linear")})
  
  output$depth = renderText({req(input$rc_table)
                                  rc_depth()})
  
  
  
  ##### -----------> HOLLOWCORE SLAB CALCULATIONS AND RENDERING OUTPUT <--------------------######
  
  #calculate the Hollowcore span length, based on the IL, and depth
  hollowcore_span <- reactive({interp_bh(input$hollowcore_table$datapath, x_i = input$il_hollowcore, y_i = as.numeric(input$depth_hollowcore), method_i = "linear")})
  
  output$span_hollowcore = renderText({req(input$hollowcore_table)
    hollowcore_span()})
  
  output$sdl_hollowcore = renderText("1.5")
  
  
  ##### -----------> CLT SLAB CALCULATIONS AND RENDERING OUTPUT <--------------------######
  
  #calculate the Hollowcore span length, based on the IL, and depth
  clt_span <- reactive({interp_bh(input$clt_table$datapath, x_i = input$il_clt, y_i = as.numeric(input$depth_clt), method_i = "linear")})
  
  output$span_clt = renderText({req(input$clt_table)
    clt_span()})
  output$sdl_clt = renderText("1.0")
  
  
  
  
}

# -------- > RUN THE APPLICATION < ---------- 
shinyApp(ui = ui, server = server)

