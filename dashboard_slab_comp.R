# erase everything from memory
# rm(list=ls())

# RStudio set wd to current folder
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()

# --- LOAD ALL THE REQUIRED LIBRARIES --- #
library(shiny)          #shiny allows tranlation of R script into HTML
library(pracma)         #library for bi-linear interpolation fucntion
library(shinydashboard, warn.conflicts = FALSE) #shiny dashboard builder
library(readxl)          # for reading excel files
library(ggplot2)         # plotting donut charts
library(dplyr)           # enables %>% functino
options(warn=-1) # turn warnings OFF, to turn them on type options(warn=0)



# -- IMPORTING ALL THE NECESSARY FUNCTIONS CREATED IN ANOTHER FILE --- #
source("functions_slab_comp.R")

# --- SOME STYLING PARAMTERS --- #
css_style_head = "font-size: 20px; color: white" #font-weight: bold
choice_label_rc <- "RC load-span table input type"
choice_label_hollowcore <- "Hollowcore load-span table input type"
choice_label_clt <- "CLT load-span table input type"
ss_ow_sb_text = "single span one way slab"
ms_ow_sb_text = "multiple span one way slab"
ms_flt_sb_text = "multiple span flat slab"

#value boxes names
depth_vb_name = "mm (total depth)"
co2_vb_name = "kgCO2e/m^2"
mass_vb_name = "kg/m^2"

#color scheme for the donut charts
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")


# --- UI PART OF THE SCRIPT --- #
ui <- dashboardPage(skin = "blue", # yellow color of the header
  dashboardHeader(title = "Structural Slab Material Comparison", titleWidth = 350
                  ),  # title width and name
                       

    dashboardSidebar(width = 350,
      tags$style(HTML(".sidebar-menu li a { font-size: 20px; }",
                      " .skin-blue .sidebar-menu > li.active > a {
                        border-left-color: #ff0000;
                      }")),
      #adds scroll bar only for the sidebar panel
      tags$head(tags$style(HTML(".sidebar {height: 100vh; overflow-y: auto;}"
               ) # close HTML       
        ) # close tags$style
        ), 
      sidebarMenu(
        radioButtons(inputId="slab_type", label="RC slab type", choices=c(ss_ow_sb_text, ms_ow_sb_text, ms_flt_sb_text), 
                     selected = ss_ow_sb_text),
        
        #------------------ author name and link to source code ------------------
        h5(withMathJax("$$$$"), align = "left"), # create a gap between the formula and what's below
        h5(HTML("&nbsp;"), align = "left"),
        tags$div(class="Header", checked=NA,
                 tags$a(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Author: Iurie Tarlev"))
                  ),
        tags$div(class="Header", checked=NA,
                 tags$a(href="https://github.com/iurietarlev/StructuralSlabOptionsComparison", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Click here to access source code"))
        ))
      ),
      
    
  dashboardBody(
    fluidRow(
      box(title = "RC Slab", width = 4, collapsible = TRUE,
          sliderInput(inputId = "il_rc", label = "Imposed Loading (kN/m^2)", value = 4, min = 2.5, max = 7.5, step = 0.1),  
          numericInput(inputId = "sdl_rc", label = "Superimposed Dead Loading (kN/m^2)", value = 1.5, min = 1.5, max = 4, step = 0.1),
          numericInput(inputId = "span_rc", label = "Span (m)", value = 6.9, min = 4, max = 10, step = 0.1),
          helpText("Depth of slab (mm)"),
          verbatimTextOutput(outputId = "depth", placeholder = T),
      
          helpText("Reinforcement quantity (kg/m^2  |  kg/m^3)"),
          verbatimTextOutput(outputId = "reinf_qt", placeholder = T),
          numericInput(inputId = "rc_concrete_density", label = "Concrete Density (kg/m^3)", value = 2500, min = 2000, max = 3000, step = 50),
          numericInput(inputId = "rebar_density", label = "Reinforcement Density (kg/m^3)", value = 7800, min = 7000, max = 10000, step = 50),
          numericInput(inputId = "rc_concrete_co2", label = "Concrete Embodied Carbon (kgCO2/kg)", value = 0.149, min = 0, max = 5, step = 0.001),
          numericInput(inputId = "rc_reinf_co2", label = "Rebar Embodied Carbon (kgCO2/kg)", value = 0.95, min = 0, max = 5, step = 0.001)),

      box(title = "Hollowcore Slab", width = 4, collapsible = TRUE,
          sliderInput(inputId = "il_hollowcore", label = "Imposed Loading (kN/m^2)", value = 4, min = 2.5, max = 7.5, step = 0.1),
          helpText("Superimposed Dead Loading (kN/m^2)"),
          verbatimTextOutput(outputId = "sdl_hollowcore", placeholder = T),
          helpText("Span (m)"),
          verbatimTextOutput(outputId = "span_hollowcore", placeholder = T),
          selectInput(inputId = "depth_hollowcore", label = "Depth of slab (mm)", selected = 150, choices = c(150,200,250,260,300,350,400,450)),
          numericInput(inputId = "topping_thickness", label = "Topping thickness (mm)", value = 50, min = 25, max = 150, step = 5),
          numericInput(inputId = "rods_qt", label = "Number of 12.5mm diameter tension rods:", value = 12, min = 2, max = 100, step = 2),
          numericInput(inputId = "topping_concrete_density", label = "Topping Concrete Density (kg/m^3)", value = 2500, min = 2000, max = 3000, step = 50),
          numericInput(inputId = "rods_density", label = "Tension Rods Density (kg/m^3)", value = 7800, min = 7000, max = 10000, step = 50),
          numericInput(inputId = "hc_precast_concrete_co2", label = "Precast Concrete Embodied Carbon (kgCO2/kg)", value = 0.172, min = 0, max = 5, step = 0.001),
          numericInput(inputId = "hc_topping_concrete_co2", label = "Topping Concrete Embodied Carbon (kgCO2/kg)", value = 0.149, min = 0, max = 5, step = 0.001),
          numericInput(inputId = "hc_rods_co2", label = "Tension Rods Embodied Carbon (kgCO2/kg)", value = 2.27, min = 0, max = 5, step = 0.001)),
      
      box(title = "CLT SLab",width = 4, collapsible = TRUE,
          sliderInput(inputId = "il_clt", label = "Imposed Loading (kN/m^2)", value = 4, min = 2.5, max = 7.5, step = 0.1),
          helpText("Superimposed Dead Loading (kN/m^2)"),
          verbatimTextOutput(outputId = "sdl_clt", placeholder = T),
          helpText("Span (m)"),
          verbatimTextOutput(outputId = "span_clt", placeholder = T),
          selectInput(inputId = "depth_clt", label = "Depth of slab (mm)", selected = 240, choices = c(60,100,160,200,240)),
          numericInput(inputId = "clt_density", label = "CLT Density (kg/m^3)", value = 470, min = 250, max = 900, step = 10),
          numericInput(inputId = "clt_co2", label = "CLT Embodied Carbon (kgCO2/kg)", value = 0.437, min = 0, max = 5, step = 0.001)
          
          
    )),
    fluidRow(
      box(title = "RC Slab - Mass Distribution", width = 4, collapsible = TRUE,
          plotOutput("rc_mass_donut")),
      box(title = "Hollowcore Slab - Mass Distribution", width = 4, collapsible = TRUE,
          plotOutput("hc_mass_donut")),
      box(title = "CLT - Mass Distribution", width = 4, collapsible = TRUE,
          plotOutput("clt_mass_donut"))),
    
    fluidRow(
      valueBoxOutput("rc_mass_value", width = 4),
      valueBoxOutput("hc_mass_value", width = 4),
      valueBoxOutput("clt_mass_value", width = 4)),
    
    
    fluidRow(
      box(title = "RC - Embodied Carbon Distribution", width = 4, collapsible = TRUE,
      plotOutput("rc_donut")),
      box(title = "Hollowcore Slab - Embodied Carbon Distribution", width = 4, collapsible = TRUE,
        plotOutput("hc_donut")),
      box(title = "CLT - Embodied Carbon Distribution", width = 4, collapsible = TRUE,
          plotOutput("clt_donut"))),
    
    fluidRow(
      valueBoxOutput("rc_co2_value", width = 4),
      valueBoxOutput("hc_co2_value", width = 4),
      valueBoxOutput("clt_co2_value", width = 4)),
    
    fluidRow(
      valueBoxOutput("rc_depth_value", width = 4),
      valueBoxOutput("hc_depth_value", width = 4),
      valueBoxOutput("clt_depth_value", width = 4))
    
  ))




#SERVER PART OF THE SCRIPT
server <- function(input, output, session) {
  
  # -------------------------------------------------------------------------------------------------------
  # ------------------ CALCULATION OF DEPFTH FOR RC AND SPAN FOR HC&CLT FROM LOAD SPAN TABLES -------------
  # -------------------------------------------------------------------------------------------------------
  
  # >> --- RC --- <<
  
  rc_depth_reinf <- reactive({
    
    #calculate the RC Slab depth, based on the IL, SDL and SPAN
    sdl_to_il <- readRDS(file = "./sdl_to_il_table.rds" )
    il_corrected <- get_il(sdl_to_il, sdll =input$sdl_rc, ill = input$il_rc)
    
    #select the relevant load-span table
    if (isTRUE(input$slab_type == ss_ow_sb_text)){
      df <- readRDS(file = "./rc_single_span_one_way_slab_tables.rds" )
      }
    else if (isTRUE(input$slab_type == ms_ow_sb_text)){
      df <- readRDS(file = "./rc_multiple_span_one_way_slab_tables.rds" )
      } 
    else { 
      df <- readRDS(file = "./rc_multiple_span_flat_slab_tables.rds" )
      }
    
    rc_depth <- get_depth(df,  span_length = input$span_rc, imposed_loading = il_corrected)
    reinf_pm2 <- get_reinf_pm2(df,  span_length = input$span_rc , imposed_loading = il_corrected)
    reinf_pm3 <- get_reinf_pm3(df,  span_length = input$span_rc , imposed_loading = il_corrected)
    
    
    values <- list(a = rc_depth, b = reinf_pm2, c = reinf_pm3)
    values
    
  })
  
  #render the result for depth of the rc slab
  output$depth = renderText({
    #req(rc_depth_reinf())
    values <- rc_depth_reinf()
    round(values$a)
  })
  
  #render the result for the reinforcement quantities for rc slab
  output$reinf_qt <- renderText({
    values <- rc_depth_reinf()
    reinf_pm2 <- values$b
    reinf_pm3 <- values$c
    paste(round(reinf_pm2), "|", round(reinf_pm3))})
  
  
  # >> --- HOLLOWCORE --- <<
  
  # hollowcore span length, based on the IL and depth
  output$span_hollowcore = renderText({
    df <- readRDS(file = "./hollowcore_table.rds" )
    hc_span <- get_span(df, depth_slab = as.numeric(input$depth_hollowcore), imposed_loading = as.numeric(input$il_hollowcore))
    hc_span
    
  })
  
  output$sdl_hollowcore = renderText("1.5")
  
  
  # >> --- CLT --- <<
  
  # clt span lenngth, based on the IL and depth
  clt__span <- reactive({
    df <- readRDS(file = "./clt_table.rds" )
    clt_span <- get_span(df, depth_slab = as.numeric(input$depth_clt), imposed_loading = as.numeric(input$il_clt))
    clt_span
  })
  
  #render the value for the span of CLT
  output$span_clt = renderText({
    clt__span()
  })
  
  #render the SDL constant load for CLT
  output$sdl_clt = renderText("1.0")
  
  
  # --------------------------------------------------------------------------
  # ----------------- LISTS OF DEPTH, MASS AND CO2 VALUES --------------------
  # --------------------------------------------------------------------------
  
  all_depth_mass_co2 <- reactive({
    
    # -------- rc depth, mass & co2 values --------
    rc_depth_reinf <- rc_depth_reinf()
    rc_depth <- rc_depth_reinf$a
    rc_reinf_pm2 <- rc_depth_reinf$b
    
    rc_concrete_mass_pm2 <- (((1*1*rc_depth/1000)*input$rc_concrete_density) - (rc_reinf_pm2/input$rebar_density*input$rc_concrete_density))
    rc_total_mass <- rc_concrete_mass_pm2 + rc_reinf_pm2
    
    rc_concrete_co2_pm2 <- rc_concrete_mass_pm2*input$rc_concrete_co2
    rc_reinf_co2_pm2 <- input$rc_reinf_co2*rc_reinf_pm2
    
    rc_total_co2_pm2 <- rc_concrete_co2_pm2 + rc_reinf_co2_pm2
    
    
    
    # -------- hc depth, mass & co2 values --------
    A_one_rod <- 93/(1000^2) #area of a single 12.5mm diameter rod (from an FPMccan Calc from Matthew Caldwell) in m
    volume_rods_pm2 <- input$rods_qt*(A_one_rod)
    hc_rods_mass_pm2 <- volume_rods_pm2*input$rods_density
    #print(hc_rods_mass_pm2)
    
    
    sw_thck_df <- data.frame("depth" = c(150,200,250,260,300,350,400,450), 
                             "selfweight_kN_pm2" = c(2.36,2.98,3.62,3.47,3.99,4.53,5.15,5.46)) #from FPMccan hollowcore tables
    mask = sw_thck_df$depth == input$depth_hollowcore
    df_selection <- sw_thck_df[which(mask), ]
    
    hc_total_mass_pm2 <- (as.numeric(df_selection$selfweight_kN_pm2)*1000/9.81) + 
      ((input$topping_thickness)/1000)*(input$topping_concrete_density)# total hollowcore mass + topping 
    hc_concrete_mass_pm2 <- hc_total_mass_pm2 - hc_rods_mass_pm2 # concrete mass for hollowcore

    
    
    hc_rods_co2_pm2 <- input$hc_rods_co2*hc_rods_mass_pm2
    hc_concrete_co2_pm2 <- (input$hc_precast_concrete_co2)*hc_concrete_mass_pm2
    hc_total_co2_pm2 <- hc_concrete_co2_pm2 + hc_rods_co2_pm2

    
    # -------- clt depth, mass & co2 values --------
    clt_depth <- as.numeric(input$depth_clt)
    clt_co2 <- as.numeric(input$clt_co2)
    clt_density <- as.numeric(input$clt_density)
    clt_co2_pm2 <- round((1*1*clt_depth/1000)*clt_co2*clt_density)
    mass_clt <- (1*1*clt_depth/1000)*clt_density
    

    values <- list(
                   #storing all rc slab variables
                   rc_depth = rc_depth , rc_conc_mass = rc_concrete_mass_pm2 , 
                   rc_reinf_mass = rc_reinf_pm2, rc_total_mass = rc_total_mass,
                   rc_conc_co2 = rc_concrete_co2_pm2, rc_reinf_co2 = rc_reinf_co2_pm2,
                   rc_total_co2 = rc_total_co2_pm2,
                   
                   #storing all hc slab variables
                   hc_depth = input$depth_hollowcore, hc_conc_mass = hc_concrete_mass_pm2 , 
                   hc_rods_mass = hc_rods_mass_pm2, hc_total_mass = hc_total_mass_pm2,
                   hc_conc_co2 = hc_concrete_co2_pm2, hc_rods_co2 = hc_rods_co2_pm2,
                   hc_total_co2 = hc_total_co2_pm2,
                   
                   #storing all clt slab variables
                   clt_depth = clt_depth, clt_total_mass = mass_clt, clt_total_co2 = clt_co2_pm2
                   
                   )
    values
  })
  
  
  conditional_colours <- reactive({
    
    #rc
    values_rc <- all_depth_mass_co2()
    rc_depth <- values_rc$rc_depth
    rc_mass_total <- values_rc$rc_total_mass
    rc_co2 <- values_rc$rc_total_co2
    
    #hc
    values_hc <- all_depth_mass_co2()
    hc_depth <- values_hc$hc_depth
    hc_mass_total <- values_hc$hc_total_mass
    hc_co2 <- values_hc$hc_total_co2
    
    #clt
    values_hc <- all_depth_mass_co2()
    clt_depth <- values_hc$clt_depth
    clt_mass_total <- values_hc$clt_total_mass
    clt_co2 <- values_hc$clt_total_co2
    
    #rc colours
    rc_depth_col <- color.picker(rc_depth, hc_depth, clt_depth)
    hc_depth_col <- color.picker(hc_depth, rc_depth, clt_depth)
    clt_depth_col <- color.picker(clt_depth, rc_depth, hc_depth)

    #hc colours
    rc_mass_col <- color.picker(rc_mass_total, hc_mass_total, clt_mass_total)
    hc_mass_col <- color.picker(hc_mass_total, rc_mass_total, clt_mass_total)
    clt_mass_col <- color.picker(clt_mass_total, rc_mass_total, hc_mass_total)
    
    #clt colours
    rc_co2_col <- color.picker(rc_co2,hc_co2,clt_co2)
    hc_co2_col <- color.picker(hc_co2,rc_co2,clt_co2)
    clt_co2_col <- color.picker(clt_co2,hc_co2,rc_co2)
    
    #store the colours in a list to be used by valueboxes
    values <- list(rc_depth_col, hc_depth_col, clt_depth_col, 
                   rc_mass_col, hc_mass_col, clt_mass_col,
                   rc_co2_col, hc_co2_col, clt_co2_col)
    values

  })
  
  
  # ----------------------------------------------------------------------------
  # -------------- DRAW DONUT CHARTS FOR CO2 MATERIAL DISTRIBUTION  ------------
  # ----------------------------------------------------------------------------
  
  # RC donut chart
  output$rc_donut <- renderPlot({
    values <- all_depth_mass_co2()
    mass_c <- values$rc_conc_mass
    mass_reinf <- values$rc_reinf_mass
    rc_concrete_co2_pm2 <- values$rc_conc_co2
    rc_reinf_co2_pm2 <- values$rc_reinf_co2
    
    
    df <- data.frame(material = c("concrete","reinforcement"), mass = c(mass_c, mass_reinf), co2 = c(rc_concrete_co2_pm2,rc_reinf_co2_pm2))
    
    df <- df %>% # add label position
      arrange(desc(material)) %>%
      mutate(lab.ypos_co2 = cumsum(co2) - 0.5*co2)
    
    mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
    p <- ggplot(df, aes(x=2, y = co2, fill = material)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0) +
      geom_text(aes(y = lab.ypos_co2, label = (paste0(round(co2/sum(co2)*100),"%"))),size = 8, color = "white")+
      scale_fill_manual(values = mycols) +
      theme_void()+
      xlim(0.5, 2.5)
    
    p + theme(legend.title=element_text(size=14,face="bold"), 
              legend.text=element_text(size=13)
              )
  })
  
  # hollowcore donut chart
  output$hc_donut <- renderPlot({
    values <- all_depth_mass_co2()
    rods_mass <- values$hc_rods_mass
    hc_mass <- values$hc_conc_mass 
    rods_co2 <- values$hc_rods_co2
    hc_co2 <- values$hc_conc_co2
    
    df <- data.frame(material = c("concrete","tension rods"), mass = c(hc_mass, rods_mass), co2 = c(hc_co2,rods_co2))
    
    df <- df %>% # add label position
      arrange(desc(material)) %>%
      mutate(lab.ypos_co2 = cumsum(co2) - 0.5*co2)
    
    
    mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
    p <- ggplot(df, aes(x=2, y = co2, fill = material)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0) +
      geom_text(aes(y = lab.ypos_co2, label = (paste0(round(co2/sum(co2)*100),"%"))),size = 8, color = "white")+
      scale_fill_manual(values = mycols) +
      theme_void()+
      xlim(0.5, 2.5)
    
    p + theme(legend.title=element_text(size=14,face="bold"), 
              legend.text=element_text(size=13))
    })
    
  # clt donut chart
  output$clt_donut <- renderPlot({
      depth_ <- as.numeric(input$depth_clt)
      co2_<- as.numeric(input$clt_co2)
      density_ <- as.numeric(input$clt_density)
      clt_total_co2 <- round((1*1*depth_/1000)*co2_*density_)
      
      clt_mass <- round((1*1*depth_/1000)*density_)

      
      
      df <- data.frame(material = "CLT", mass = clt_mass, co2 = clt_total_co2)
      
      df <- df %>% # Add label position
        arrange(desc(material)) %>%
        mutate(lab.ypos_co2 = cumsum(co2) - 0.5*co2)
      
      
      mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
      p <- ggplot(df, aes(x=2, y = co2, fill = material)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0) +
        geom_text(aes(y = lab.ypos_co2, label = (paste0(round(co2/sum(co2)*100),"%"))),size = 8, color = "white")+
        scale_fill_manual(values = mycols) +
        theme_void()+
        xlim(0.5, 2.5)
      
      p + theme(legend.title=element_text(size=14,face="bold"), 
                legend.text=element_text(size=13)
      )
    
    
    
  })
  
  # -------------------------------------------------------------------------------
  #---------------- DONUTS FOR MASS DISTRIBUTION ----------------------------------
  # -------------------------------------------------------------------------------
  
  # RC donut chart
  output$rc_mass_donut <- renderPlot({
    values <- all_depth_mass_co2()
    mass_c <- values$rc_conc_mass
    mass_reinf <- values$rc_reinf_mass
    rc_concrete_co2_pm2 <- values$rc_conc_co2
    rc_reinf_co2_pm2 <- values$rc_reinf_co2
    
    
    df <- data.frame(material = c("concrete","reinforcement"), mass = c(mass_c, mass_reinf), co2 = c(rc_concrete_co2_pm2,rc_reinf_co2_pm2))
    
    df <- df %>% # add label position
      arrange(desc(material)) %>%
      mutate(lab.ypos_mass = cumsum(mass) - 0.5*mass)
    

    p <- ggplot(df, aes(x=2, y = mass, fill = material)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0) +
      geom_text(aes(y = lab.ypos_mass, label = (paste0(round(mass/sum(mass)*100),"%"))),size = 8, color = "white")+
      scale_fill_manual(values = mycols) +
      theme_void()+
      xlim(0.5, 2.5)
    
    p + theme(legend.title=element_text(size=14,face="bold"), 
              legend.text=element_text(size=13)
    )
  })
  
  
  # hollowcore donut chart
  output$hc_mass_donut <- renderPlot({
    values <- all_depth_mass_co2()
    
    rods_mass <- values$hc_rods_mass
    hc_mass <- values$hc_conc_mass 
    rods_co2 <- values$hc_rods_co2
    hc_co2 <- values$hc_conc_co2
    
    df <- data.frame(material = c("concrete","tension rods"), mass = c(hc_mass, rods_mass), co2 = c(hc_co2,rods_co2))
    
    df <- df %>% # add label position
      arrange(desc(material)) %>%
      mutate(lab.ypos_mass = cumsum(mass) - 0.5*mass)
    
    

    p <- ggplot(df, aes(x=2, y = mass, fill = material)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0) +
      geom_text(aes(y = lab.ypos_mass, label = (paste0(round(mass/sum(mass)*100),"%"))),size = 8, color = "white")+
      scale_fill_manual(values = mycols) +
      theme_void()+
      xlim(0.5, 2.5)
    
    p + theme(legend.title=element_text(size=14,face="bold"), 
              legend.text=element_text(size=13))
  })
  
  # clt donut chart
  output$clt_mass_donut <- renderPlot({
    depth_ <- as.numeric(input$depth_clt)
    co2_<- as.numeric(input$clt_co2)
    density_ <- as.numeric(input$clt_density)
    clt_total_co2 <- round((1*1*depth_/1000)*co2_*density_)
    
    clt_mass <- round((1*1*depth_/1000)*density_)
    
    
    
    df <- data.frame(material = "CLT", mass = clt_mass, co2 = clt_total_co2)
    
    df <- df %>% # Add label position
      arrange(desc(material)) %>%
      mutate(lab.ypos_co2 = cumsum(mass) - 0.5*mass)
    
    
    p <- ggplot(df, aes(x=2, y = mass, fill = material)) +
      geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0) +
      geom_text(aes(y = lab.ypos_co2, label = (paste0(round(mass/sum(mass)*100),"%"))),size = 8, color = "white")+
      scale_fill_manual(values = mycols) +
      theme_void()+
      xlim(0.5, 2.5)
    
    p + theme(legend.title=element_text(size=14,face="bold"), 
              legend.text=element_text(size=13)
    )
    
    
    
  })
  
  
  
  # -----------------------------------------------------------------------------
  # ------------- RENDER VALUE BOXES WITH TOTAL DEPTH VALUES --------------------
  # -----------------------------------------------------------------------------
  
  # RC depth value box
  output$rc_depth_value <- renderValueBox({
    values <- all_depth_mass_co2()
    rc_depth <- round(values$rc_depth)
    
    col_values <- conditional_colours()
    rc_depth_col <- col_values[[1]]
    
    valueBox(
      paste(rc_depth), depth_vb_name, icon = icon("ffas fa-bars", lib = "font-awesome"),
      color = rc_depth_col)
  })
  
  # HC depth value box
  output$hc_depth_value <- renderValueBox({
    hc_depth <- as.numeric(input$depth_hollowcore) + as.numeric(input$topping_thickness)
    
    col_values <- conditional_colours()
    hc_depth_col <- col_values[[2]]
    
    valueBox(
      paste(hc_depth), depth_vb_name, icon = icon("ffas fa-bars", lib = "font-awesome"),
      color = hc_depth_col)
  })
  
  # HC depth value box
  output$clt_depth_value <- renderValueBox({
    clt_depth <- as.numeric(input$depth_clt)
    
    col_values <- conditional_colours()
    clt_depth_col <- col_values[[3]]
    
    valueBox(
      paste(clt_depth), depth_vb_name, icon = icon("ffas fa-bars", lib = "font-awesome"),
      color = clt_depth_col)
  })
  
  # -----------------------------------------------------------------------------
  # ------------- RENDER VALUE BOXES WITH TOTAL MASS VALUES --------------------
  # -----------------------------------------------------------------------------
  
  # RC depth value box
  output$rc_mass_value <- renderValueBox({
    values_rc <- all_depth_mass_co2()
    rc_mass_total <- round(values_rc$rc_total_mass)
    
    col_values <- conditional_colours()
    rc_mass_col <- col_values[[4]]
    
    valueBox(
      paste(rc_mass_total), mass_vb_name, icon = icon("ffas fa-layer-group", lib = "font-awesome"),
      color = rc_mass_col)
  })
  
  # HC depth value box
  output$hc_mass_value <- renderValueBox({
    values <- all_depth_mass_co2()
    hc_total_mass <- round(values$hc_total_mass)
    
    col_values <- conditional_colours()
    hc_mass_col <- col_values[[5]]
    
    valueBox(
      paste(hc_total_mass), mass_vb_name, icon = icon("ffas fa-layer-group", lib = "font-awesome"),
      color = hc_mass_col)
  })
  
  # HC depth value box
  output$clt_mass_value <- renderValueBox({
    values <- all_depth_mass_co2()
    clt_total_mass <- round(values$clt_total_mass)
    
    
    col_values <- conditional_colours()
    clt_mass_col <- col_values[[6]]
    
    valueBox(
      paste(clt_total_mass), mass_vb_name, icon = icon("ffas fa-layer-group", lib = "font-awesome"),
      color = clt_mass_col)
  })
  
  
  # ---------------------------------------------------------------------------
  # ------------- RENDER VALUE BOXES WITH TOTAL CO2 VALUES --------------------
  # ---------------------------------------------------------------------------
  
  # RC co2 value box
  output$rc_co2_value <- renderValueBox({
    values <- all_depth_mass_co2()
    rc_total_co2 <- round(values$rc_total_co2)
    
    col_values <- conditional_colours()
    rc_co2_col <- col_values[[7]]
    
    valueBox(
      paste(rc_total_co2), co2_vb_name, icon = icon("ffas fa-leaf", lib = "font-awesome"),
      color = rc_co2_col)
  })
  
  # hollowcore co2 value box
  output$hc_co2_value <- renderValueBox({
    values <- all_depth_mass_co2()
    hc_total_co2 <- round(values$hc_total_co2)
    
    
    col_values <- conditional_colours()
    hc_co2_col <- col_values[[8]]
    
    valueBox(
      paste(hc_total_co2), co2_vb_name, icon = icon("ffas fa-leaf", lib = "font-awesome"),
      color = hc_co2_col)
  })
  
  # clt co2 value box
  output$clt_co2_value <- renderValueBox({
    values <- all_depth_mass_co2()
    clt_total_co2 <- round(values$clt_total_co2)
    
    col_values <- conditional_colours()
    clt_co2_col <- col_values[[9]]
    
    valueBox(
      paste(clt_total_co2), co2_vb_name, icon = icon("ffas fa-leaf", lib = "font-awesome"),
      color = clt_co2_col)
  })
}

# -------- > RUN THE APPLICATION < ---------- 
shinyApp(ui = ui, server = server)

