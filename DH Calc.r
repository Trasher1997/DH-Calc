#Title ....... : DH Calc
#Author........: Marek Lubszczyk
#Version.......: ver.1.2
#Date..........: 04.2023

# Librarys
library(shiny)
library(DT)
library(shinyFiles)
library(matlib)
library(plotly)
library(rmarkdown)
library(shinythemes)
library(shinydashboard)
library(htmltools)
source("Information_text.R")

# Interface
ui <- fluidPage(
  includeCSS("style.css"),
  dashboardPage(

  #menu elements define
  dashboardHeader(title = "DH Calc"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Strona główna", tabName = "home", icon = icon("home"), selected = TRUE),
      menuItem("Obliczenia", tabName = "data", icon = icon("bar-chart"),
               menuSubItem("Tabela DH", tabName = "dh_table"),
               menuSubItem("Wykres 3D", tabName = "show_plot")
               #,menuSubItem("Orientacja członów", tabName = "orientation_parts")
      ),
      menuItem("Przydatne informacje", tabName = "usefoul_information", icon = icon("info"),
               menuSubItem("Kinematyka", tabName = "kinematic_task"),
               #menuSubItem("Dynamika", tabName = "dynamic_task"),
               menuSubItem("Objaśnienie tabeli DH", tabName = "dh_info")
      ),
      menuItem("O Autorze", tabName = "about_me", icon = icon("user"))
    )
  ),

  dashboardBody(
    
    #menu page define
    tabItems(
      tabItem(tabName = "home",
              fluidPage(
                div(class = "container-fluid",
                img(src = "https://mechatronikadlawszystkich.pl/imager/articles/84964/W1200_H675_P50-50.jpg", width = "105%", height = "auto"),
                h4("Kalkulator Denavita-Hartenberga")
                )
              )
      ),
      tabItem(tabName = "dh_table",
              fluidPage(
                  h1("Tabela Denavita-Hartenberga"),
                  DTOutput("table"), br(), br(),
                  actionButton("add_row", "Dodaj wiersz", class = "button"),
                  actionButton("calculate", "Oblicz", class = "button"),
                  checkboxInput("only_last_result", 
                                label = tags$span("Wyświetl tylko pozycje ostatniego członu", class ="last-value"), 
                                value = FALSE)
              ),
              fluidPage(
                  br(),br(),
                  conditionalPanel(
                    condition = "input.calculate",
                    h3("Współrzędne końca członu (punkt TCP)")
                  ),
                  verbatimTextOutput("result"),
                  br()
              )
      ),
      tabItem(tabName = "show_plot",
              fluidPage(
                  style = "height: 1000px",
                  h1("Wykres 3D"),
                  conditionalPanel(
                    condition = "!input.calculate",
                    h3("Brak danych do wyświetlenia wykresu. Proszę wprowadzić dane do tabeli i nacisnąć przycisk Oblicz.")
                  ),
                  plotlyOutput("plot")
              )
      ),
      tabItem(tabName = "orientation_parts",
              fluidPage(
                  h1("Strona w budowie")
              )
      ),
      tabItem(tabName = "kinematic_task",
              fluidPage(
                  h1("Co to jest kinematyka?"),
                  textOutput("kinematic"), 
                  br(), br(),
                  
                  h1("Proste zadanie kinematyki"),
                  textOutput("simple_task"), br(), br(),
                  
                  h1("Odwrotne zadanie kinematyki"),
                  textOutput("inverse_task"), br(), br()
              )
      ),
      tabItem(tabName = "dynamic_task",
              fluidPage(
                  h1("Strona w budowie")
              )
      ),
      tabItem(tabName = "dh_info",
              fluidPage(
                fluidRow(
                  column(width = 12,
                         h1("Parametry tabeli DH"),
                         br(),
                         column(
                           width = 6, 
                           dh_list()
                           )
                  )
                )
              )
      ),
      tabItem(tabName = "about_me",
              fluidPage(
                  h1("O Autorze"),
                  textOutput("about_me0"), br(), br(),
                  
                  h2("Dlaczego?"),
                  textOutput("about_me1"), br(), br(),
                  
                  h2("Nieoceniona pomoc"),
                  textOutput("about_me2"), br(), br(),
                  
                  h2("Podsumowanie"),
                  textOutput("about_me3"), br(), br()
              )
      )
    ),
    #page footer
    tags$footer(
      style = "background-color: #666; color: white; text-align: center; position: fixed; bottom: 0; width: 100%; padding: 10px;",
      "Wersja aplikacji: 1.2 | Autor: Marek Lubszczyk | © 2023 "
    ),
    tags$style("#plot { margin-left: 0px;}")
  )
))

# Set plotly config
options(plotly = list(width = "100%", height = "100%"))

server <- function(input, output, session) {
  
  # Create empty table 
  DH_table <- reactiveValues(
    data = data.frame(
      t = rep(NA_real_),
      d = rep(NA_real_),
      a = rep(NA_real_),
      al = rep(NA_real_),
      stringsAsFactors = FALSE
    )
  )
  
  #Create empty file
  empty_data <- data.frame()
  write.csv(empty_data, file = "tabela.csv", row.names = FALSE)
  
  # Add data button
  observeEvent(input$add_row, {
    if(nrow(DH_table$data) < 6) {
      DH_table$data <- rbind(DH_table$data, rep(NA_real_, 4))
      if(nrow(DH_table$data) == 6) {
        updateActionButton(session, "add_row", label = "Maksymalna liczba wierszy")
      }
    }
  })
  
  # Render table
  output$table <- renderDT(
    DT::datatable(
      DH_table$data, 
      editable = TRUE,
      options = list(
        dom = "t",
        columnDefs = list(
          list(targets = 0, width = "30px", className = "dt-center", borderWidth = 1, orderable = FALSE),
          list(targets = 1, width = "150px", className = "dt-center", borderWidth = 1),
          list(targets = 2:4, width = "75px", className = "dt-center", borderWidth = 1)
        ),
        # css column head
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '20px', 'font-weight': 'bold'});",
          "}")
      )
    ) %>% 
      formatStyle(c(1, 2, 3, 4), fontSize = "24px")
  )
  
  # Save data to file
  observeEvent(input$table_cell_edit, {
    info = input$table_cell_edit
    i = info$row
    j = info$col
    v = info$value
    if (i %in% seq_len(nrow(DH_table$data)) && j %in% seq_len(ncol(DH_table$data))) {
      DH_table$data[i, j] <- DT::coerceValue(v, DH_table$data[i, j])
      write.table(t(DH_table$data), "tabela.csv", row.names = FALSE, sep = ";", col.names = FALSE, quote = FALSE)
    }
  })
  
  # Press calculate 
  observeEvent(input$calculate, {
    
    #read data from file
    DH_data <- read.csv("tabela.csv", header = FALSE, sep = ";")
    DH_data <- as.matrix(DH_data)
    
    # Validate data
    if (length(DH_data) == 0 || any(apply(DH_table$data, 1, function(x) any(is.na(x) | x == "")))) {
      
      # Show error message
      showModal(modalDialog(
        title = h2("Błąd", style = "text-algin: center;"),
        "Wprowadź wartości wszystkich pól tabeli.",
        easyClose = TRUE,
        footer = NULL,
        tags$head(tags$style(".modal-dialog p { font-size: 20px; }"))
      ))
    } else {
      
      # Nrows define 
      nrows <- nrow(DH_table$data)
      
      # 1st matrix calculate
      if (nrows >= 1) {
        t <- c(DH_data[1,1])
        d <- c(DH_data[2,1])
        a <- c(DH_data[3,1])
        al <- c(DH_data[4,1])
        
        A1 <- matrix(c(
          cos(t[1]), -sin(t[1])*cos(al[1]), sin(t[1])*sin(al[1]), a[1]*cos(t[1]),
          sin(t[1]), sin(t[1])*cos(al[1]), -cos(t[1])*sin(al[1]), a[1]*sin(t[1]),
          0, sin(al[1]), cos(t[1]), d[1],
          0, 0, 0, 1
        ), nrow=4, ncol=4, byrow=TRUE)
        # print(A1, quote=FALSE)
        T1 <- A1
        # print(T1, quote=FALSE)
        
        x1 <- T1[1,4]
        y1 <- T1[2,4]
        z1 <- T1[3,4]
      }
      
      # 2nd matrix  calculate 
      if (nrows >=2) {
        t <- c(DH_data[1,1], DH_data[1,2])
        d <- c(DH_data[2,1], DH_data[2,2])
        a <- c(DH_data[3,1], DH_data[3,2])
        al <- c(DH_data[4,1], DH_data[4,2])
          
        A2 <- matrix(c(
          cos(t[2]), -sin(t[2])*cos(al[2]), sin(t[2])*sin(al[2]), a[2]*cos(t[2]),
          sin(t[2]), sin(t[2])*cos(al[2]), -cos(t[2])*sin(al[2]), a[2]*sin(t[2]),
          0, sin(al[2]), cos(t[2]), d[2],
          0, 0, 0, 1
        ), nrow=4, ncol=4, byrow=TRUE)
        T2 <- A1 %*% A2
        
        x2 <- T2[1,4]
        y2 <- T2[2,4]
        z2 <- T2[3,4]
      }
      
      # 3th matrix calculate
      if (nrows >=3) {
        t <- c(DH_data[1,1], DH_data[1,2], DH_data[1,3])
        d <- c(DH_data[2,1], DH_data[2,2], DH_data[2,3])
        a <- c(DH_data[3,1], DH_data[3,2], DH_data[3,3])
        al <- c(DH_data[4,1], DH_data[4,2], DH_data[4,3])
        
        A3 <- matrix(c(
          cos(t[3]), -sin(t[3])*cos(al[3]), sin(t[3])*sin(al[3]), a[3]*cos(t[3]),
          sin(t[3]), sin(t[3])*cos(al[3]), -cos(t[3])*sin(al[3]), a[3]*sin(t[3]),
          0, sin(al[3]), cos(t[3]), d[3],
          0, 0, 0, 1
        ), nrow=4, ncol=4, byrow=TRUE)
        T3 <- A1 %*% A2 %*% A3
        
        x3 <- T3[1,4]
        y3 <- T3[2,4]
        z3 <- T3[3,4]
      }
      
      # 4th matrix calculate
      if (nrows >=4) {
        t <- c(DH_data[1,1], DH_data[1,2], DH_data[1,3], DH_data[1,4])
        d <- c(DH_data[2,1], DH_data[2,2], DH_data[2,3], DH_data[2,4])
        a <- c(DH_data[3,1], DH_data[3,2], DH_data[3,3], DH_data[3,4])
        al <- c(DH_data[4,1], DH_data[4,2], DH_data[4,3], DH_data[4,4])
        
        A4 <- matrix(c(
          cos(t[4]), -sin(t[4])*cos(al[4]), sin(t[4])*sin(al[4]), a[4]*cos(t[4]),
          sin(t[4]), sin(t[4])*cos(al[4]), -cos(t[4])*sin(al[4]), a[4]*sin(t[4]),
          0, sin(al[4]), cos(t[4]), d[4],
          0, 0, 0, 1
        ), nrow=4, ncol=4, byrow=TRUE)
        T4 <- A1 %*% A2 %*% A3 %*% A4
        
        x4 <- T4[1,4]
        y4 <- T4[2,4]
        z4 <- T4[3,4]
      }
      
      # 5th matrix calculate
      if (nrows >= 5) {
        t <- c(DH_data[1,1], DH_data[1,2], DH_data[1,3], DH_data[1,4], DH_data[1,5])
        d <- c(DH_data[2,1], DH_data[2,2], DH_data[2,3], DH_data[2,4], DH_data[2,5])
        a <- c(DH_data[3,1], DH_data[3,2], DH_data[3,3], DH_data[3,4], DH_data[3,5])
        al <- c(DH_data[4,1], DH_data[4,2], DH_data[4,3], DH_data[4,4], DH_data[4,5])
        
        A5 <- matrix(c(
          cos(t[5]), -sin(t[5])*cos(al[5]), sin(t[5])*sin(al[5]), a[5]*cos(t[5]),
          sin(t[5]), sin(t[5])*cos(al[5]), -cos(t[5])*sin(al[5]), a[5]*sin(t[5]),
          0, sin(al[5]), cos(t[5]), d[5],
          0, 0, 0, 1
        ), nrow=4, ncol=4, byrow=TRUE)
        T5 <- A1 %*% A2 %*% A3 %*% A4 %*% A5
        
        x5 <- T5[1,4]
        y5 <- T5[2,4]
        z5 <- T5[3,4]
      }
      
      # 6th matrix calculate
      if( nrows >=6) {
        t <- c(DH_data[1,1], DH_data[1,2], DH_data[1,3], DH_data[1,4], DH_data[1,5], DH_data[1,6])
        d <- c(DH_data[2,1], DH_data[2,2], DH_data[2,3], DH_data[2,4], DH_data[2,5], DH_data[2,6])
        a <- c(DH_data[3,1], DH_data[3,2], DH_data[3,3], DH_data[3,4], DH_data[3,5], DH_data[3,6])
        al <- c(DH_data[4,1], DH_data[4,2], DH_data[4,3], DH_data[4,4], DH_data[4,5], DH_data[4,6])
        
        A6 <- matrix(c(
          cos(t[6]), -sin(t[6])*cos(al[6]), sin(t[6])*sin(al[6]), a[6]*cos(t[6]),
          sin(t[6]), sin(t[6])*cos(al[6]), -cos(t[6])*sin(al[6]), a[6]*sin(t[6]),
          0, sin(al[6]), cos(t[6]), d[6],
          0, 0, 0, 1
        ), nrow=4, ncol=4, byrow=TRUE)
        T6 <- A1 %*% A2 %*% A3 %*% A4 %*% A5 %*% A6
        
        x6 <- T6[1,4]
        y6 <- T6[2,4]
        z6 <- T6[3,4]
      }
      
      # 3D Plot function
        output$plot <- renderPlotly({
          
          # min/max value
         if (nrows == 1) {
            x_min <- x1
            x_max <- x1
            y_min <- y1
            y_max <- y1
            z_max <- z1
            z_min <- z1
          } else if (nrows == 2) {
            x_min <- min(c(x1, x2))
            x_max <- max(c(x1, x2))
            y_min <- min(c(y1, y2))
            y_max <- max(c(y1, y2))
            z_max <- max(c(z1, z2))
            z_min <- min(c(z1, z2))
          } else if (nrows == 3) {
            x_min <- min(c(x1, x2, x3))
            x_max <- max(c(x1, x2, x3))
            y_min <- min(c(y1, y2, y3))
            y_max <- max(c(y1, y2, y3))
            z_max <- max(c(z1, z2, z3))
            z_min <- min(c(z1, z2, z3))
          } else if (nrows == 4) {
            x_min <- min(c(x1, x2, x3, x4))
            x_max <- max(c(x1, x2, x3, x4))
            y_min <- min(c(y1, y2, y3, y4))
            y_max <- max(c(y1, y2, y3, y4))
            z_max <- max(c(z1, z2, z3, z4))
            z_min <- min(c(z1, z2, z3, z4))
          } else if (nrows == 5) {
            x_min <- min(c(x1, x2, x3, x4, x5))
            x_max <- max(c(x1, x2, x3, x4, x5))
            y_min <- min(c(y1, y2, y3, y4, y5))
            y_max <- max(c(y1, y2, y3, y4, y5))
            z_max <- max(c(z1, z2, z3, z4, z5))
            z_min <- min(c(z1, z2, z3, z4, z5))
          } else if (nrows == 6) {
            x_min <- min(c(x1, x2, x3, x4, x5, x6))
            x_max <- max(c(x1, x2, x3, x4, x5, x6))
            y_min <- min(c(y1, y2, y3, y4, y5, y6))
            y_max <- max(c(y1, y2, y3, y4, y5, y6))
            z_max <- max(c(z1, z2, z3, z4, z5, z6))
            z_min <- min(c(z1, z2, z3, z4, z5, z6))
          }
          
          if (z_min >= 5) z_min <- -15
          else z_min <- z_min -15

          # Render 3Dplot
          chart3d <- plot_ly(type = "scatter3d", mode = "markers", width = 1000, height = 950)
          if (nrows >= 1) chart3d <- add_trace(chart3d, x = c(0, x1), y = c(0, y1), z = c(0, z1), type = "scatter3d", mode = "lines", line = list(color = "blue"), name = "Człon 1")
          if (nrows >= 2) chart3d <- add_trace(chart3d, x = c(x1, x2), y = c(y1, y2), z = c(z1, z2), type = "scatter3d", mode = "lines", line = list(color = "red"), name = "Człon 2")
          if (nrows >= 3) chart3d <- add_trace(chart3d, x = c(x2, x3), y = c(y2, y3), z = c(z2, z3), type = "scatter3d", mode = "lines", line = list(color = "green"), name = "Człon 3")
          if (nrows >= 4) chart3d <- add_trace(chart3d, x = c(x3, x4), y = c(y3, y4), z = c(z3, z4), type = "scatter3d", mode = "lines", line = list(color = "brown"), name = "Człon 4")
          if (nrows >= 5) chart3d <- add_trace(chart3d, x = c(x4, x5), y = c(y4, y5), z = c(z4, z5), type = "scatter3d", mode = "lines", line = list(color = "yellow"), name = "Człon 5")
          if (nrows >= 6) chart3d <- add_trace(chart3d, x = c(x5, x6), y = c(y5, y6), z = c(z5, z6), type = "scatter3d", mode = "lines", line = list(color = "purple"), name = "Człon 6")
          chart3d <- chart3d %>% layout(scene = list(xaxis = list(title = "Oś X [mm]", range = c(x_min, x_max)),
                                                     yaxis = list(title = "Oś y [mm]", range = c(y_min, y_max)),
                                                     zaxis = list(title = "Oś z [mm]", range = c(z_min, z_max)),
                                                     aspectmode = "manual", aspectratio = list(x=1, y=1, z=1),
                                                     width = 1500, height = 950),
                                                     plot_bgcolor = "transparent",
                                                     paper_bgcolor = "transparent")
          print(chart3d)
        })
      
      # render result 
      output$result <- renderPrint(
        if(input$only_last_result) {
          if (nrows == 1) cat(sprintf("Pozycja: x1 = %-20.10f  Pozycja: y1 = %-20.10f  Pozycja: z1 = %-20.10f\n", x1, y1, z1))
          else if (nrows == 2) cat(sprintf("Pozycja: x2 = %-20.10f  Pozycja: y2 = %-20.10f  Pozycja: z2 = %-20.10f\n", x2, y2, z2))
          else if (nrows == 3) cat(sprintf("Pozycja: x3 = %-20.10f  Pozycja: y3 = %-20.10f  Pozycja: z3 = %-20.10f\n", x3, y3, z3))
          else if (nrows == 4) cat(sprintf("Pozycja: x4 = %-20.10f  Pozycja: y4 = %-20.10f  Pozycja: z4 = %-20.10f\n", x4, y4, z4))
          else if (nrows == 5) cat(sprintf("Pozycja: x5 = %-20.10f  Pozycja: y5 = %-20.10f  Pozycja: z5 = %-20.10f\n", x5, y5, z5))
          else if (nrows == 6) cat(sprintf("Pozycja: x6 = %-20.10f  Pozycja: y6 = %-20.10f  Pozycja: z6 = %-20.10f\n", x6, y6, z6))
        }
        else {
          if (nrows == 1) {
            cat(sprintf("Pozycja: x1 = %-20.10f  Pozycja: y1 = %-20.10f  Pozycja: z1 = %-20.10f\n", x1, y1, z1))
          } else if (nrows == 2) {
            cat(sprintf("Pozycja: x1 = %-20.10f  Pozycja: y1 = %-20.10f  Pozycja: z1 = %-20.10f\n", x1, y1, z1))
            cat(sprintf("Pozycja: x2 = %-20.10f  Pozycja: y2 = %-20.10f  Pozycja: z2 = %-20.10f\n", x2, y2, z2))
          } else if (nrows == 3) {
            cat(sprintf("Pozycja: x1 = %-20.10f  Pozycja: y1 = %-20.10f  Pozycja: z1 = %-20.10f\n", x1, y1, z1))
            cat(sprintf("Pozycja: x2 = %-20.10f  Pozycja: y2 = %-20.10f  Pozycja: z2 = %-20.10f\n", x2, y2, z2))
            cat(sprintf("Pozycja: x3 = %-20.10f  Pozycja: y3 = %-20.10f  Pozycja: z3 = %-20.10f\n", x3, y3, z3))
          } else if (nrows == 4) {
            cat(sprintf("Pozycja: x1 = %-20.10f  Pozycja: y1 = %-20.10f  Pozycja: z1 = %-20.10f\n", x1, y1, z1))
            cat(sprintf("Pozycja: x2 = %-20.10f  Pozycja: y2 = %-20.10f  Pozycja: z2 = %-20.10f\n", x2, y2, z2))
            cat(sprintf("Pozycja: x3 = %-20.10f  Pozycja: y3 = %-20.10f  Pozycja: z3 = %-20.10f\n", x3, y3, z3))
            cat(sprintf("Pozycja: x4 = %-20.10f  Pozycja: y4 = %-20.10f  Pozycja: z4 = %-20.10f\n", x4, y4, z4))
          } else if (nrows == 5) {
            cat(sprintf("Pozycja: x1 = %-20.10f  Pozycja: y1 = %-20.10f  Pozycja: z1 = %-20.10f\n", x1, y1, z1))
            cat(sprintf("Pozycja: x2 = %-20.10f  Pozycja: y2 = %-20.10f  Pozycja: z2 = %-20.10f\n", x2, y2, z2))
            cat(sprintf("Pozycja: x3 = %-20.10f  Pozycja: y3 = %-20.10f  Pozycja: z3 = %-20.10f\n", x3, y3, z3))
            cat(sprintf("Pozycja: x4 = %-20.10f  Pozycja: y4 = %-20.10f  Pozycja: z4 = %-20.10f\n", x4, y4, z4))
            cat(sprintf("Pozycja: x5 = %-20.10f  Pozycja: y5 = %-20.10f  Pozycja: z5 = %-20.10f\n", x5, y5, z5))
          } else if (nrows == 6) {
            cat(sprintf("Pozycja: x1 = %-20.10f  Pozycja: y1 = %-20.10f  Pozycja: z1 = %-20.10f\n", x1, y1, z1))
            cat(sprintf("Pozycja: x2 = %-20.10f  Pozycja: y2 = %-20.10f  Pozycja: z2 = %-20.10f\n", x2, y2, z2))
            cat(sprintf("Pozycja: x3 = %-20.10f  Pozycja: y3 = %-20.10f  Pozycja: z3 = %-20.10f\n", x3, y3, z3))
            cat(sprintf("Pozycja: x4 = %-20.10f  Pozycja: y4 = %-20.10f  Pozycja: z4 = %-20.10f\n", x4, y4, z4))
            cat(sprintf("Pozycja: x5 = %-20.10f  Pozycja: y5 = %-20.10f  Pozycja: z5 = %-20.10f\n", x5, y5, z5))
            cat(sprintf("Pozycja: x6 = %-20.10f  Pozycja: y6 = %-20.10f  Pozycja: z6 = %-20.10f\n", x6, y6, z6))
          }
        }
      )
    }
  })
  
  #kinematic
  output$kinematic <- renderText({
    kinematyka()
  })
  
  # simple kinematic task
  output$simple_task <- renderText({
    simple_task()
  })
  
  #inverse kinematic task
  output$inverse_task <- renderText({
    inverse_task()
  })
  
  #about me
  output$about_me0 <- renderText({
    about_me0()
  })
  output$about_me1 <- renderText({
    about_me1()
  })
  output$about_me2 <- renderText({
    about_me2()
  })
  output$about_me3 <- renderText({
    about_me3()
  })
}
shinyApp(ui = ui, server = server)