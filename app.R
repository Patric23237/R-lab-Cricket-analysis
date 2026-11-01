
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# 1. Cricket dataset
players <- data.frame(
  Player = c(
    "Virat Kohli", "Rohit Sharma", "MS Dhoni",
    "KL Rahul", "Suryakumar Yadav",
    "Jasprit Bumrah", "Hardik Pandya",
    "Ravindra Jadeja"
  ),
  Matches = c(20, 20, 18, 19, 20, 20, 18, 19),
  Runs = c(850, 780, 620, 710, 900, 120, 450, 390),
  Average = c(47.2, 43.3, 41.3, 44.4, 50.0, 12.0, 32.1, 35.4),
  Strike_Rate = c(138.5, 145.2, 132.1, 139.8, 172.5, 105.3, 151.2, 142.6),
  Fifties = c(6, 5, 5, 6, 7, 0, 3, 2),
  Hundreds = c(2, 2, 1, 1, 3, 0, 0, 0),
  Wickets = c(0, 1, 0, 0, 2, 28, 15, 22)
)

# 2. Performance score
players$Performance_Score <-
  players$Runs / max(players$Runs) * 50 +
  players$Average / max(players$Average) * 30 +
  players$Wickets / max(players$Wickets) * 20

# 3. Bright green-and-white website theme
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#168653",
  bg = "#F3F7F2",
  fg = "#24352B",
  secondary = "#D9EBDD",
  base_font = font_google("Roboto")
)

# 4. Website layout
ui <- navbarPage(
  title = "🏏 Cricket Analytics",
  id = "tabs",
  theme = theme,
  
  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #F3F7F2;
        color: #24352B;
      }

      .navbar {
        background-color: #146B43 !important;
        border: none !important;
        box-shadow: 0 3px 10px rgba(20, 80, 45, 0.12);
      }

      .navbar .navbar-brand,
      .navbar .nav-link {
        color: #FFFFFF !important;
      }

      .navbar .nav-link:hover,
      .navbar .nav-link.active {
        background-color: #218653 !important;
        color: #FFD166 !important;
      }

      .stat-card {
        background: #FFFFFF;
        padding: 22px;
        margin: 8px 0 18px;
        border-radius: 14px;
        border: 1px solid #DDE8DE;
        box-shadow: 0 3px 12px rgba(20, 80, 45, 0.08);
        min-height: 125px;
      }

      .stat-card h4 {
        color: #52665A;
        font-size: 16px;
        font-weight: 600;
      }

      .stat-value {
        color: #168653;
        font-size: 25px;
        font-weight: bold;
        overflow-wrap: anywhere;
      }

      h2, h3 {
        color: #146B43;
        margin-bottom: 20px;
        font-weight: 700;
      }

      .well {
        background: #FFFFFF;
        border: 1px solid #DDE8DE;
        border-radius: 12px;
      }

      .dataTables_wrapper {
        background: #FFFFFF;
        padding: 12px;
        border-radius: 12px;
      }

      table.dataTable thead th {
        background-color: #E1F1E5;
        color: #146B43;
      }

      .btn-primary {
        background: #168653;
        border-color: #168653;
      }

      .btn-primary:hover {
        background: #105F3A;
        border-color: #105F3A;
      }

      .form-control, .form-select {
        border-radius: 8px;
        border-color: #CBDDCE;
      }

      .container-fluid {
        padding-top: 18px;
        padding-bottom: 25px;
      }

      .tab-content {
        padding-top: 10px;
      }
    "))
  ),
  
  # OVERVIEW
  tabPanel(
    "Overview",
    fluidPage(
      h2("🏆 Player Performance Dashboard"),
      p("Explore the cricket statistics in this sample dataset."),
      
      fluidRow(
        column(
          3,
          div(class = "stat-card",
              h4("Total Runs"),
              div(class = "stat-value", sum(players$Runs)))
        ),
        
        column(
          3,
          div(class = "stat-card",
              h4("Total Wickets"),
              div(class = "stat-value", sum(players$Wickets)))
        ),
        
        column(
          3,
          div(class = "stat-card",
              h4("Top Run Scorer"),
              div(class = "stat-value",
                  players$Player[which.max(players$Runs)])
          )
        ),
        
        column(
          3,
          div(class = "stat-card",
              h4("Top Bowler"),
              div(class = "stat-value",
                  players$Player[which.max(players$Wickets)])
          )
        )
      ),
      
      h3("Runs scored by players"),
      plotOutput("runs_plot", height = "420px"),
      
      h3("Wickets taken by players"),
      plotOutput("wickets_plot", height = "420px")
    )
  ),
  
  # PLAYER DATA
  tabPanel(
    "Player Data",
    fluidPage(
      h2("Player Statistics"),
      
      selectInput(
        "selected_player",
        "Choose a player:",
        choices = c("All Players", players$Player)
      ),
      
      DTOutput("player_table")
    )
  ),
  
  # CHARTS
  tabPanel(
    "Charts",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "metric",
          "Choose statistic:",
          choices = c(
            "Runs" = "Runs",
            "Batting Average" = "Average",
            "Strike Rate" = "Strike_Rate",
            "Wickets" = "Wickets",
            "Fifties" = "Fifties",
            "Hundreds" = "Hundreds",
            "Performance Score" = "Performance_Score"
          )
        ),
        
        radioButtons(
          "chart_type",
          "Chart type:",
          choices = c(
            "Bar Chart" = "bar",
            "Line Chart" = "line"
          ),
          selected = "bar"
        )
      ),
      
      mainPanel(
        h3(textOutput("chart_title")),
        plotOutput("metric_plot", height = "500px")
      )
    )
  ),
  
  # INSIGHTS
  tabPanel(
    "Insights",
    fluidPage(
      h2("Performance Insights"),
      
      h3("Top 3 Players by Performance Score"),
      DTOutput("top_players"),
      
      h3("Runs vs Strike Rate"),
      plotOutput("scatter_plot", height = "420px"),
      
      h3("Correlation"),
      verbatimTextOutput("correlation"),
      
      h3("Summary Statistics"),
      verbatimTextOutput("summary")
    )
  )
)

# 5. Server logic
server <- function(input, output, session) {
  
  output$runs_plot <- renderPlot({
    ggplot(players, aes(
      x = reorder(Player, Runs),
      y = Runs
    )) +
      geom_col(fill = "#168653", width = 0.7) +
      coord_flip() +
      labs(x = NULL, y = "Runs") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#F3F7F2", color = NA),
        panel.background = element_rect(fill = "#F3F7F2", color = NA),
        panel.grid.major = element_line(color = "#DDE8DE"),
        axis.text = element_text(color = "#24352B")
      )
  })
  
  output$wickets_plot <- renderPlot({
    ggplot(players, aes(
      x = reorder(Player, Wickets),
      y = Wickets
    )) +
      geom_col(fill = "#E6B84A", width = 0.7) +
      coord_flip() +
      labs(x = NULL, y = "Wickets") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#F3F7F2", color = NA),
        panel.background = element_rect(fill = "#F3F7F2", color = NA),
        panel.grid.major = element_line(color = "#DDE8DE"),
        axis.text = element_text(color = "#24352B")
      )
  })
  
  output$player_table <- renderDT({
    data <- players
    
    if (input$selected_player != "All Players") {
      data <- data[data$Player == input$selected_player, ]
    }
    
    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE),
      class = "stripe hover compact"
    )
  })
  
  output$chart_title <- renderText({
    paste("Player Comparison:", gsub("_", " ", input$metric))
  })
  
  output$metric_plot <- renderPlot({
    metric <- input$metric
    
    p <- ggplot(
      players,
      aes(
        x = reorder(Player, .data[[metric]]),
        y = .data[[metric]]
      )
    )
    
    if (input$chart_type == "bar") {
      p <- p +
        geom_col(fill = "#168653", width = 0.7) +
        coord_flip()
    } else {
      p <- p +
        geom_line(
          aes(group = 1),
          color = "#168653",
          linewidth = 1.2
        ) +
        geom_point(
          color = "#E6B84A",
          size = 3.5
        ) +
        coord_flip()
    }
    
    p +
      labs(x = NULL, y = gsub("_", " ", metric)) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#F3F7F2", color = NA),
        panel.background = element_rect(fill = "#F3F7F2", color = NA),
        panel.grid.major = element_line(color = "#DDE8DE"),
        axis.text = element_text(color = "#24352B")
      )
  })
  
  output$top_players <- renderDT({
    ranked <- players[
      order(-players$Performance_Score),
      c(
        "Player", "Runs", "Average", "Wickets",
        "Performance_Score"
      )
    ]
    
    datatable(
      head(ranked, 3),
      rownames = FALSE,
      options = list(dom = "t"),
      class = "stripe hover compact"
    )
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(players, aes(x = Runs, y = Strike_Rate)) +
      geom_point(color = "#168653", size = 4) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "#E6B84A",
        linewidth = 1.2
      ) +
      labs(x = "Runs", y = "Strike Rate") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#F3F7F2", color = NA),
        panel.background = element_rect(fill = "#F3F7F2", color = NA),
        panel.grid.major = element_line(color = "#DDE8DE"),
        axis.text = element_text(color = "#24352B")
      )
  })
  
  output$correlation <- renderPrint({
    cor(players$Runs, players$Strike_Rate)
  })
  
  output$summary <- renderPrint({
    summary(players)
  })
}

# 6. Launch the website
shinyApp(ui = ui, server = server)
