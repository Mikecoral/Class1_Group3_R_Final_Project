library(shiny)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(hexbin)
library(sf)
library(plotly)
library(lubridate)

dt <- fread("trips_2015-week02_final_clean.csv")

N <- 1000
set.seed(123)

dt[, tpep_pickup_datetime := as.POSIXct(tpep_pickup_datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
dt[, tpep_dropoff_datetime := as.POSIXct(tpep_dropoff_datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]

dt <- dt %>%
  mutate(
    pickup_hour = hour(tpep_pickup_datetime),
    pickup_wday_num = lubridate::wday(tpep_pickup_datetime, week_start = 1), 
    pickup_wday = factor(pickup_wday_num, 
                         levels = 1:7, 
                         labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                         ordered = TRUE)
  )

center_lat <- 40.7128
center_lon <- -74.0059
dt[, area := case_when(
  pickup_latitude < center_lat & pickup_longitude < center_lon ~ "SW",
  pickup_latitude < center_lat & pickup_longitude > center_lon ~ "SE",
  pickup_latitude > center_lat & pickup_longitude < center_lon ~ "NW",
  pickup_latitude > center_lat & pickup_longitude > center_lon ~ "NE",
  TRUE ~ "All"
)]

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

ui <- fluidPage(
  titlePanel("NYC Yellow Taxi Data Visualization"),
  
  tags$head(
    tags$style(HTML("
      .value-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        border-radius: 15px;
        padding: 25px;
        color: white;
        box-shadow: 0 8px 16px rgba(0,0,0,0.2);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        margin-bottom: 20px;
        position: relative;
        overflow: hidden;
        min-height: 120px;
      }
      
      .value-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 12px 24px rgba(0,0,0,0.3);
      }
      
      .value-box-revenue {
        background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%) !important;
      }
      
      .value-box-pickup {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
      }
      
      .value-box-icon {
        font-size: 80px;
        opacity: 0.15;
        position: absolute;
        right: 20px;
        top: 50%;
        transform: translateY(-50%);
        z-index: 0;
      }
      
      .value-box-label {
        font-size: 14px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 1.5px;
        margin-bottom: 12px;
        opacity: 0.95;
        color: white;
      }
      
      .value-box-value {
        font-size: 42px;
        font-weight: bold;
        margin: 0;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
        color: white;
        line-height: 1.2;
      }
      
      .value-box-content {
        position: relative;
        z-index: 1;
      }
      
      .filter-summary-box {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        font-family: 'Arial', sans-serif;
        font-size: 14px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.05);
      }
      .filter-summary-box h4 {
        margin-top: 0;
        color: #495057;
        border-bottom: 2px solid #6c757d;
        padding-bottom: 5px;
      }
      .filter-summary-box p {
        margin-bottom: 5px;
        line-height: 1.6;
      }
      .filter-summary-box strong {
        color: #343a40;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h3("Area Filter"),
      radioButtons(
        inputId = "area_filter",
        label = "Select Area",
        choices = c("All" = "All", "SW" = "SW", "SE" = "SE", "NW" = "NW", "NE" = "NE"),
        selected = "All"
      ),
      
      hr(),
      
      h3("Weekday Filter"),
      checkboxGroupInput(
        inputId = "weekday_filter",
        label = "Select Weekdays",
        choices = weekdays,
        selected = weekdays
      ),
      
      hr(),
      
      h3("Time of Day Filter"),
      sliderInput(
        inputId = "time_filter",
        label = "Select Time Range (Hours)",
        min = 0,
        max = 24,
        value = c(0, 24),
        step = 0.5
      ),
      
      hr(),
      
      downloadButton("download_pickup_map", "Download Pickup Map"),
      br(), br(),
      downloadButton("download_revenue_map", "Download Revenue Map")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Data Overview",
          br(),
          
          fluidRow(
            column(6, uiOutput("statBoxTotalTrips")),
            column(6, uiOutput("statBoxTotalRevenue"))
          ),
          
          fluidRow(
            column(4, uiOutput("statBoxAvgFare")),
            column(4, uiOutput("statBoxAvgDistance")),
            column(4, uiOutput("statBoxAvgDuration"))
          ),
          
          hr(),
          
          uiOutput("filterSummaryUI"),
          
          hr(),
          
          h4("Filtered Data Preview (First 100 Rows)"),
          DT::dataTableOutput("filteredTable")
        ),
        
        tabPanel(
          "Analytics",
          br(),
          fluidRow(
            column(12,
                   plotlyOutput("bar_line", height = "500px")
            )
          ),
          br(),
          fluidRow(
            column(6,
                   h4("Trip Distance Distribution"),
                   plotlyOutput("violin_plot", height = "450px")
            ),
            column(6,
                   h4("Total Amount Distribution"),
                   plotlyOutput("box_plot", height = "450px")
            )
          ),
          br(),
          fluidRow(
            column(6,
                   h4("Distance vs Fare Relationship"),
                   plotlyOutput("scatter_with_line", height = "450px")
            ),
            column(6,
                   h4("Passenger Count Distribution"),
                   plotlyOutput("donut_plot", height = "450px")
            )
          )
        ),
        
        tabPanel(
          "Pickup Locations Map",
          br(),
          fluidRow(
            column(12,
                   uiOutput("pickupValueBox")
            )
          ),
          hr(),
          plotlyOutput("pickupMap", height = "700px")
        ),
        
        tabPanel(
          "Revenue Heatmap",
          br(),
          fluidRow(
            column(12,
                   uiOutput("revenueValueBox")
            )
          ),
          hr(),
          plotlyOutput("revenueMap", height = "700px")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$area_filter != "All") {
      dt_filtered <- dt[area == input$area_filter]
    } else {
      dt_filtered <- dt
    }
    
    dt_filtered <- dt_filtered[
      pickup_wday %in% input$weekday_filter
    ]
    
    time_start <- input$time_filter[1]
    time_end <- input$time_filter[2]
    
    hour_pickup <- as.numeric(format(dt_filtered$tpep_pickup_datetime, "%H")) + 
      as.numeric(format(dt_filtered$tpep_pickup_datetime, "%M")) / 60
    
    dt_filtered <- dt_filtered[hour_pickup >= time_start & hour_pickup <= time_end]
    
    return(dt_filtered)
  })
  
  aggregated_data <- reactive({
    dt_filtered <- filtered_data()
    
    agg_data <- dt_filtered %>%
      mutate(
        lat = round(pickup_latitude, 3),
        long = round(pickup_longitude, 3)
      ) %>%
      group_by(lat, long) %>%
      summarise(
        num_pickups = n(),
        total_revenue = sum(fare_amount, na.rm = TRUE),
        avg_fare = mean(fare_amount, na.rm = TRUE),
        .groups = 'drop'
      )
    
    return(agg_data)
  })
  
  createStatBox <- function(value, label, icon, box_class) {
    div(class = paste("value-box", box_class),
        div(class = "value-box-icon", icon),
        div(class = "value-box-content",
            div(class = "value-box-label", label),
            div(class = "value-box-value", value)
        )
    )
  }
  
  output$statBoxTotalTrips <- renderUI({
    total_trips <- format(nrow(filtered_data()), big.mark = ",")
    createStatBox(total_trips, "TOTAL TRIPS", "🚕", "value-box-pickup")
  })
  
  output$statBoxTotalRevenue <- renderUI({
    total_rev <- paste0("$", format(round(sum(filtered_data()$fare_amount, na.rm = TRUE), 2), big.mark = ","))
    createStatBox(total_rev, "TOTAL REVENUE", "💵", "value-box-revenue")
  })
  
  output$statBoxAvgFare <- renderUI({
    avg_fare <- paste0("$", format(round(mean(filtered_data()$fare_amount, na.rm = TRUE), 2), big.mark = ","))
    createStatBox(avg_fare, "AVERAGE FARE", "💲", "value-box-revenue")
  })
  
  output$statBoxAvgDistance <- renderUI({
    avg_dist <- paste0(format(round(mean(filtered_data()$trip_distance, na.rm = TRUE), 2), big.mark = ","), " miles")
    createStatBox(avg_dist, "AVERAGE TRIP DISTANCE", "📏", "value-box-pickup")
  })
  
  output$statBoxAvgDuration <- renderUI({
    dt_filtered <- filtered_data()
    avg_dur <- format(round(mean(as.numeric(difftime(dt_filtered$tpep_dropoff_datetime, 
                                                     dt_filtered$tpep_pickup_datetime, 
                                                     units = "mins")), na.rm = TRUE), 2), big.mark = ",")
    avg_dur_text <- paste(avg_dur, "min")
    createStatBox(avg_dur_text, "AVERAGE TRIP DURATION", "⏱️", "value-box-pickup")
  })
  
  output$filterSummaryUI <- renderUI({
    dt_filtered <- filtered_data()
    
    if (nrow(dt_filtered) == 0) {
      date_range_text <- "N/A (No data for this selection)"
    } else {
      date_range_text <- paste(
        format(min(dt_filtered$tpep_pickup_datetime), "%Y-%m-%d %H:%M"), 
        " to ", 
        format(max(dt_filtered$tpep_pickup_datetime), "%Y-%m-%d %H:%M")
      )
    }
    
    div(class = "filter-summary-box",
        h4("📊 Filter Summary"),
        HTML(paste0(
          "<p><strong>Total Filtered Trips:</strong> ", format(nrow(dt_filtered), big.mark = ","), "</p>",
          "<p><strong>Area Filter:</strong> ", input$area_filter, "</p>",
          "<p><strong>Weekdays Selected:</strong> ", paste(input$weekday_filter, collapse = ", "), "</p>",
          "<p><strong>Time Range:</strong> ", input$time_filter[1], " to ", input$time_filter[2], " hours</p>",
          "<p><strong>Date Range:</strong> ", date_range_text, "</p>"
        ))
    )
  })
  
  output$filteredTable <- renderDataTable({
    dt_filtered <- filtered_data()
    datatable(
      head(dt_filtered, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  output$pickupValueBox <- renderUI({
    dt_filtered <- filtered_data()
    total_pickups <- nrow(dt_filtered)
    
    div(class = "value-box value-box-pickup",
        div(class = "value-box-icon", "🚕"),
        div(class = "value-box-content",
            div(class = "value-box-label", "📍 TOTAL PICKUPS (FILTERED)"),
            div(class = "value-box-value", format(total_pickups, big.mark = ","))
        )
    )
  })
  
  output$revenueValueBox <- renderUI({
    dt_filtered <- filtered_data()
    total_revenue <- sum(dt_filtered$fare_amount, na.rm = TRUE)
    
    div(class = "value-box value-box-revenue",
        div(class = "value-box-icon", "💵"),
        div(class = "value-box-content",
            div(class = "value-box-label", "💰 TOTAL REVENUE (FILTERED)"),
            div(class = "value-box-value", paste0("$", format(round(total_revenue, 2), big.mark = ",")))
        )
    )
  })
  
  output$bar_line <- renderPlotly({
    dt_filtered <- filtered_data()
    
    wday_stats <- dt_filtered %>%
      group_by(pickup_wday) %>%
      summarise(
        order_count = n(),
        avg_amount = mean(total_amount, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(wday_stats) %>%
      add_bars(
        x = ~pickup_wday,
        y = ~order_count,
        name = "Order Count",
        marker = list(color = "#FF7F00", opacity = 0.7),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Order Count: %{y:,}<br>",
          "<extra></extra>"
        )
      ) %>%
      add_lines(
        x = ~pickup_wday,
        y = ~avg_amount,
        name = "Average Amount",
        yaxis = "y2",
        line = list(color = "#FFD700", width = 3),
        marker = list(color = "#FFD700", size = 10),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Average Amount: $%{y:.2f}<br>",
          "<extra></extra>"
        )
      ) %>%
      layout(
        title = list(
          text = "Daily Distribution of Order Volume and Average Total Amount",
          font = list(size = 14, family = "Arial", color = "black")
        ),
        xaxis = list(title = "Day of Week"),
        yaxis = list(
          title = "Order Count",
          side = "left",
          showgrid = TRUE
        ),
        yaxis2 = list(
          title = "Average Total Amount ($)",
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          rangemode = "tozero"
        ),
        hovermode = "x unified",
        showlegend = TRUE,
        margin = list(l = 80, r = 80, t = 80, b = 50),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2
        )
      )
  })
  
  output$violin_plot <- renderPlotly({
    dt_filtered <- filtered_data()
    
    if (nrow(dt_filtered) > 5000) {
      dt_sampled <- dt_filtered %>% sample_n(5000)
    } else {
      dt_sampled <- dt_filtered
    }
    
    plot_ly(
      data = dt_sampled,
      x = ~pickup_wday,
      y = ~trip_distance,
      type = 'violin',
      color = ~pickup_wday,
      colors = viridis::viridis(7),
      box = list(visible = TRUE),
      meanline = list(visible = TRUE)
    ) %>%
      layout(
        title = list(
          text = "Trip Distance Distribution by Weekday",
          font = list(size = 14)
        ),
        xaxis = list(title = "Day of Week"),
        yaxis = list(title = "Trip Distance (miles)"),
        showlegend = FALSE,
        hovermode = "closest"
      )
  })
  
  output$box_plot <- renderPlotly({
    dt_filtered <- filtered_data()
    
    if (nrow(dt_filtered) > 5000) {
      dt_sampled <- dt_filtered %>% sample_n(5000)
    } else {
      dt_sampled <- dt_filtered
    }
    
    plot_ly(
      data = dt_sampled,
      x = ~pickup_wday,
      y = ~total_amount,
      type = 'box',
      color = ~pickup_wday,
      colors = viridis::plasma(7)
    ) %>%
      layout(
        title = list(
          text = "Total Amount Distribution by Weekday",
          font = list(size = 14)
        ),
        xaxis = list(title = "Day of Week"),
        yaxis = list(title = "Total Amount ($)"),
        showlegend = FALSE,
        hovermode = "closest"
      )
  })
  
  output$scatter_with_line <- renderPlotly({
    dt_filtered <- filtered_data()
    
    sampled_data <- dt_filtered %>%
      sample_n(min(500, nrow(.)), replace = FALSE)
    
    fit <- lm(fare_amount ~ trip_distance, data = sampled_data)
    sampled_data$predicted <- predict(fit, sampled_data)
    
    plot_ly(sampled_data) %>%
      add_markers(
        x = ~trip_distance,
        y = ~fare_amount,
        name = "Actual",
        marker = list(
          color = "#3498DB",
          size = 6,
          opacity = 0.6,
          line = list(color = "#2980B9", width = 1)
        ),
        hovertemplate = paste(
          "<b>Distance:</b> %{x:.2f} miles<br>",
          "<b>Fare:</b> $%{y:.2f}<br>",
          "<extra></extra>"
        )
      ) %>%
      add_lines(
        x = ~trip_distance,
        y = ~predicted,
        name = "Linear Regression",
        line = list(color = "#E74C3C", width = 3)
      ) %>%
      layout(
        title = list(
          text = paste("Distance vs Fare Relationship (Sample:", nrow(sampled_data), "trips)"),
          font = list(size = 14)
        ),
        xaxis = list(title = "Trip Distance (miles)"),
        yaxis = list(title = "Fare Amount ($)"),
        hovermode = "closest",
        showlegend = TRUE,
        legend = list(x = 0.02, y = 0.98)
      )
  })
  
  output$donut_plot <- renderPlotly({
    dt_filtered <- filtered_data()
    
    passenger_data <- dt_filtered %>%
      filter(passenger_count %in% 1:6) %>%
      group_by(passenger_count) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(passenger_count) %>%
      mutate(
        proportion = count / sum(count),
        percentage = scales::percent(proportion, accuracy = 0.1)
      )
    
    plot_ly(
      data = passenger_data,
      labels = ~paste("Passenger", passenger_count),
      values = ~count,
      type = 'pie',
      hole = 0.4,
      marker = list(
        colors = viridis::plasma(6, begin = 0.1, end = 0.9),
        line = list(color = 'white', width = 2)
      ),
      textposition = 'inside',
      textinfo = 'label+percent',
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Count: %{value:,}<br>",
        "Percentage: %{percent}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = list(
          text = "Order Distribution by Passenger Count",
          font = list(size = 14)
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.1
        )
      )
  })
  
  output$pickupMap <- renderPlotly({
    agg_data <- aggregated_data()
    
    plot_ly(
      data = agg_data,
      x = ~long,
      y = ~lat,
      type = 'scattergl',
      mode = 'markers',
      marker = list(
        size = 3,
        color = ~log(num_pickups),
        colorscale = list(
          c(0, '#CCCCCC'),
          c(1, '#8E44AD')
        ),
        colorbar = list(
          title = "Log(Pickups)",
          tickmode = 'linear',
          bgcolor = 'black',
          tickfont = list(color = 'white'),
          titlefont = list(color = 'white')
        ),
        showscale = TRUE
      ),
      text = ~paste(
        'Pickups:', format(num_pickups, big.mark = ","),
        '<br>Avg Fare: $', round(avg_fare, 2),
        '<br>Location:', round(lat, 4), ',', round(long, 4)
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = "NYC Yellow Taxi Pickups - Filtered Data",
          font = list(color = 'white', size = 16)
        ),
        paper_bgcolor = 'black',
        plot_bgcolor = 'black',
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE,
          color = 'white'
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE,
          color = 'white',
          scaleanchor = "x",
          scaleratio = 1
        ),
        hovermode = 'closest'
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  output$revenueMap <- renderPlotly({
    agg_data <- aggregated_data() %>% filter(num_pickups > 3)
    
    plot_ly(
      data = agg_data,
      x = ~long,
      y = ~lat,
      z = ~total_revenue,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = 3,
        color = ~total_revenue,
        colorscale = list(
          c(0, '#F0F0F0'),
          c(1, '#006400')
        ),
        colorbar = list(
          title = "Revenue ($)",
          tickformat = '$,.0f',
          bgcolor = 'black',
          tickfont = list(color = 'white'),
          titlefont = list(color = 'white')
        ),
        showscale = TRUE,
        opacity = 0.8
      ),
      text = ~paste(
        'Total Revenue: $', format(round(total_revenue, 2), big.mark = ","),
        '<br>Pickups:', format(num_pickups, big.mark = ","),
        '<br>Avg Fare: $', round(avg_fare, 2),
        '<br>Location:', round(lat, 4), ',', round(long, 4)
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = "Total Revenue by Pickup Location - Filtered Data",
          font = list(color = 'white', size = 16)
        ),
        paper_bgcolor = 'black',
        plot_bgcolor = 'black',
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE,
          color = 'white'
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE,
          color = 'white',
          scaleanchor = "x",
          scaleratio = 1
        ),
        hovermode = 'closest'
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  output$download_pickup_map <- downloadHandler(
    filename = function() {
      paste0("nyc-taxi-pickup-map-", Sys.Date(), ".png")
    },
    content = function(file) {
      agg_data <- aggregated_data()
      p <- ggplot(agg_data, aes(x = long, y = lat, color = num_pickups)) +
        geom_point(size = 0.06) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"),
          plot.title = element_text(color = "white", hjust = 0.5, size = 16),
          legend.background = element_rect(fill = "black"),
          legend.title = element_text(color = "white"),
          legend.text = element_text(color = "white")
        ) +
        scale_color_gradient(low = "#CCCCCC", high = "#8E44AD", trans = "log") +
        labs(
          title = "NYC Yellow Taxi Pickups - Filtered Data",
          color = "Number of\nPickups"
        ) +
        coord_sf(default_crs = sf::st_crs(4326))
      
      ggsave(file, plot = p, width = 8, height = 8, dpi = 300, bg = "black")
    }
  )
  
  output$download_revenue_map <- downloadHandler(
    filename = function() {
      paste0("nyc-taxi-revenue-map-", Sys.Date(), ".png")
    },
    content = function(file) {
      agg_data <- aggregated_data() %>% filter(num_pickups > 100)
      p <- ggplot(agg_data, aes(x = long, y = lat, z = total_revenue)) +
        geom_point(size = 0.06, color = "#999999") +
        stat_summary_hex(fun = sum, bins = 80, alpha = 0.8) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"),
          plot.title = element_text(color = "white", hjust = 0.5, size = 16),
          legend.background = element_rect(fill = "black"),
          legend.title = element_text(color = "white"),
          legend.text = element_text(color = "white")
        ) +
        scale_fill_gradient(low = "#CCCCCC", high = "#006400", labels = dollar) +
        labs(
          title = "Total Revenue by Pickup Location - Filtered Data",
          fill = "Total Revenue"
        ) +
        coord_sf(default_crs = sf::st_crs(4326))
      
      ggsave(file, plot = p, width = 9.5, height = 8.6, dpi = 300, bg = "black")
    }
  )
}

shinyApp(ui = ui, server = server)
