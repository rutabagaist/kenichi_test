library(RMariaDB)
library(DBI)
library(tidyverse)
library(pool)
library(lubridate)
library(shiny)
library(plotly)
library(suncalc)


HalfHourlyNumDays                   <- 31
HalfHourlyDisplayDaysDefault        <- 7
HalfHourlyNumRecords                <- HalfHourlyNumDays * 48   # 7 Days worth of half hourlies
DailyNumRecords                     <- 365      # THe past year worth of dailies
LiveNumRecords                      <- 24 * 60  # The past day worth of minute records




UTMMSdb_read                       <- dbConnect(RMariaDB::MariaDB(), dbname = "default", host = "localhost", user = "utmms_read", password = "Z9b2ZvsN6*Nhp4@6", port = 3306)
#      UTMMSdb_read                       <- dbConnect(RMariaDB::MariaDB(), dbname = "default", host = "", user = "utmms_read", password = "UTMMSDataRead", port = 3306)
      
      
      # The DB code inlcudes a stupid hack to rectify the stored timestamps. In the DB all time stamps are stored as-is from the weather station. This means
      # they are in the "EST" time zone. Always, daylight savings is not observed by the weather station. All observation times are stored in EST. 
      # The RMariaDB connector assumes that storage is in UTC format, so to fix this an two step conversion is done to the stored dates to force them into EST 
      # within R. An update to the driver might fix this (by allowing specification of the time-zone), but for now the hack will have to do.
      
      HalfHourlyDisplay_UTMMSdb           <- as_tibble(
        dbGetQuery(UTMMSdb_read, paste0("SELECT * FROM `default`.halfhourlyUTMMSData ORDER BY TIMESTAMP DESC LIMIT ", HalfHourlyNumRecords)) %>%
          modify_if(., is.POSIXt, with_tz, "UTC") %>% 
          modify_if(., is.POSIXt, force_tz, "EST")
      )     # This is a fix/hack documented in the header.
      
      DailyDisplay_UTMMSdb                <- as_tibble(
        dbGetQuery(UTMMSdb_read, paste0("SELECT * FROM `default`.dailyUTMMSData ORDER BY TIMESTAMP DESC LIMIT ", DailyNumRecords)) %>%
          modify_if(., is.POSIXt, with_tz, "UTC") %>% 
          modify_if(., is.POSIXt, force_tz, "EST")
      )     # This is a fix/hack documented in the header.
      
      LiveDisplay_UTMMSdb                <- as_tibble(
        dbGetQuery(UTMMSdb_read, paste0("SELECT * FROM `default`.liveUTMMSData ORDER BY TIMESTAMP DESC LIMIT ", LiveNumRecords)) %>%
          modify_if(., is.POSIXt, with_tz, "UTC") %>% 
          modify_if(., is.POSIXt, force_tz, "EST")
      )     # This is a fix/hack documented in the header.

# Disconnect from the database
dbDisconnect(UTMMSdb_read)

# This operation groups by day, creates a mean daily albedo value for each day

HalfHourlyDisplay_UTMMSdb %>%
  group_by(floor_date(TIMESTAMP, unit = 'day')) %>% 
  summarise(avg = round(mean(Albedo_Avg, na.rm = T), 3), 
            sd = round(sd(Albedo_Avg, na.rm = T), 3))         -> Daily_AlbedoDisplay

# Add column names
colnames(Daily_AlbedoDisplay)        <- c("date", "avg", "sd")

# Add 12 hours to each daily value to centre it behind the radiation data.
Daily_AlbedoDisplay$date             <- Daily_AlbedoDisplay$date + hours (12)

# Create the cumulative precipitation plot

PrecipCumulative <- tibble(
  TimeSince = c(
    "Past 30 minutes",
    "Past hour",
    "Past 6 hours",
    "Past 12 hours",
    "Past 24 hours",
    "Past 2 days",
    "Past 7 days",
    "Past 30 days"),
  mm_Precip = c(
    head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,1),
    sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,2)),
    sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,12)),
    sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,24)),
    sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,48)),
    sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,96)),
    sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,384)),
    sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,1440))),
  Colour = c('rgba(196, 213, 223, 1)',
             'rgba(170, 202, 223, 1)',
             'rgba(155, 197, 223, 1)',
             'rgba(140, 191, 223, 1)',
             'rgba(118, 182, 223, 1)',
             'rgba(105, 177, 223, 1)',
             'rgba(94, 173, 223, 1)',
             'rgba(51,  156, 223, 1)')
)











# Load the global.R file which sources packages and performs the data importing
#source(paste0(filesRoot, "\\_geo_lab\\UTMMS\\__rspace\\_shiny\\UTMMS_FrontEnd\\db_implementation_sandbox.R"), local = FALSE)
filesRoot <- "C:\\Users\\me\\OneDrive - University of Toronto"

source(paste0(filesRoot, "\\_geo_lab\\UTMMS\\__rspace\\_shiny\\UTMMS_FrontEnd\\global.R"), local = FALSE)


shinyServer(function(input, output) {
  
  
  # Constants
  
  # Get 31 days of half hourly data, display most recent 7 by default
  
  UTMMScoords                       <- c(43.552559, -79.666493)
  currentSunAltitude                <- getSunlightPosition(date = now("EST"), lat = UTMMScoords[1], lon = UTMMScoords[2]) %>% .$altitude * 180 / pi
  currentSunAzimuth                 <- getSunlightPosition(date = now("EST"), lat = UTMMScoords[1], lon = UTMMScoords[2]) %>% .$azimuth * 180 / pi
  currentSunParameters              <- getSunlightTimes(date = date(now("EST")), lat = UTMMScoords[1], lon = UTMMScoords[2], tz = "EST")
  
  
  # Graphing variables
  
  # Used for determining the axis extents for both current and historical data. Takes min/max and expands it by one degree to add some buffer to the graph for display.
  AirTemp_minaxis                       <- min(HalfHourlyDisplay_UTMMSdb$AirTemp_Avg) - 1
  AirTemp_maxaxis                       <- max(HalfHourlyDisplay_UTMMSdb$AirTemp_Avg) + 1
  
  # Creates a positive and negative column for the purpose of coloring these differently in the graph.
  HalfHourlyDisplay_UTMMSdb                         <- HalfHourlyDisplay_UTMMSdb %>% mutate(AirTemp_Neg = ifelse(AirTemp_Avg <= 0, AirTemp_Avg, 0))
  HalfHourlyDisplay_UTMMSdb                         <- HalfHourlyDisplay_UTMMSdb %>% mutate(AirTemp_Pos = ifelse(AirTemp_Avg >= 0, AirTemp_Avg, 0))
  
  # Pulls the latest value from the LiveDisplay_UTMMSdb
  latest_AirTemp_Avg                <- tail(LiveDisplay_UTMMSdb$AirTemp_Avg, 1)
  latest_RH_Avg                     <- tail(LiveDisplay_UTMMSdb$RH_Avg, 1)
  latest_TimeStamp                  <- tail(LiveDisplay_UTMMSdb$TIMESTAMP, 1)
  
  latest_TotalRad_y                 <- c(tail(LiveDisplay_UTMMSdb$SR01up_Avg, n=1), tail(LiveDisplay_UTMMSdb$SR01down_Avg, n=1), tail(LiveDisplay_UTMMSdb$IR01upCo_Avg, n=1), tail(LiveDisplay_UTMMSdb$IR01downCo_Avg, n=1))
  latest_TotalRad_x                 <- c("SW Incoming/Upfacing", "Shortwave Outgoing", "Longwave Incoming", " Longwave Outgoing")
  
  
  
  # Global parameters
  paper_bg_global                   <- 'transparent'
  plot_bg_global                    <- 'transparent'
  margins                           <- list(l = 50, r = 50, b = 50, t = 50, pad = 0)
  current_conditions_margins        <- list(l = 0, r = 35, b = 70, t = 100, pad = 0)
  legend_font_style                 <- list(family = "inherit", size = 12, color = 'grey35')
  title_font_style                  <- list(family = "inherit", size = 12, color = 'grey35')
  axis_title_fontstyle              <- list(family = "inherit", size = 12, color = 'grey35')
  tick_fontsize                     <- 11
  default_legend                    <- list(x = .8, y = 1.2, font = legend_font_style)
  
  # Colours
  fill_bars                         <- 'rgba(182, 216, 196, 0.8)'
  bars_width_curcond                <- 0.75
  fill_bars_neg                     <- 'rgba(52, 196, 229, 0.8)'
  fill_bars_pos                     <- 'rgba(229, 79, 52, 0.6)'
  fill_sw_in                        <- 'rgba(252, 228, 50, 0.6)'
  fill_sw_out                       <- 'rgba(244, 184, 111, 0.9)'
  fill_lw_in                        <- 'rgba(252, 73, 73, 0.9)'
  fill_lw_out                       <- 'rgba(198, 21, 21, 0.6)'
  axis_line_color                   <- toRGB("lightgrey")
  line_fill                         <- toRGB("black")
  line_width                        <-  0.6
  fill_temp                         <- 'rgba(229, 79, 52, 0.6)'
  fill_rh                           <- 'rgba(182, 216, 196, 0.3)'
  line_rh                           <- list(width = 2, color = 'rgb(69, 107, 80)', dash = 'dot')
  
  # Dynamic Colours
  fill_bars_current_temp            <- ifelse(latest_AirTemp_Avg >= 0, fill_bars_pos, fill_bars_neg)
  
  
  
  
  
  
  
  
  # The half hourly Radiation Plot
  # |____   
  
  
  
  
  
  
  output$RadiationPlot <- renderPlotly( {
    
        # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
        x_begin                 <- input$time_slider[1]
        x_end                   <- input$time_slider[2]  

    

    plot_ly(HalfHourlyDisplay_UTMMSdb) %>%
      
      add_trace(x = ~Daily_AlbedoDisplay$date,
                y = ~Daily_AlbedoDisplay$avg * 100,
                name = 'Daily Average Albedo      ',  # Added spaces to prevent legend text cut-off   
                type = 'bar',
                yaxis = 'y2',
                marker = list(color = fill_bars,
                              line = list(color = line_fill,
                                          width = line_width)),
                hoverlabel = list(bgcolor = fill_bars),
                hoverinfo = 'y',
                error_y = list(visible = T, 
                               type = 'data', 
                               symmetric = T, 
                               array = Daily_AlbedoDisplay$sd * 100,
                               color = line_fill,
                               thickness = 1,
                               width = 10)
      ) %>%
      
      add_trace(x = ~TIMESTAMP, 
                y = ~SR01up_Avg, 
                type = 'scatter',
                mode = 'lines',
                fill= 'tozeroy', 
                name = 'Incoming Shortwave', 
                fillcolor = fill_sw_in, 
                line = list(color = line_fill, 
                            width = line_width),
                hoverlabel = list(bgcolor = fill_sw_in),
                hoverinfo = 'x+y') %>%
      
      
      add_trace(x = ~TIMESTAMP, 
                y = ~SR01down_Avg, 
                type = 'scatter',
                mode = 'lines',
                fill = 'tozeroy', 
                name = 'Outgoing Shortwave', 
                fillcolor = fill_sw_out,
                line = list(color = line_fill,
                            width = line_width),
                hoverlabel = list(bgcolor = fill_sw_out),
                hoverinfo = 'x+y') %>%
          
          add_trace(x = DailyDisplay_UTMMSdb$SR01up_TMx,
                    y = DailyDisplay_UTMMSdb$SR01up_Max,
                    type = 'scatter',
                    mode = 'markers',
                    symbol = I('triangle-down'),
                    color = I('red'), 
                    marker = list(size = 10),
                    name = "Daily Max Incoming Shortwave",
                    hoverinfo = 'x+y') %>%
          
      
      layout(title = list(text = "Incoming and Outgoing Shortwave Radiation and Albedo", x = 0.1),
             
             paper_bgcolor = paper_bg_global,
             plot_bgcolor = plot_bg_global,
             
             font = title_font_style,
        
             legend = default_legend,
             
             hovermode = 'x',
             
             margin = margins,
             
             xaxis = list(title = "Date",
                          titlefont = axis_title_fontstyle,
                          range = list(x_begin, x_end),
                          type = 'date',
                          ticklen = 8,
                          showgrid = TRUE,
                          showline = TRUE,
                          linecolor = axis_line_color,
                          tickfont = list(size = tick_fontsize)),
             
             yaxis2 = list(range = c(0.0, 100.0),
                           title = list(text = "Albedo (%)"),
                           titlefont = axis_title_fontstyle,
                           tickfont = list(size = tick_fontsize),
                           ticks = "outside",
                           side = 'right',
                           showgrid = FALSE,
                           zeroline = FALSE,
                           linecolor = axis_line_color),
             
             yaxis = list(title = list(text = "Solar Radiation (W m<sup>2</sup>)"),
                          titlefont = axis_title_fontstyle,
                          overlaying = 'y2',
                          ticks = "outside",
                          range = c(0, 1300),
                          showgrid = FALSE,
                          zeroline = FALSE,
                          linecolor = axis_line_color,
                          tickfont = list(size = tick_fontsize))) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  # The half hourly Precipitation Plot
  # |____   
  
  output$PrecipitationPlot <- renderPlotly( {
        
        # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
        x_begin                 <- input$time_slider[1]
        x_end                   <- input$time_slider[2]  
        
        plot_ly(DailyDisplay_UTMMSdb) %>%
              
              add_trace(x = ~TIMESTAMP - hours(12),
                        y = ~Precip_mm_Tot,
                        name = 'Precipitation',
                        type = 'bar',
                        yaxis = 'y',
                        marker = list(color = fill_bars,
                                      line = list(color = line_fill,
                                                  width = line_width)),
                        hoverlabel = list(bgcolor = fill_bars),
                        hoverinfo = 'y'
              ) %>%
              
              
              layout(title = "Daily Total Precipitation",
                     
                     paper_bgcolor = paper_bg_global,
                     plot_bgcolor = plot_bg_global,
                     font = title_font_style,
                     
                     legend = default_legend,
                     
                     hovermode = 'x',
                     
                     margin = margins,
                     
                     
                     xaxis = list(title = "Date",
                                  titlefont = axis_title_fontstyle,
                                  range = list(x_begin, x_end),
                                  type = 'date',
                                  ticklen = 8,
                                  showgrid = TRUE,
                                  showline = TRUE,
                                  linecolor = axis_line_color,
                                  tickfont = list(size = tick_fontsize)),
                     
                     
                     
                     yaxis = list(title = list(text = "Precipitation (mm)"),
                                  titlefont = axis_title_fontstyle,
                                  ticks = "outside",
                                  
                                  showgrid = FALSE,
                                  zeroline = FALSE,
                                  linecolor = axis_line_color,
                                  tickfont = list(size = tick_fontsize))) %>%
              
              config(displayModeBar = FALSE)
        
  })
  
  # The Cumulative Precipitation Plot
  # |____   
  
  output$PrecipitationCumulativePlot <- renderPlotly( {
    
    plot_ly() %>%
      
      add_trace(x = PrecipCumulative$TimeSince,
                y = PrecipCumulative$mm_Precip,
                name = 'Precipitation',
                type = 'bar',
                marker = list(color = PrecipCumulative$Colour,
                              line = list(color = line_fill,
                                          width = line_width)),
                hoverlabel = list(bgcolor = fill_bars),
                hoverinfo = 'y'
      ) %>%
      
      
      layout(title = "Cumulative Precipitation",
             
             paper_bgcolor = paper_bg_global,
             plot_bgcolor = plot_bg_global,
             font = title_font_style,
             legend = default_legend,
             hovermode = 'x',
             margin = margins,
             
             xaxis = list(title = "Accumulation in the past...",
                          titlefont = axis_title_fontstyle,
                          ticklen = 8,
                          showgrid = FALSE,
                          showline = TRUE,
                          linecolor = axis_line_color,
                          tickfont = list(size = tick_fontsize),
                          categoryorder = "array",
                          categoryarray = c(PrecipCumulative$TimeSince),
                          autorange = "reversed"),
                          
                          
             yaxis = list(title = "Precipitation (mm)",
                          titlefont = axis_title_fontstyle,
                          showgrid = FALSE,
                          zeroline = FALSE,
                          linecolor = axis_line_color,
                          tickfont = list(size = tick_fontsize))) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  # The half hourly Air Temperature and RH Plot
  # |____   
  
  output$AirTempRHPlot <- renderPlotly( {
    
    # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
    x_begin                 <- input$time_slider[1]
    x_end                   <- input$time_slider[2]
    
   plot_ly(HalfHourlyDisplay_UTMMSdb) %>%
      
      add_trace(x = ~TIMESTAMP, 
                y = ~RH_Avg, 
                type = 'scatter',
                mode = 'lines',
                name = 'RH', 
                yaxis = 'y2',
                line = line_rh,
                hoverlabel = list(bgcolor = fill_rh),
                hoverinfo = 'y') %>%
      
      add_trace(x = ~TIMESTAMP, 
                y = ~AirTemp_Pos, 
                type = 'scatter',
                mode = 'lines',
                fill= 'tozeroy', 
                name = 'Temperature     ', # Added 5 spaces to fix legend cutoff
                connectgaps = FALSE,
                fillcolor = fill_temp, 
                line = list(color = line_fill, 
                            width = line_width),
                hoverlabel = list(bgcolor = fill_temp),
                hoverinfo = 'y') %>%
     
     add_trace(x = ~TIMESTAMP, 
               y = ~AirTemp_Neg, 
               type = 'scatter',
               mode = 'lines',
               fill= 'tozeroy', 
               name = 'Temperature',
               connectgaps = FALSE,
               fillcolor = fill_bars_neg, 
               line = list(color = line_fill, 
                           width = line_width),
               hoverlabel = list(bgcolor = fill_bars_neg),
               hoverinfo = 'y',
               showlegend = FALSE) %>%
      
      layout(title = "Air Temperature and Humidity",
             
             paper_bgcolor = paper_bg_global,
             plot_bgcolor = plot_bg_global,
             
             font = title_font_style,
             
             legend = default_legend,
             
             hovermode = 'x',
             
             margin = margins,
             
             xaxis = list(title = "Date", 
                          range = list(x_begin, x_end),
                          type = 'date',
                          ticklen = 8,
                          showgrid = TRUE,
                          showline = TRUE,
                          linecolor = axis_line_color,
                          titlefont = axis_title_fontstyle,
                          tickfont = list(size = tick_fontsize)),
             
             yaxis = list(title ='Air Temperature (&deg;C)',
                           titlefont = axis_title_fontstyle,
                           tickfont = list(size = tick_fontsize),
                           showgrid = FALSE,
                           overlaying = 'y2',
                           linecolor = axis_line_color,
                           ticks = "outside",
                           range = c(AirTemp_minaxis, AirTemp_maxaxis)),
             
             yaxis2 = list(title = "RH (%)",
                          titlefont = axis_title_fontstyle,
                          side = 'right',
                          range = c(0, 105),
                          linecolor = axis_line_color,
                          ticks = "outside",
                          showgrid = FALSE,
                          tickfont = list(size = tick_fontsize))) %>%
      
      config(displayModeBar = FALSE)
    
  })

  # The Live Air Temperaure and RH Plot
  # |____   
  
output$LiveAirTempRHPlot <- renderPlotly( {
    
    # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
    
    plot_ly(LiveDisplay_UTMMSdb) %>%
      
      add_trace(x = ~TIMESTAMP, 
                y = ~RH_Avg, 
                type = 'scatter',
                mode = 'lines',
                name = 'RH', 
                yaxis = 'y2',
                line = line_rh,
                hoverlabel = list(bgcolor = fill_rh),
                hoverinfo = 'y') %>%
      
      add_trace(x = ~TIMESTAMP, 
                y = ~AirTemp_Avg, 
                type = 'scatter',
                mode = 'lines',
                fill= 'tozeroy', 
                name = 'Temperature     ', # Added 5 spaces to fix legend cutoff
                connectgaps = FALSE,
                fillcolor = fill_temp, 
                line = list(color = line_fill, 
                            width = line_width),
                hoverlabel = list(bgcolor = fill_temp),
                hoverinfo = 'y') %>%
      
      
      layout(title = "Air Temperature and Humidity",
             
             paper_bgcolor = paper_bg_global,
             plot_bgcolor = plot_bg_global,
             
             font = title_font_style,
             
             legend = default_legend,
             
             hovermode = 'x',
             
             margin = margins,
             
             xaxis = list(title = "Date", 
                          range = list((max(LiveDisplay_UTMMSdb$TIMESTAMP - minutes(60))), max(LiveDisplay_UTMMSdb$TIMESTAMP)),
                          type = 'date',
                          ticklen = 8,
                          showgrid = TRUE,
                          showline = TRUE,
                          linecolor = axis_line_color,
                          titlefont = axis_title_fontstyle,
                          tickfont = list(size = tick_fontsize)),
             
             yaxis = list(title ='Air Temperature (&deg;C)',
                          titlefont = axis_title_fontstyle,
                          tickfont = list(size = tick_fontsize),
                          showgrid = FALSE,
                          overlaying = 'y2',
                          linecolor = axis_line_color,
                          ticks = "outside",
                          range = c(AirTemp_minaxis, AirTemp_maxaxis)),
             
             yaxis2 = list(title = "RH (%)",
                           titlefont = axis_title_fontstyle,
                           side = 'right',
                           range = c(0, 105),
                           linecolor = axis_line_color,
                           ticks = "outside",
                           showgrid = FALSE,
                           tickfont = list(size = tick_fontsize))) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  # The Current Temperature Bar Graph
  # |____   
    
  output$CurrentTempBar <- renderPlotly ({

    plot_ly(HalfHourlyDisplay_UTMMSdb) %>%
      add_trace(type = "bar",
                width = bars_width_curcond,
                x = paste0("As of<br>",
                           hour(latest_TimeStamp),
                           ":",
                           ifelse(nchar(minute(latest_TimeStamp)) == 1, 
                                  paste0("0", minute(latest_TimeStamp)), 
                                  minute(latest_TimeStamp)),
                           ifelse(am(latest_TimeStamp), "am (EST)", "pm (EST)")),
                
                y = latest_AirTemp_Avg,
                textposition = "auto",
                text = paste0(round(latest_AirTemp_Avg, 2), "&deg;C"),
                marker = list(color = fill_bars_current_temp,
                              line = list(color = line_fill,
                                          width = line_width))) %>%
      
      layout(title = "Current <br> Temperature",
             
             paper_bgcolor = paper_bg_global,
             plot_bgcolor = plot_bg_global,
             font = title_font_style,
        
             margin = current_conditions_margins,
             
             yaxis = list(title = "&deg;C",
                      titlefont = axis_title_fontstyle,
                      range = c(AirTemp_minaxis, AirTemp_maxaxis),
                      side = "left",
                      showgrid = TRUE,
                      zeroline = TRUE,
                      showline = TRUE,
                      linecolor = axis_line_color,
                      ticks = "outside",
                      tickfont = list(size = tick_fontsize)),
             
             xaxis = list(showline = TRUE,
                          linecolor = axis_line_color,
                          tickfont = list(size = tick_fontsize))
             
             ) %>%
      
      config(displayModeBar = FALSE)
    
    
  })

  # The Current RH Plot
  # |____   
  
  output$CurrentRHBar <- renderPlotly ({
    
    plot_ly(HalfHourlyDisplay_UTMMSdb) %>%
      add_trace(type = "bar",
                width = bars_width_curcond,
                x = paste0("As of<br>",
                           hour(latest_TimeStamp),
                           ":",
                           ifelse(nchar(minute(latest_TimeStamp)) == 1, 
                                  paste0("0", minute(latest_TimeStamp)), 
                                  minute(latest_TimeStamp)),
                           ifelse(am(latest_TimeStamp), "am (EST)", "pm (EST)")),
                
                y = latest_RH_Avg,
                textposition = "auto",
                text = paste0(round(latest_RH_Avg, 1), "%"),
                
                marker = list(color = fill_bars,
                              line = list(color = line_fill,
                                          width = line_width))) %>%
      
      layout(title = "Current <br> Humidity",
             
             paper_bgcolor = paper_bg_global,
             plot_bgcolor = plot_bg_global,
             font = title_font_style,
             
             margin = current_conditions_margins,
             
             yaxis = list(title = "%",
                          titlefont = axis_title_fontstyle,
                          range = c(0, 105),
                          side = "left",
                          showgrid = TRUE,
                          zeroline = TRUE,
                          showline = TRUE,
                          linecolor = axis_line_color,
                          ticks = "outside",
                          tickfont = list(size = tick_fontsize)),
             
             xaxis = list(showline = TRUE,
                          linecolor = axis_line_color,
                          tickfont = list(size = tick_fontsize))
             
      ) %>%
      
      config(displayModeBar = FALSE)
    
    
  }) 
  
  # The Current Net Rad Value
  # |____   

CurrentNetRadValue      <- tail(LiveDisplay_UTMMSdb$NetTot_Avg, 1)

  output$CurrentNetRad <- renderPlotly ({
        
        plot_ly() %>%
              add_trace(type = "bar",
                        width = 10,
                        orientation = 'h',
                        name = "Shortwave",
                        x = CurrentNetRadValue,
                        y = '',
                        hoverinfo = "none",
                        marker = list(color = c('rgba(198, 21, 21, 1)'),
                                      line = list(color = line_fill,
                                                  width = line_width))) %>%
              
              
              layout(title = "Current Net Radiation",
                     paper_bgcolor = paper_bg_global,
                     plot_bgcolor = plot_bg_global,
                     font = title_font_style,
                     margin = current_conditions_margins,
                     
                     xaxis = list(title = "W m<sup>2</sup>",
                                  titlefont = axis_title_fontstyle,
                                  side = "left",
                                  showgrid = TRUE,
                                  zeroline = TRUE,
                                  showline = TRUE,
                                  range = c(0,1000),
                                  linecolor = axis_line_color,
                                  ticks = "outside",
                                  tickfont = list(size = tick_fontsize)),
                     
                     yaxis = list(showline = TRUE,
                                  linecolor = axis_line_color,
                                  tickfont = list(size = tick_fontsize)
                                  
                     )
                     
              ) %>%
              
              add_annotations(
                    text = paste0(CurrentNetRadValue, ' W m<sup>2</sup>'),
                    font = list(color = 'grey35',
                                size = 12),
                    xref = 'x', 
                    yref = 'y',
                    x = (CurrentNetRadValue + 20),
                    yshift = -5,
                    xanchor = 'left',
                    showarrow = FALSE
              ) %>%
              
              config(displayModeBar = FALSE)
        
        
  })  

  # The Current Rad Bar graph
  # |____  
  
  output$CurrentRadBar <- renderPlotly ({
    
    plot_ly() %>%
      add_trace(type = "bar",
                width = bars_width_curcond,
                
                name = "Shortwave",
                
                  x = latest_TotalRad_x,
                  y = latest_TotalRad_y, #latest_SR01,
                  textposition = "auto",
                  hoverinfo = "none",
                  text = paste0(round(latest_TotalRad_y, 1), "W m<sup>2</sup>"),
               
                marker = list(color = c(fill_sw_in, fill_sw_out, fill_lw_in, fill_lw_out),
                              line = list(color = line_fill,
                                          width = line_width))) %>%
                
      

      
      layout(title = "Current Solar <br> Radiation Components",
             
             paper_bgcolor = paper_bg_global,
             plot_bgcolor = plot_bg_global,
             font = title_font_style,
             margin = current_conditions_margins,
             
             yaxis = list(title = "W m<sup>2</sup>",
                          titlefont = axis_title_fontstyle,
                          side = "left",
                          showgrid = TRUE,
                          zeroline = TRUE,
                          showline = TRUE,
                          linecolor = axis_line_color,
                          ticks = "outside",
                          tickfont = list(size = tick_fontsize)),
             
             xaxis = list(showline = TRUE,
                          linecolor = axis_line_color,
                          categoryarray = c("a", "b", "c", "d"), #Stupid hack to reorder the bars
                          categoryorder = "array",
                          tickfont = list(size = tick_fontsize)
                          
             )
             
      ) %>%
      
      config(displayModeBar = FALSE)
    
    
  })  
  

  # The wind rose
  # |____  
  
  output$WindRosePlot <- renderPlotly( {
    
    # Set the limits of the axis to the first and last time stamps in the radiation dataset. This prevents daily aggregate albedo data from resizing the graphs.
    x_begin                 <- input$time_slider[1]
    x_end                   <- input$time_slider[2]
    
    

    plot_ly(LiveDisplay_UTMMSdb) %>%
      
      add_trace(type = 'scatterpolar',
                mode = 'markers',
                r = ~Wind_Speed_m_s, 
                theta = ~Wind_Direction_deg, 
                name = '< 0.5 m/s',
                marker = list(color = ~Wind_Speed_m_s,
                              colorscale = 'Portland',
                              opacity = 0.4,
                              size = 5),
                showlegend = T
                
                ) %>%
      
     layout(title = "T - 1h | Wind Speed 15m Above Ground",
             
             paper_bgcolor = paper_bg_global,
             plot_bgcolor = plot_bg_global,
             
             font = title_font_style,
             
             legend = default_legend,
             
             hovermode = 'none',
             
             margin = margins
             
             ) %>%
      
      config(displayModeBar = FALSE)
    
  })  
  
  
  
    
})
