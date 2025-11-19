# Title: Stage 3 Incidence and Prevalence Forecast by ARIMA Shiny Application, prevalence tab
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2025-09-17
# Updated: 2025-09-22
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1




#--- UI ----------------------------------------------------------------------------#
prevalenceUI <- function(id) {
  ns <- NS(id)
  
  card(style = "margin-top: 10px", height = "76vh",
       
       layout_sidebar(
         
         sidebar = sidebar(width = "25%",
                           layout_columns(
                             actionButton(inputId = ns("defs"),
                                          label = "Definitions",
                                          icon = icon("book")),
                             actionButton(inputId = ns("help"),
                                          label = "Help",
                                          icon = icon("circle-question"))
                           ),
                           
                           hr(style = "border-top: 2px solid #363538; !important"),
                           
                           HTML("<span style = 'font-size: 100%; color: #316673'><i>Make selections to customize the map:</i></span>"),
                           
                           prettyRadioButtons(inputId = ns("subset"),
                                              label = HTML("<span style = 'font-size: 115%;'>1. Select subset type:</span>"),
                                              choices = list("Disease Type" = "dis", "Age Category" = "age", "Sex" = "sex"),
                                              inline = FALSE,
                                              status = "info"),
                           
                           conditionalPanel(condition = "input.subset == 'dis'", ns = ns, #JS expression
                                            
                                            prettyRadioButtons(inputId = ns("select_dis"),
                                                               label = HTML("<span style = 'font-size: 115%;'>2. Select disease type:</span>"),
                                                               choices = list("All IBD" = "IBD", "Crohn's Disease" = "CD", "Ulcerative Colitis" = "UC"),
                                                               inline = FALSE,
                                                               status = "info")
                                            
                           ),
                           
                           conditionalPanel(condition = "input.subset == 'age'", ns = ns, #JS expression
                                            
                                            prettyRadioButtons(inputId = ns("select_age"),
                                                               label = HTML("<span style = 'font-size: 115%;'>2. Select age category:</span>"),
                                                               choices = list("Pediatric (<18)" = "Peds (<18)", "Adult (18–64)" = "Adults (18 to 64)", "Seniors (>65)" = "Elderly (65+)"),
                                                               inline = FALSE,
                                                               status = "info")
                                            
                           ),
                           
                           conditionalPanel(condition = "input.subset == 'sex'", ns = ns, #JS expression
                                            
                                            prettyRadioButtons(inputId = ns("select_sex"),
                                                               label = HTML("<span style = 'font-size: 115%;'>2. Select sex:</span>"),
                                                               choices = list("Female", "Male"),
                                                               inline = FALSE,
                                                               status = "info")
                                            
                           ),
                           
                           sliderTextInput(inputId = ns("select_year"),
                                           label = HTML("<span style = 'font-size: 115%;'>3. Select year:</span><br><span style = 'font-size: 90%;color: #316673;'><i>Click on the slider bar to view a specific year or click the play button under time slider to view animation.</i></span>"),
                                           choices = c(seq(1990,2035)), # sliderTextInput used (for formatting reasons)
                                           selected = "2025",
                                           grid = TRUE,
                                           width = "95%",
                                           animate = animationOptions(interval = 2500, loop = FALSE)
                           ),
                           
                           # Implementation of next button: https://stackoverflow.com/questions/61598501/how-to-add-a-next-button-to-the-sliderinput-of-r-shiny
                           
                           layout_columns(
                             downloadButton(ns("download"),
                                            label = "Download .CSV")
                           ),
                           
                           hr(style = "border-top: 2px solid #363538;"),
                           
                           layout_columns(
                             div(class = "logo", tags$a(img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/IOIBD-GIVES_HemsleyLogo.png", height = "95%", width = "95%"), href = "https://helmsleytrust.org/", target = "_blank"), align = "center"),
                             div(class = "logo", tags$a(img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/CANGIEC2%20red-grey%20transparent.png", width = "110%"), href = "https://cangiec.ca/", target = "_blank"), align = "center")
                           ),
                           div(class = "logo", tags$a(img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/logo-cihr-reversed-en.jpeg", width = "75%"), href = "https://cihr-irsc.gc.ca/e/193.html", target = "_blank"), align = "center")
                           
         ), # sidebar
         
         
         leafletOutput(ns("map"), height = "75vh") %>% withSpinner(),
         class = "p-0",
         
         # trend plots
         absolutePanel(
           top = 10, left = "auto", right = 10, bottom = "auto",
           style = "padding: 5px; background: transparent;",
           width = 360, draggable = TRUE,
           card(
             full_screen = TRUE,
             card_body(
               height = 400,
               style = "gap: 0;",
               full_screen = TRUE,
               HTML("<p style = 'font-size: 100%; color: #316673;'><i>Click on a map region to view trend. Click again to remove a region from the plot.</i></p>"),
               plotlyOutput(outputId = ns("plot"))
             )
           )
         )
         
       ) # layout sidebar
  ) # card
}




#--- Server ----------------------------------------------------------------------------#
prevalenceServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    icons <- awesomeIcons(
      icon = "chart-bar",
      iconColor = "black",
      library = "fa",
      markerColor = "orange"
    )
    
    error_message <- HTML("<span style = 'font-size: 250%; color: #408697; text-align: center;'>No data to present. Please make another selection.</span>")
    
    observeEvent(input$help, {
      if (input$help > 0)  {
        showModal(modalDialog(
          title = "How to Use the Interactive Maps",
          HTML("<p style = 'font-size: 150%;'><p><b>Directions</b><br>
                        Make selections in the sidebar to view maps and data for different variable combinations. The map can be animated by clicking the play button beside the slider bar. You can also view a single year by clicking on a particular point in the slider bar.<br><br>
                        For prevalence details, hover mouse over a coloured region. Scroll to zoom in and out of the map.<br><br>
                        Add lines showing the entire trend over time for a region to the line plot by clicking on a region on the map. Remove a line by clicking on the region a second time. Click and drag the cursor over a section of the line plot to zoom in on a portion of the plot. Double click anywhere on the line plot to zoom back out. The vertical line corresponds with the year of data shown on the map. The plot can be expanded using the button on the bottom right corner.<br><br>
                        The complete data set for all regional disease, age, and sex stratifications can be downloaded by clicking on the <i>Download .CSV</i> button at the bottom of the left-hand sidebar.</p>"),
          size = "l",
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      } else{}
      
    }) 
    
    observeEvent(input$defs, {
      if (input$defs > 0)  {
        showModal(modalDialog(
          title = "Useful Definitions",
          HTML("<p style = 'font-size: 100%;'><b>IBD</b>: inflammatory bowel disease<br><br>
                        <b>CD</b>: Crohn's disease<br><br>
                        <b>UC</b>: ulcerative colitis<br><br>
                        <b>incidence</b>: the number of new diagnoses of a particular disease made in a geographic area in a year<br><br>
                        <b>prevalence</b>: the number of people living with a particular disease in a geographic area at a point in time<br><br>
                        <b>study population</b>: population-based surveillance cohorts from nine global regions (Canada, Catalonia, Denmark, Hungary (Veszprém), Israel, New Zealand (Canterbury), Scotland (Lothian), Sweden, and the United States (Olmsted County))
                        </p>"),
          size = "l",
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      } else{}
      
    })
    
    # create dataset based on user input
    prev_react <- reactive({
      if (input$subset == "dis") {
        prev_react <- prev_dis %>% 
          filter(disease_type == input$select_dis,
                 year == input$select_year)
      } else if (input$subset == "age") {
        prev_react <- prev_age %>% 
          filter(agegrp == input$select_age,
                 year == input$select_year)
      } else if (input$subset == "sex") {
        prev_react <- prev_sex %>% 
          filter(sex == input$select_sex,
                 year == input$select_year)
      }
      prev_react %>% filter(name != "Global")
    })


    # create palette with bins that auto-adjust to full rate range
    prev_rates <- data.frame(rate = c(prev_dis$rate, prev_age$rate, prev_sex$rate))


    # create bins using max rate as per above df
    bins <- c(0, round(max(prev_rates$rate)/5), round(2*(max(prev_rates$rate)/5)), round(3*(max(prev_rates$rate)/5)), round(4*(max(prev_rates$rate)/5)), round(5*(max(prev_rates$rate)/5)))


    # create legend breaks to add to leaflet below
    legend <- c(paste0("0.0–", as.character(round(max(prev_rates$rate)/5), digits = 1), ".0"),
                paste0(as.character(round(max(prev_rates$rate)/5) + 0.1), "–", as.character(round(2*(max(prev_rates$rate)/5))), ".0"),
                paste0(as.character(round(2*(max(prev_rates$rate))/5) + 0.1), "–", as.character(round(3*(max(prev_rates$rate)/5))), ".0"),
                paste0(as.character(round(3*(max(prev_rates$rate))/5) + 0.1), "–", as.character(round(4*(max(prev_rates$rate)/5))), ".0"),
                paste0(as.character(round(4*(max(prev_rates$rate))/5) + 0.1), "–", as.character(round(5*(max(prev_rates$rate)/5))), ".0")
    )


    # manual specify bins
    pal <- colorBin(palette = c("#FDE1CC", "#FEAF73", "#D95F02", "#7C3601", "#1D0D00"), domain = prev_rates$rate, bins = bins, pretty = FALSE)

    # base map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 2, maxZoom = 4, zoomControl = FALSE)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -0, lat = 20, zoom = 2) %>%
        addLegend(position = "bottomleft",
                  title = "Prevalence (per 100,000)",
                  colors = c("#FDE1CC", "#FEAF73", "#D95F02", "#7C3601", "#1D0D00"),
                  labels = legend,
                  opacity = 0.55)
    })

    # Ensure polygon loads on initial opening of the prev tab
    observe({outputOptions(output, "map", suspendWhenHidden = FALSE)})
    
    observe({



          label = paste0("<span style = 'font-size: 125%;'><b>Region</b>: ", unique(prev_react()$name),
                         "<br><b>Disease type</b>: ", unique(prev_react()$disease_type), 
                         "<br><b>Age group</b>: ", unique(prev_react()$agegrp), 
                         "<br><b>Sex</b>: ", unique(prev_react()$sex),
                         "<br><b>Year</b>: ", unique(prev_react()$year),
                         "<br><b>Prevalence rate</b>: ", paste0(sprintf("%.1f", round(prev_react()$rate, 1)), " (", sprintf("%.1f", round(prev_react()$lb, 1)), ", ", sprintf("%.1f", round(prev_react()$ub, 1)), ")"),
                         "<br><b>Rate type</b>: ", unique(prev_react()$forecast)) %>% lapply(htmltools::HTML)


        leafletProxy("map", data = prev_react()) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(prev_react()$rate),
                      color = "#363538",
                      weight = 2,
                      stroke = TRUE,
                      layerId = prev_react()$name,
                      label = ~label,
                      fillOpacity = 0.55,
                      highlightOptions = highlightOptions(color = "#363538", weight = 3, bringToFront = FALSE, opacity = 1)) %>%
          clearGroup("labels") %>% 
          addLabelOnlyMarkers(lng = ~marker_long, lat = ~marker_lat, label = ~name,
                              group = "labels",
                              labelOptions = labelOptions(noHide = TRUE,
                                                          direction = "auto",
                                                          textOnly = FALSE,
                                                          opacity = 0.75)) %>%
          removeControl(layerId = "year_control") %>%
          addControl(layerId = "year_control", paste0(HTML("<span style = 'font-size:175%; color: #363538;'>", unique(prev_react()$year), "</span>")), position = "topleft")

    })
    
    ## Plot ------------------------------------------
    
    # generate datasets based on user input
    global_react <- reactive({
      data <- switch(input$subset,
                     "dis" = prev_dis %>% filter(disease_type == input$select_dis),
                     "age" = prev_age %>% filter(agegrp == input$select_age),
                     "sex" = prev_sex %>% filter(sex == input$select_sex))
      data %>% 
        mutate(year = as.numeric(year)) %>% 
        filter(name == "Global")
    })
    
    global_labels <- reactive({
      data <- switch(input$subset,
                     "dis" = prev_dis %>% filter(disease_type == input$select_dis),
                     "age" = prev_age %>% filter(agegrp == input$select_age),
                     "sex" = prev_sex %>% filter(sex == input$select_sex))
      data %>%
        mutate(year = as.numeric(year)) %>% 
        filter(name == "Global",
               year == max(year))
    })
    
    y_max <- reactive({
      if (input$subset == "dis") {
        y_max <- switch(input$select_dis,
                        "IBD" = 1600,
                        "CD" = 600,
                        "UC" = 1100)
      } else if (input$subset == "age"){
        y_max <- switch(input$select_age,
                        "Peds (<18)" = 140,
                        "Adults (18 to 64)" = 2000,
                        "Elderly (65+)" = 2000)
      } else if( input$subset == "sex"){
        y_max = 1700
      }
    })
    
    # build plot
    output$plot <- renderPlotly({
      plot_ly() %>%
        add_text(data = global_labels(),
                 x = ~year, y=~rate,
                 text = "Overall",
                 color = I("#363538"),
                 textposition = "middle right",
                 showlegend = FALSE,
                 inherit = TRUE,
                 hoverinfo = "none") %>% 
        add_trace(data = global_react(),
                  x=~year, y=~rate, 
                  type = "scatter", mode = "lines",
                  line = list(color = "#363538"),
                  name = "Overall") %>% 
        add_annotations(x = input$select_year-1.5, y = 0.9*y_max(),
                        text = "Map Year",
                        textangle = -90,
                        font = list(color = "grey",
                                    size = 10),
                        showarrow = FALSE) %>% 
        layout(title= "Prevalence Temporal Trends",
               xaxis = list(title = "Year",
                            range = c(1990, 2052),
                            dtick = 10),
               yaxis = list(title = "Prevalence (per 100,000)",
                            range = c(10,y_max())),
               shapes = list(
                 list(type = "line",
                      y0=0, y1=1, yref = "paper",
                      x0 = input$select_year, x1 = input$select_year, 
                      line = list(color = "grey", dash = "solid", width = 1))),
               showlegend = FALSE,
               margin = list(t = 50, r = 0, b = 50, l = 50),
               font = list(family = "Ubuntu")) %>% 
        config(displayModeBar = FALSE)
    }) 
    
    # add regional data to plot by clicking on the map
    # linking map_click event to plotly output: https://stackoverflow.com/questions/52024741/linking-leaflets-icons-to-plotly-line-plot-in-shiny
    # storing click event data: https://stackoverflow.com/questions/41106547/how-to-save-click-events-in-leaflet-shiny-map
    RV <- reactiveValues(click_data = character())
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click$id
      
      if(!is.null(click)) {
        if (click %in% RV$click_data) {
          RV$click_data <- setdiff(RV$click_data, click)
        } else {
          RV$click_data <- c(RV$click_data, click)
        }
      }
      
      # generate datasets based on user input
      plot_react <- reactive({
        data <- switch(input$subset,
                       "dis" = prev_dis %>% filter(disease_type == input$select_dis),
                       "age" = prev_age %>% filter(agegrp == input$select_age),
                       "sex" = prev_sex %>% filter(sex == input$select_sex))
        data %>% 
          mutate(year = as.numeric(year)) %>% 
          filter(name != "Global",
                 name %in% RV$click_data)
      })
      
      plot_labels <- reactive({
        data <- switch(input$subset,
                       "dis" = prev_dis %>% filter(disease_type == input$select_dis),
                       "age" = prev_age %>% filter(agegrp == input$select_age),
                       "sex" = prev_sex %>% filter(sex == input$select_sex))
        data %>%
          mutate(year = as.numeric(year)) %>% 
          group_by(name) %>%
          filter(year == max(year),
                 name != "Global",
                 name %in% RV$click_data)
      })
      
      # build plot
      output$plot <- renderPlotly({
        plot_ly() %>%
          add_text(data = global_labels(),
                   x = ~year, y=~rate,
                   text = "Overall",
                   color = I("#363538"),
                   textposition = "middle right",
                   showlegend = FALSE,
                   inherit = TRUE,
                   hoverinfo = "none") %>%
          add_trace(data = global_react(),
                    x=~year, y=~rate, 
                    type = "scatter", mode = "lines",
                    line = list(color = "#363538"),
                    name = "Overall") %>% 
          add_annotations(x = input$select_year-1.5, y = 0.9*y_max(),
                          text = "Map Year",
                          textangle = -90,
                          font = list(color = "grey",
                                      size = 10),
                          showarrow = FALSE) %>% 
          add_text(data = plot_labels(),
                   x = ~year, y = ~rate,
                   text = ~name, 
                   textfont = list(color = ~color),
                   textposition = "middle right",
                   showlegend = FALSE,
                   inherit = TRUE,
                   hoverinfo = "none") %>% 
          add_trace(data = plot_react() %>% filter(forecast == "Observed"),
                    x=~year, y = ~rate,
                    type = "scatter", mode = "lines",
                    line = list(color = ~color, dash = "solid"),
                    name = ~name,
                    hoverinfo = "none") %>%
          add_trace(data = plot_react(),
                    x=~year, y = ~rate,
                    type = "scatter", mode = "lines",
                    line = list(color = ~color, dash = "dot"),
                    name = ~name) %>%
          layout(title= "Prevalence Temporal Trends",
                 xaxis = list(title = "Year",
                              range = c(1990, 2052),
                              dtick = 10),
                 yaxis = list(title = "Prevalence (per 100,000)",
                              range = c(10,y_max())),
                 shapes = list(
                   list(type = "line",
                        y0=0, y1=1, yref = "paper",
                        x0 = input$select_year, x1 = input$select_year, 
                        line = list(color = "grey", dash = "solid", width = 1))),
                 showlegend = FALSE,
                 margin = list(t = 50, r = 0, b = 50, l = 50),
                 font = list(family = "Ubuntu")) %>% 
          config(displayModeBar = FALSE)
      }) 
      
      
    })
    
    # download country data
    output$download <- downloadHandler(
      filename = function(){
        paste("Stage3_prevalence_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file){
        write.csv(prev_dl, file, row.names = FALSE)
      }
    )
    
    
  }) # server
  
  
}
