# Title: Stage 3 Incidence and Prevalence Forecast by ARIMA Shiny Application
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2025-09-17
# Updated: 2025-09-22
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1


# link: 


# version notes:


source("global.R", local = TRUE)$value
source("incidence.R", local = TRUE)$value
source("prevalence.R", local = TRUE)$value


#--- UI ----------------------------------------------------------------------------#
ui <- page_fillable(
  
  title = "Stage 3 Incidence and Prevalence",
  
  useShinyjs(),
  
  theme = bs_theme(bootswatch = "united",
                   primary = "#408697",
                   base_font = "Ubuntu"),
  
  # format link style
  tags$head(tags$style(HTML("a {color: #408697}"))),
  # format action button
  tags$head(tags$style(HTML(".btn {color:rgb(255,255,255); border-color: #408697; background-color: #408697;}"))),
  # format logo placement
  tags$head(tags$style(HTML(".logo {margin-left: 10px; margin-right: 10px; margin-bottom: 20px; vertical-align: middle;}"))),
  # format slider bar (for eachslider bar, withincreasing increments on js)
  #chooseSliderSkin("Shiny", color = "transparent"),
  tags$head(tags$style(HTML(".js-irs-0 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-0 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-0 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-1 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-1 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-1 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-2 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-2 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-2 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-3 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-3 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-3 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-4 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-4 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-4 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-5 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-5 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-5 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-6 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-6 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-6 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-7 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-7 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-7 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-8 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-8 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-8 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  tags$head(tags$style(HTML(".js-irs-9 .irs-bar {background-color: transparent; border-color: transparent}"))),
  tags$head(tags$style(HTML(".js-irs-9 .irs-single {color: black; background: #BEC4C6}"))),
  tags$head(tags$style(HTML(".js-irs-9 .irs-grid-text {transform: rotate(45deg) translate(2px)}"))),
  # format slider input play button
  tags$head(tags$style(HTML(".slider-animate-button {font-size: 20px; color: #52D6F4; margin-right: -20px;}"))),
  # format Leaflet popup font
  tags$head(tags$style(HTML(".leaflet-pane {font-family: Ubuntu;}
                               .leaflet .info {font-family: Ubuntu;}"))),
  # remove Leaflet control which interfers with sidebar
  tags$head(tags$style(HTML(".leaflet-left .leaflet-control{margin-left: 60px; opacity: 75%;}"))), #visibility: hidden; background-color: transparent;
  # See for issues with z-index for leaflet-containers: https://github.com/rstudio/bslib/issues/955
  tags$head(tags$style(HTML('.leaflet-container {z-index: 0;}'))),
  # Sidebar transparency for mobile
  tags$head(tags$style(HTML('.bslib-gap-spacing .sidebar {background: rgba(212, 218, 220, 0.7)}'))),
  # change sidebar collapse arrow to make more visible
  assignInNamespace(
    "collapse_icon", 
    function() {
      bsicons::bs_icon(
        "chevron-double-left", class = "collapse-icon", size = NULL
      ) 
    },
    ns = "bslib"
  ),
  tags$head(tags$style(HTML('.bslib-sidebar-layout .collapse-toggle .collapse-icon {fill: #000000 !important;}'))),
  # resizable panel for plots
  # tags$head(
  #   tags$style(HTML("
  #     .resizable-panel {
  #       resize: both; 
  #       overflow: auto;
  #       min-width: 100px;
  #       min-height: 100px;
  #     }
  #   "))
  # ),
  
  tags$head(
    tags$style(HTML("
      .resizable-panel {
        position: relative;
        resize: both;
        overflow: auto;
        min-width: 300px;
        min-height: 300px;
        max-width: 600px;
        max-height: 600px;
      }
    "))
  ),
  
  
  # Header
  div(style = "color: #F6F6F6; background-color: #363538; margin: -20px -20px 0px -20px; padding: 25px 20px 5px 20px;",
      layout_columns(
        col_widths = c(10, 2),
        tags$h2("Forecasting the incidence and prevalence of inflammatory bowel disease:",
                tags$h4(style = "margin-top: -0.5em;","An analysis of stage 3 countries")),
        layout_column_wrap(
          div(align = "right",
              tags$a(
                actionButton(
                  width = "125px",
                  inputId = "paper_share",
                  label = "Paper",
                  icon = icon("link")),
                href = "", #ADD ON PUBLICATION
                target = "_blank"
              ) 
          ),
          div(align = "right",
              tags$a(
                actionButton(
                  width = "125px",
                  inputId = "contact us",
                  label = "Contact",
                  icon = icon("envelope")),
                href = "mailto:kaplan.lab@ucalgary.ca"
              )
          )
        )
      )
  ),
  
  
  # Tabs
  navset_tab(
    
    nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Incidence"),
              incidenceUI("incidence")         
    ),
    
    nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Prevalence"),
              prevalenceUI("prevalence")
    )
  )
)



#--- Server ----------------------------------------------------------------------------#
server <- function(input, output, session) {
  
  incidenceServer("incidence")
  prevalenceServer("prevalence")
  
  
  showModal(modalDialog(
    title = "Welcome!",
    HTML("<p style = 'font-size: 100%;'>Thank you for visiting our data repository.<br><br>
                    <b>To cite:</b><br>
                    <i>S. Coward - Manuscript under review. Citation will be updated after publication.</i><br><br>
                    <b>To contact:</b><br>
                    For more information about this project, please refer to the publication above or feel free to contact us with the email subject line \"Stage 3 incidence and prevalence (ARIMA) data repository\" using the <i>Contact</i> button at the top of the webpage.</p>"),
    size = "l",
    footer = modalButton("Close"),
    easyClose = TRUE)
  )
  
  
  
}


shinyApp(ui, server)
