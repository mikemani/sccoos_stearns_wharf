#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
library(shiny)
sccoos_long <- readr::read_csv("sw_long_shiny_data.csv")



# rename variables




# Define UI for application that draws a histogram
ui = fluidPage(
  titlePanel("Stearns Wharf HAB monitoring"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("data",
                     "Select data:",
                     choices = c("Temperature (Celcius)",
                                 "Chlorophyll fluorescence (ug/L)",
                                 "Salinity(1e-3)",
                                 "Chl-a extracted (ug/L)",
                                 "Phosphate (uM)",
                                 "Silicate (uM)",
                                 "Ammonium (uM)",
                                 "Nitrate (uM)",
                                 "pDA (ng/mL)",
                                 "Akashiwo_sanguinea (cells/L)",
                                 "Alexandrium_spp (cells/L)",
                                 "Dinophysis_spp (cells/L)",
                                 "Lingulodinium_polyedra (cells/L)",
                                 "Prorocentrum_spp (cells/L)",
                                 "Pseudo_nitzschia_delicatissima_group (cells/L)",
                                 "Pseudo_nitzschia_seriata_group (cells/L)",
                                 "Ceratium (cells/L)",
                                 "Cochlodinium (cells/L)",
                                 "Gymnodinium_spp (cells/L)",
                                 "Other_Diatoms (cells/L)",
                                 "Other_Dinoflagellates (cells/L)",
                                "Total_Phytoplankton (cells/L)"   ),
                     selected = "Temperature (Celcius)"  ,
                     multiple = TRUE
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot = renderPlot({
    #not sure if input$cnt is a list or a vector
    #may need to manipulate that before passing
    plot.data <- sccoos_long[sccoos_long$Variable %in% input$data, ]
    ggplot(plot.data) +
      geom_line(mapping = aes(x = day, y = data_vals, colour = Variable)) +
      labs (x = "Years", y = "Variable", title = " ") +
      scale_colour_discrete(name = "Variable")
  })
}

# Run the application
shinyApp(ui = ui, server = server)


