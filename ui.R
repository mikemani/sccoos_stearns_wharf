ui = fluidPage(
  titlePanel("Stearns Wharf HAB monitoring"),
  sliderInput(inputId = "Order",
              label = "Date range",
              min = as.Date(min(sccoos_long$day)),
              max = as.Date(max(sccoos_long$day)),
              value = c(as.Date(min(sccoos_long$day)),
                        as.Date(max(sccoos_long$day)))),
  sidebarLayout(position= "left",
                sidebarPanel(
                  checkboxInput("donum1", "Make environmental plot", value = T),
                  checkboxInput("donum2", "Make phytoplankton plot", value = T),
                  selectizeInput("data1",
                                 "Select environmental data:",
                                 choices = c("Temperature (Celcius)",
                                             "Chlorophyll a fluorescence (ug/L)"= "Chlorophyll fluorescence (ug/L)",
                                             "Salinity(1e-3)",
                                             "Chlorophyll a extracted (ug/L)"="Chl-a extracted (ug/L)",
                                             "Phosphate (uM)",
                                             "Silicate (uM)",
                                             "Ammonium (uM)",
                                             "Nitrate (uM)",
                                             "pDA (ng/mL)"),
                                 selected = "Temperature (Celcius)"  ,
                                 multiple = TRUE
                  ),
                  selectizeInput("data2",
                                 "Select phytoplankton data:",
                                 choices = c("Akashiwo sanguinea (cells/L)"= "Akashiwo_sanguinea (cells/L)",
                                             "Alexandrium (cells/L)" = "Alexandrium_spp (cells/L)",
                                             "Dinophysis (cells/L)"= "Dinophysis_spp (cells/L)",
                                             "Lingulodinium polyedra (cells/L)" ="Lingulodinium_polyedra (cells/L)",
                                             "Prorocentrum (cells/L)"="Prorocentrum_spp (cells/L)",
                                             "Pseudo nitzschia delicatissima (cells/L)"="Pseudo_nitzschia_delicatissima_group (cells/L)",
                                             "Pseudo nitzschia seriata (cells/L)"="Pseudo_nitzschia_seriata_group (cells/L)",
                                             "Ceratium (cells/L)",
                                             "Cochlodinium (cells/L)",
                                             "Gymnodinium spp (cells/L)"= "Gymnodinium_spp (cells/L)",
                                             "Other Diatoms (cells/L)"="Other_Diatoms (cells/L)",
                                             "Other Dinoflagellates (cells/L)"="Other_Dinoflagellates (cells/L)",
                                             "Total Phytoplankton (cells/L)" = "Total_Phytoplankton (cells/L)"   ),
                                 selected = "Pseudo_nitzschia_seriata_group (cells/L)"  ,
                                 multiple = TRUE
                  )

                ),
                mainPanel(
                  plotOutput(outputId="plotgraph", height="600px")
                )
  )
)

secure_ui(ui)
