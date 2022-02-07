# edits to make:
# make text bigger
# make species names more readable
# ####################################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(shiny)
library(cowplot)
library(ggplot2)

scaleFUN <- function(x) sprintf("%.2e", x)
theme_set(
  theme(text=element_text(family="Times"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        plot.title = element_text(size=12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(size = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'top',
        # legend.key = element_rect(fill = NA,size = 0.25),
        legend.background = element_blank(),
        legend.key.width =unit(0.15, units = "cm"),
        legend.key.height =unit(0.15, units = "cm"),
        # axis.text.x = element_blank(),
        # axis.title.y = element_blank(),
        axis.text.x = element_text( angle=-45, hjust = 0.2, vjust = 1),
        legend.text.align= 0,
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
)

sccoos_long <- readr::read_csv("sw_long_shiny_data.csv")


# Define UI for application that draws two line plots
ui = fluidPage(
  titlePanel("Stearns Wharf HAB monitoring"),
  sliderInput(inputId = "Order",
              label = "Date range",
              min = as.Date(min(sccoos_long$day)),
              max = as.Date(max(sccoos_long$day)),
              value = c(as.Date(min(sccoos_long$day)),
                        as.Date(min(sccoos_long$day)))),
  sidebarLayout(position= "left",
                sidebarPanel(
                  checkboxInput("donum1", "Make environmental plot", value = T),
                  checkboxInput("donum2", "Make phytoplankton plot", value = T),
                  selectizeInput("data1",
                                 "Select environmental data:",
                                 choices = c("Temperature (Celcius)",
                                             "Chlorophyll fluorescence (ug/L)",
                                             "Salinity(1e-3)",
                                             "Chl-a extracted (ug/L)",
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
                                 choices = c("Akashiwo_sanguinea (cells/L)",
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
                                 selected = "Pseudo_nitzschia_seriata_group (cells/L)"  ,
                                 multiple = TRUE
                  )

                ),
                mainPanel(
                  plotOutput(outputId="plotgraph", height="600px")
                )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  ### Filter by date
  env_plot <- reactive({
    sccoos_long <- sccoos_long[as.Date(sccoos_long$day) >= input$Order[1] & as.Date(sccoos_long$day) <= input$Order[2] ,]
    plot.env <- sccoos_long[sccoos_long$Variable %in% input$data1, ]
    if (!input$donum1) return(NULL)
    ggplot(plot.env) +
      geom_point(aes(x = day, y = data_vals, colour = Variable), na.rm = T) +
      labs (x = "Time", y = "Values", title = " ") +
      # scale_colour_discrete(name = "Variable")+
      geom_line(data=plot.env[!is.na(plot.env$data_vals),], aes(x = day, y = data_vals, colour = Variable))+
      scale_y_continuous(labels = scaleFUN)
  })

  phyto_plot <- reactive({
    sccoos_long <- sccoos_long[as.Date(sccoos_long$day) >= input$Order[1] & as.Date(sccoos_long$day) <= input$Order[2] ,]
    plot.phyto <- sccoos_long[sccoos_long$Variable %in% input$data2, ]

    if (!input$donum2) return(NULL)
    phyto_plot <-ggplot(plot.phyto) +
      geom_point(aes(x = day, y = data_vals, colour = Variable), na.rm = T) +
      labs (x = "Time", y = "Values", title = " ") +
      # scale_colour_discrete(name = "Variable")+
      geom_line(data=plot.phyto[!is.na(plot.phyto$data_vals),], aes(x = day, y = data_vals, colour = Variable))+
      scale_y_continuous(labels = scaleFUN)
  })

  output$plotgraph = renderPlot({

    ptlist <- list(env_plot(),phyto_plot())
    wtlist <- c(input$wt1,input$wt2)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)

    # cowplot::plot_grid(plotlist = ptlist, ncol=1)
    gridExtra::grid.arrange(grobs=ptlist,widths=wtlist,nrow=length(ptlist))
  })
}




# Run the application
shinyApp(ui = ui, server = server)
