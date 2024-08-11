server <- function(input, output, session) {

  output$auth_user <- renderText({
    req(session$userData$user())
    session$userData$user()$email
  })

  observeEvent(input$polish__sign_out, {
    req(session$userData$user()$email)
    sign_out_from_shiny(session)
    session$reload()
  })


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



secure_server(server)
