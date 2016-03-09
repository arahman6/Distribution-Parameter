library(ggplot2)
options(shiny.maxRequestSize = 9 * 1024 ^ 2)

function(input, output, session) {
  ###############################################################################################
  #######################################Regression##############################################
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(
      file = inFile$datapath, header = input$header, sep = input$sep, quote = input$quote
    )
  })
  
  selected_reg_data1 <- reactive({paste(input$regression_dat)})
  selected_reg_data2 <- reactive({eval(parse(as.character(selected_reg_data1())))})
  
  output$s_reg_depen_var_ui <- renderUI({
    selectInput(
      inputId = "s_reg_depen_var",
      label = "Dependent Variable",
      choices = c(names(selected_reg_data2()),""),
      selected = ""
    )
  })
  
  output$s_reg_indepen_var_ui <- renderUI({
    selectInput(
      inputId = "s_reg_indepen_var",
      label = "Independent Variable",
      choices = c(names(selected_reg_data2()),""),
      selected = ""
    )
  })
  
  
  # s_reg_fit_lm <- lm()
  
  
  
  
  
  
  
  
  
  ###############################################################################################
  #####################################Distribution Parameter####################################
  index.p <- reactive(which(distrib_name == input$distName))
  output$plot1 <- renderPlot({
    if (input$distName == "") {
      return()
    }
    else{
      if (distrib_typ[index.p()] == "continuous") {
        p <- ggplot(data.frame(x = c(min_lim[index.p()],
                                     max_lim[index.p()])), aes(x)) +
          stat_function(
            fun = as.character(pdf.dist[index.p()]),
            args = c(input$par1, input$par2, input$par3)[!is.na(c(input$par1, input$par2, input$par3))],
            colour = "red", geom = "area", fill = "red", alpha = I(0.5)
          ) +
          stat_function(
            fun = as.character(pdf.dist[index.p()]),
            args = c(input$par1_2, input$par2_2, input$par3_2)[!is.na(c(input$par1_2, input$par2_2, input$par3_2))],
            colour = "blue", geom = "area", fill = "blue", alpha = I(0.5)
          ) +
          coord_cartesian(xlim = c(min_lim[index.p()],
                                   max_lim[index.p()]),
                          ylim = c(0,max_lim_y[index.p()]))
        return(p)
      }
      else{
        p <-
          ggplot(data.frame(x = c(min_lim[index.p()]:max_lim[index.p()])), aes(x)) +
          stat_function(
            fun = as.character(pdf.dist[index.p()]),
            args = c(input$par1, input$par2, input$par3)[!is.na(c(input$par1, input$par2, input$par3))],
            colour = "red", geom = "bar", fill = "red",
            n = abs(min_lim[index.p()] - max_lim[index.p()]) + 1,
            alpha = I(0.5)
          ) +
          stat_function(
            fun = as.character(pdf.dist[index.p()]),
            args = c(input$par1_2, input$par2_2, input$par3_2)[!is.na(c(input$par1_2, input$par2_2, input$par3_2))],
            colour = "blue", geom = "bar", fill = "blue",
            n = abs(min_lim[index.p()] - max_lim[index.p()]) + 1,
            alpha = I(0.5)
          ) +
          coord_cartesian(xlim = c(min_lim[index.p()],
                                   max_lim[index.p()]),
                          ylim = c(0,max_lim_y[index.p()]))
        return(p)
      }
    }
  })
  
  output$plot2 <- renderPlot({
    if (input$distName == "") {
      return()
    }
    else{
      if (distrib_typ[index.p()] == "continuous") {
        p <- ggplot(data.frame(x = c(min_lim[index.p()],
                                     max_lim[index.p()])), aes(x)) +
          stat_function(
            fun = as.character(cdf.dist[index.p()]),
            args = c(input$par1, input$par2, input$par3)[!is.na(c(input$par1, input$par2, input$par3))],
            colour = "red", size = 1, alpha = I(0.5)
          ) +
          stat_function(
            fun = as.character(cdf.dist[index.p()]),
            args = c(input$par1_2, input$par2_2, input$par3_2)[!is.na(c(input$par1_2, input$par2_2, input$par3_2))],
            colour = "blue", size = 1, alpha = I(0.5)
          ) +
          coord_cartesian(xlim = c(min_lim[index.p()],
                                   max_lim[index.p()]))
        return(p)
      }
      else{
        p <-
          ggplot(data.frame(x = c(min_lim[index.p()]:max_lim[index.p()])), aes(x)) +
          stat_function(
            fun = as.character(cdf.dist[index.p()]),
            args = c(input$par1, input$par2, input$par3)[!is.na(c(input$par1, input$par2, input$par3))],
            geom = "step",
            colour = "red", size = 1, alpha = I(0.5)
          ) +
          stat_function(
            fun = as.character(cdf.dist[index.p()]),
            args = c(input$par1_2, input$par2_2, input$par3_2)[!is.na(c(input$par1_2, input$par2_2, input$par3_2))],
            geom = "step",
            colour = "blue", size = 1, alpha = I(0.5)
          ) +
          coord_cartesian(xlim = c(min_lim[index.p()],
                                   max_lim[index.p()]))
        return(p)
      }
    }
  })
  
  output$sliderPar1_1 <- renderUI({
    if (input$distName == "") {
      return()
    }
    else{
      sliderInput(
        "par1", label =  HTML(paste(par1_exp[index.p()])),
        par1_min[index.p()],
        par1_max[index.p()],
        par1_default[index.p()],step = step_par1[index.p()]
      )
    }
  })
  
  output$sliderPar2_1 <- renderUI({
    if (input$distName == "") {
      return()
    }
    else{
      if (number_par[index.p()] >= 2) {
        sliderInput(
          "par2", label =  HTML(paste(par2_exp[index.p()])),
          par2_min[index.p()],
          par2_max[index.p()],
          par2_default[index.p()],step = step_par2[index.p()]
        )
      }
      else
        (return())
    }
  })
  
  output$sliderPar3_1 <- renderUI({
    if (input$distName == "") {
      return()
    }
    else{
      if (number_par[index.p()] >= 3) {
        sliderInput(
          "par3", label =  HTML(paste(par2_exp[index.p()])),
          par3_min[index.p()],
          par3_max[index.p()],
          par3_default[index.p()],step = step_par3[index.p()]
        )
      }
      else
        (return())
    }
  })
  
  output$sliderPar1_2 <- renderUI({
    if (input$distName == "") {
      return()
    }
    else{
      sliderInput(
        "par1_2", label =  HTML(paste(par1_exp[index.p()])),
        par1_min[index.p()],
        par1_max[index.p()],
        par1_default[index.p()],step = step_par1[index.p()]
      )
    }
  })
  
  output$sliderPar2_2 <- renderUI({
    if (input$distName == "") {
      return()
    }
    else{
      if (number_par[index.p()] >= 2) {
        sliderInput(
          "par2_2", label =  HTML(paste(par2_exp[index.p()])),
          par2_min[index.p()],
          par2_max[index.p()],
          par2_default[index.p()],step = step_par2[index.p()]
        )
      }
      else
        (return())
    }
  })
  
  output$sliderPar3_2 <- renderUI({
    if (input$distName == "") {
      return()
    }
    else{
      if (number_par[index.p()] >= 3) {
        sliderInput(
          "par3_2", label =  HTML(paste(par2_exp[index.p()])),
          par3_min[index.p()],
          par3_max[index.p()],
          par3_default[index.p()],step = step_par3[index.p()]
        )
      }
      else
        (return())
    }
  })
  
  range_x <- reactive(c(min_lim[index.p()]:max_lim[index.p()]))
  
  text_script_mean1 <- reactive(if (input$distName == "") {
    return()
  }
  else{
    eval(parse(
      text = paste(
        "round(sum(",
        pdf.dist[index.p()],
        "(range_x(),",
        input$par1,
        if (number_par[index.p()] >= 2) {
          paste(",",input$par2)
        },
        if (number_par[index.p()] >= 3) {
          paste(",",input$par3)
        },
        ")*range_x()),2)",
        sep = ""
      )
    ))
  })
  
  
  text_script_var1 <- reactive(if (input$distName == "") {
    return()
  }
  else{
    eval(parse(
      text = paste(
        "round(sum(",
        pdf.dist[index.p()],
        "(range_x(),",
        input$par1,
        if (number_par[index.p()] >= 2) {
          paste(",",input$par2)
        },
        if (number_par[index.p()] >= 3) {
          paste(",",input$par3)
        },
        ")*((range_x() - text_script_mean1()) ^ 2)),2)",
        sep = ""
      )
    ))
  })
  
  text_script_mean2 <- reactive(if (input$distName == "") {
    return()
  }
  else{
    eval(parse(
      text = paste(
        "round(sum(",
        pdf.dist[index.p()],
        "(range_x(),",
        input$par1_2,
        if (number_par[index.p()] >= 2) {
          paste(",",input$par2_2)
        },
        if (number_par[index.p()] >= 3) {
          paste(",",input$par3_2)
        },
        ")*range_x()),2)",
        sep = ""
      )
    ))
  })
  
  
  text_script_var2 <- reactive(if (input$distName == "") {
    return()
  }
  else{
    eval(parse(
      text = paste(
        "round(sum(",
        pdf.dist[index.p()],
        "(range_x(),",
        input$par1_2,
        if (number_par[index.p()] >= 2) {
          paste(",",input$par2_2)
        },
        if (number_par[index.p()] >= 3) {
          paste(",",input$par3_2)
        },
        ")*((range_x() - text_script_mean2()) ^ 2)),2)",
        sep = ""
      )
    ))
  })
  
  output$infoMean1 <- renderInfoBox({
    infoBox(
      "Mean:(red)",
      text_script_mean1(),
      icon = icon("list"),
      color = "orange",
      fill = TRUE
    )
  })
  
  
  output$infoVarience1 <- renderInfoBox({
    infoBox(
      "Varience:(red)",
      text_script_var1(),
      icon = icon("list"),
      color = "orange",
      fill = TRUE
    )
  })
  
  output$infoMean2 <- renderInfoBox({
    infoBox(
      "Mean:(blue)",
      text_script_mean2(),
      icon = icon("list"),
      color = "orange",
      fill = TRUE
    )
  })
  
  
  output$infoVarience2 <- renderInfoBox({
    infoBox(
      "Varience:(blue)",
      text_script_var2(),
      icon = icon("list"),
      color = "orange",
      fill = TRUE
    )
  })
}