library(ggplot2)
library(gridExtra)


options(shiny.maxRequestSize = 9 * 1024 ^ 2)

function(input, output, session) {
  ###############################################################################################
  #######################################Regression##############################################
  
  #######################################Simple##################################################
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(file = inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
  })
  
  selected_reg_data_s <- reactive({eval(parse(text = paste(input$s_regression_dat)))})
  
  output$s_reg_depen_var_ui <- renderUI({
    selectInput(
      inputId = "s_reg_depen_var",
      label = "Dependent Variable",
      choices = c(names(selected_reg_data_s()),""),
      selected = "hp"
    )
  })
  
  output$s_reg_indepen_var_ui <- renderUI({
    selectInput(
      inputId = "s_reg_indepen_var",
      label = "Independent Variable",
      choices = c(names(selected_reg_data_s()),""),
      selected = "mpg"
    )
  })
  
  
  s_reg_fit_lm <- reactive({lm(selected_reg_data_s()[,colnames(selected_reg_data_s())==input$s_reg_depen_var]~selected_reg_data_s()[,colnames(selected_reg_data_s())==input$s_reg_indepen_var])})
  
  output$s_regression_par1 <- renderUI({
    sliderInput(
      inputId = "s_regression_par_intercept", 
      label = "Intercept", 
      min = (s_reg_fit_lm()$coe[1]-5),
      max = (s_reg_fit_lm()$coe[1]+5), 
      value = s_reg_fit_lm()$coe[1], 
      step = 0.01
    )
  })
  
  output$s_regression_par2 <- renderUI({
    sliderInput(
      inputId = "s_regression_par_slope", 
      label = "Slope", 
      min = (s_reg_fit_lm()$coe[2]-5) ,
      max = (s_reg_fit_lm()$coe[2]+5), 
      value = s_reg_fit_lm()$coe[2], 
      step = 0.01
    )
  })
  
#   output$s_regression_main <- renderPlot({
#     plot(selected_reg_data_s()[,colnames(selected_reg_data_s())==input$s_reg_indepen_var],
#          selected_reg_data_s()[,colnames(selected_reg_data_s())==input$s_reg_depen_var],
#          type = "p", pch = 20,col="gray",
#          xlab=input$s_reg_indepen_var,
#          ylab=input$s_reg_depen_var
#     )
#     if(input$fit_s_reg)abline(s_reg_fit_lm())
#       else abline(a = input$s_regression_par_intercept, b = input$s_regression_par_slope)
#     for(i in 1:nrow(selected_reg_data_s())){
#       lines(rep(selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_indepen_var],2),
#             c(input$s_regression_par_intercept + input$s_regression_par_slope*selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_indepen_var],
#               selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_depen_var]),
#             col = "red"
#       )
#     }
#   })

  output$s_regression_main <- renderPlot({
    p <- ggplot(data = selected_reg_data_s(), mapping = aes(x=eval(parse(text = paste(input$s_reg_indepen_var))), y=eval(parse(text = paste(input$s_reg_depen_var)))))+
      geom_point(size=2)+
      geom_abline(intercept = input$s_regression_par_intercept, slope = input$s_regression_par_slope, color = "blue")+
      xlab(input$s_reg_indepen_var)+
      ylab(input$s_reg_depen_var)
    
    err_res <- c()
    
    for(i in 1:nrow(selected_reg_data_s())){
      p <- p + geom_line(aes(x=x,y=y, color= "red"),
                         data = data.frame(x=rep(selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_indepen_var],2),
                                           y=c(input$s_regression_par_intercept + input$s_regression_par_slope*
                                                 selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_indepen_var],
                                               selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_depen_var])
                                )
               )
        
      err_res[i] <- selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_depen_var]-
        (input$s_regression_par_intercept + input$s_regression_par_slope*
           selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_indepen_var])
    }
    
    p <- p + theme(legend.title=element_blank(), legend.text = element_text())+
      scale_colour_manual(values=c("red"), breaks=c("red"), labels=c("Residuals"))
    
    err_res <- as.data.frame(err_res)
    
    ress_hist <- ggplot(mapping = aes(x=err_res), data = err_res)+
      geom_histogram(aes(y=..density..), binwidth = 10)+
      geom_density()+
      xlab("Residuals")
    
    ress_qq <- ggplot(mapping = aes(sample = err_res), data = err_res) +
      geom_point(stat = "qq")+
      geom_abline(intercept = 0, slope = 45)
    
    ress_fit <- ggplot(data = data.frame(err_res,y = selected_reg_data_s()[,colnames(selected_reg_data_s())==input$s_reg_depen_var]),
                       mapping = aes(x=y,y=err_res))+
      geom_point(size=1)+
      geom_abline(intercept = 0, slope = 0)
    
    ress <- grid.arrange(ress_fit,ress_qq, ress_hist, ncol=3, nrow=1, widths =c(3, 3, 3))
    
    return(grid.arrange(p, ress, ncol=1, nrow=2, heights =c(3, 2)))
  })
  
  ######################################Multiple#################################################

  selected_reg_data_m <- reactive({eval(parse(text = paste(input$m_regression_dat)))})
  
  output$m_reg_depen_var_ui <- renderUI({
    selectInput(
      inputId = "m_reg_depen_var",
      label = "Dependent Variable",
      choices = c(names(selected_reg_data_m()),""),
      selected = ""
    )
  })
  
  output$m_reg_indepen_var_ui <- renderUI({
    checkboxGroupInput(
      inputId = "m_reg_indepen_var",
      label = "Independent Variable",
      choices = c(names(selected_reg_data_m())[names(selected_reg_data_m())!=input$m_reg_depen_var]),
      inline = TRUE
    )
  })
  

  m_reg_fit_lm <- reactive({lm(selected_reg_data_m()[,colnames(selected_reg_data_m())==input$m_reg_depen_var]
                               ~ selected_reg_data_m()[,colnames(selected_reg_data_m())==input$m_reg_indepen_var]
                            )
  })
#   
#   output$s_regression_par1 <- renderUI({
#     sliderInput(
#       inputId = "s_regression_par_intercept", 
#       label = "Intercept", 
#       min = (s_reg_fit_lm()$coe[1]-5),
#       max = (s_reg_fit_lm()$coe[1]+5), 
#       value = s_reg_fit_lm()$coe[1], 
#       step = 0.01
#     )
#   })
#   
#   output$s_regression_par2 <- renderUI({
#     sliderInput(
#       inputId = "s_regression_par_slope", 
#       label = "Slope", 
#       min = (s_reg_fit_lm()$coe[2]-5) ,
#       max = (s_reg_fit_lm()$coe[2]+5), 
#       value = s_reg_fit_lm()$coe[2], 
#       step = 0.01
#     )
#   })
#   
#   output$s_regression_main <- renderPlot({
#     plot(selected_reg_data_s()[,colnames(selected_reg_data_s())==input$s_reg_indepen_var],
#          selected_reg_data_s()[,colnames(selected_reg_data_s())==input$s_reg_depen_var],
#          type = "p", pch = 20,col="gray",
#          xlab=input$s_reg_indepen_var,
#          ylab=input$s_reg_depen_var
#     )
#     if(input$fit_s_reg)abline(s_reg_fit_lm())
#     else abline(a = input$s_regression_par_intercept, b = input$s_regression_par_slope)
#     for(i in 1:nrow(selected_reg_data_s())){
#       lines(rep(selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_indepen_var],2),
#             c(input$s_regression_par_intercept + input$s_regression_par_slope*selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_indepen_var],
#               selected_reg_data_s()[i,colnames(selected_reg_data_s())==input$s_reg_depen_var]),
#             col = "red"
#       )
#     }
#   })
  
  
  
  
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