library(ggplot2)


function(input, output, session) {
    output$plot1 <- renderPlot({
        if(distrib_typ[distrib_name==input$distName]=="continuous"){
            p <- ggplot(data.frame(x = c(min_lim[distrib_name==input$distName],
                                         max_lim[distrib_name==input$distName])), aes(x)) +
                stat_function(fun = as.character(pdf[distrib_name==input$distName]), 
                              args = c(input$par1, input$par2, input$par3)[!is.na(c(input$par1, input$par2, input$par3))], 
                              colour = "red", geom = "area", fill = "red", alpha = I(0.5)) +
                coord_cartesian(xlim=c(min_lim[distrib_name==input$distName],
                                       max_lim[distrib_name==input$distName]),
                                ylim=c(0,max_lim_y[distrib_name==input$distName]))
            return(p)
        }
        else{
            p <- ggplot(data.frame(x = c(min_lim[distrib_name==input$distName]:
                                         max_lim[distrib_name==input$distName])), aes(x)) +
                stat_function(fun = as.character(pdf[distrib_name==input$distName]), 
                              args = c(input$par1, input$par2, input$par3)[!is.na(c(input$par1, input$par2, input$par3))], 
                              colour = "red", geom = "bar", 
                              n=abs(min_lim[distrib_name==input$distName]-max_lim[distrib_name==input$distName])+1, 
                              alpha = I(0.5)) +
                coord_cartesian(xlim=c(min_lim[distrib_name==input$distName],
                                       max_lim[distrib_name==input$distName]),
                                ylim=c(0,max_lim_y[distrib_name==input$distName]))
            return(p)
        }
    })
    
    output$plot2 <- renderPlot({
        if(distrib_typ[distrib_name==input$distName]=="continuous"){
            p <- ggplot(data.frame(x = c(min_lim[distrib_name==input$distName],
                                         max_lim[distrib_name==input$distName])), aes(x)) +
                stat_function(fun = as.character(cdf[distrib_name==input$distName]), 
                              args = c(input$par1, input$par2, input$par3)[!is.na(c(input$par1, input$par2, input$par3))], 
                              colour = "red", size = 1, alpha = I(0.5)) +
                coord_cartesian(xlim=c(min_lim[distrib_name==input$distName],
                                       max_lim[distrib_name==input$distName]))
            return(p)
        }
        else{
            p <- ggplot(data.frame(x = c(min_lim[distrib_name==input$distName]:
                                             max_lim[distrib_name==input$distName])), aes(x)) +
                stat_function(fun = as.character(cdf[distrib_name==input$distName]), 
                              args = c(input$par1, input$par2, input$par3)[!is.na(c(input$par1, input$par2, input$par3))],
                              geom = "step",
                              colour = "red", size = 1, alpha = I(0.5)) +
                coord_cartesian(xlim=c(min_lim[distrib_name==input$distName],
                                       max_lim[distrib_name==input$distName]))
            return(p)
        }
    })
    
    output$sliderPar1 <- renderUI(
        sliderInput("par1", label =  HTML(paste(par1_exp[distrib_name==input$distName])),
                    par1_min[distrib_name==input$distName],
                    par1_max[distrib_name==input$distName],
                    par1_default[distrib_name==input$distName],step = step[distrib_name==input$distName])
    )
    
    output$sliderPar2 <- renderUI({
        if(number_par[distrib_name==input$distName]>=2){
                sliderInput("par2", label =  HTML(paste(par2_exp[distrib_name==input$distName])),
                    par2_min[distrib_name==input$distName],
                    par2_max[distrib_name==input$distName],
                    par2_default[distrib_name==input$distName],step = 0.1)
        }
        else(return())
    })
    
    output$sliderPar3 <- renderUI({
        if(number_par[distrib_name==input$distName]>=3){
            sliderInput("par3", label =  HTML(paste(par2_exp[distrib_name==input$distName])),
                        par3_min[distrib_name==input$distName],
                        par3_max[distrib_name==input$distName],
                        par3_default[distrib_name==input$distName],step = 0.1)
        }
        else(return())
    })
    
    
    output$infoMean <- renderInfoBox(
        infoBox("Mean:",
                switch(input$distName,
                       Normal = input$par1,
                       Weibull = gamma(1+1/input$par1)*input$par2,
                       Poisson = input$par1),
                icon = icon("list"),
                color = "orange",
                fill = TRUE
        )
    )
    gen_dat <- reactive(
        rdf[distrib_name==input$distName]
    )
    output$infoMean_gen <- renderInfoBox(
        infoBox(
            "Mean:", round(mean(gen_dat()$values),4), icon = icon("list"),
            color = "purple", fill = TRUE
        )
    )
    
    output$infoVarience_gen <- renderInfoBox(
        infoBox(
            "Varience:", round(var(gen_dat()$values),2), icon = icon("list"),
            color = "purple", fill = TRUE
        )
    )
    output$infoVarience <- renderInfoBox(
        infoBox("Varience:",
                switch(input$distName,
                       Normal = input$par2,
                       Weibull = (gamma(1+2/input$par1) - gamma(1+1/input$par1))*(input$par2)^2,
                       Poisson = input$par1),
                icon = icon("list"),
                color = "orange",
                fill = TRUE
        )
    )
    
}

