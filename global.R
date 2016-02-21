
list_dist <- read.csv("distribution_config.csv", header = TRUE, stringsAsFactors = FALSE)
attach(list_dist)



gen_pdf <- function(dist_fun,left_lim,right_lim,par1,par2,par3){
    par_comb <- c(par1,par2,par3)
    par_comb <- par_comb[!is.na(par_comb)]
    p <- ggplot(data.frame(x = c(left_lim, right_lim)), aes(x)) +
        stat_function(fun = dist_fun, 
                      args = par_comb, 
                      colour = "red", 
                      geom = "area", 
                      fill = "red", 
                      alpha = I(0.5)
        )
    return(p)
}

gen_cdf <- function(dist_fun,left_lim,right_lim,par1,par2,par3){
    par_comb <- c(par1,par2,par3)
    par_comb <- par_comb[!is.na(par_comb)]
    p <- ggplot(data.frame(x = c(left_lim, right_lim)), aes(x)) +
        stat_function(fun = dist_fun, 
                      args = par_comb, 
                      colour = "red", 
                      geom = "area", 
                      fill = "red", 
                      alpha = I(0.5)
        )
    return(p)
}
