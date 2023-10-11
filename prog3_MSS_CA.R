prog_3_grafico <- function(resultado){
  set.seed(1991)
  
  plot1_list <- list()

    for(i in 1:length(resultado$city_aux)){
    datos_p1 <-  as.data.frame(resultado$city_aux[[i]])
    datos_p1 <- as.matrix(datos_p1)
    colnames(datos_p1) <- paste("c", 1:as.matrix(dim(datos_p1))[1,1],sep="")
    rownames(datos_p1) <- paste("f", 1:as.matrix(dim(datos_p1))[1,1],sep="")
    df <- reshape::melt(datos_p1)
    colnames(df) <- c("x", "y", "valor") 
    df$valor <- as.numeric(df$valor)
    df <- as.data.frame(df)
    
    plotly1 <- plot_ly(data = df, 
                     x = ~x, y = ~y, 
                     z = ~valor, 
                     type = "heatmap",
                     colors =  c("white", "green4", "black"),
                     showscale = FALSE,
                     hoverinfo  = 'text',
                     text = ~paste(valor)) %>% 
      layout(xaxis = list(tickvals = '', ticktext = '',title =''),
             yaxis= list(tickvals = '', ticktext = '',title =''))
    
    plot1_list[[i]] <- plotly1

        }
  
  return(plot1_list)
}

