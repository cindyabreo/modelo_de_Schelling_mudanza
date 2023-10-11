
prog_2_grafico <- function(resultado) {
  library(plotly)
  set.seed(1991)
  
  #datos PLOT 1####
  datos_p1 <- resultado$city
  datos_p1 <- as.matrix(datos_p1)
  colnames(datos_p1) <- paste("c", 1:as.matrix(dim(datos_p1))[1,1],sep="")
  rownames(datos_p1) <- paste("f", 1:as.matrix(dim(datos_p1))[1,1],sep="")
  df <- reshape::melt(datos_p1)
  colnames(df) <- c("x", "y", "valor") 
  df$valor <- as.numeric(df$valor)
  df <- as.data.frame(df)
  head(df)
  
  # graficos PLOT 1####
  plotly_plot1 <- plot_ly(data = df, 
                   x = ~x, y = ~y, 
                   z = ~valor, 
                   type = "heatmap",
                   colors =  c("white", "green4", "black"),
                   showscale = FALSE,
                   hoverinfo  = 'text',
                   text = ~paste(valor)) %>% 
    layout(xaxis = list(tickvals = '', ticktext = '',title =''),
           yaxis= list(tickvals = '', ticktext = '',title =''))
  
  list(plotly_plot1)
}


