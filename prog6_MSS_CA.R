# resultado_prog5=resultado_mv

prog_6 <- function(resultado_prog5) {
  set.seed(1991)
  
  library(RColorBrewer)
  datos_p2<-list()
  df_2<-list()
  
  for (i in 1:length(resultado_prog5)) {
    datos_p2[[i]] <- as.data.frame(cbind(seq(1:length(resultado_prog5[[i]]$unHappyMonitor_aux)),
                                         resultado_prog5[[i]]$unHappyMonitor_aux,
                                         resultado_prog5[[i]]$similarMonitor,
                                         resultado_prog5[[i]]$mudanza_var))
    names(datos_p2[[i]])=c("t","uHM","sM",'costoMudanza')
    # df_2[[i]]$costoMudanza=paste("CM=",df_2[[i]]$costoMudanza,sep="")
    df_2[[i]] <- reshape::melt(datos_p2[[i]], id.vars = c("t","costoMudanza"))
  }
  
  df <- do.call(rbind, df_2)
  head(df)
  # df=df %>% mutate(v=paste(variable,"-", costoMudanza))
  df=df %>% mutate(v=factor(paste(variable,paste("CM=",costoMudanza,sep=""), sep="-"),
                            levels=c(c(sprintf("sM-CM=%d", 0:20), sprintf("uHM-CM=%d", 0:20)))))
  
  names(df)=c("T","costoMudanza","variable","Valor","v")
  
  
  n_colors <- length(resultado_prog5)  # Número de colores necesarios
  
  # Generar una paleta de colores en función del número de colores necesarios
  
  if (n_colors == 1) {
    color_palette <- c('blue4', 'green4')
  } else if (n_colors == 2) {
    color_palette <- c('blue4', 'blue3', 'green4', 'green3')
  } else {
    color_palette_1 <- colorRampPalette(brewer.pal(n_colors, "Blues"))(n_colors)
    color_palette_2 <- colorRampPalette(brewer.pal(n_colors, "Greens"))(n_colors)
    color_palette <- c(color_palette_1, color_palette_2)
  }
  
  
  plot <- plot_ly(df, x = ~T, y = ~Valor, color = ~factor(v), colors = color_palette,
                  type = "scatter",
                  mode="lines")

  
  plotly_costomudanza <- plot %>% layout(xaxis = list(title = "Tiempo"), 
                                         yaxis = list(range = c(0, 1),
                                         title = "Proporción"),
                                         legend = list(font = list(size = 12)))
  
  return(plotly_costomudanza)
}
