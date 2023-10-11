prog_10 <- function(out1,out2){
  set.seed(1991)
  
  #grafico DENSIDAD FIJA Y ALIKEPREF VARIABLE----
  library(colorspace)
  library(RColorBrewer)
  library(plotly)
  library(dplyr)
  
  names(out1)=c("sM","uHM",'costoMudanza',"alikePref")
  df_out1 <- reshape::melt(out1, id.vars = c("alikePref","costoMudanza"))
  
  df_out1=as.data.frame(df_out1)
  # df_out1=df_out1 %>% mutate(v=paste(variable,"-", costomudanza))
  df_out1=df_out1 %>% mutate(v=factor(paste(variable,paste("CM=",costoMudanza,sep=""), sep="-"),
                            levels=c(c(sprintf("sM-CM=%d", 1:20), sprintf("uHM-CM=%d", 1:20)))))
  
  names(df_out1)=c("alikePref","costoMudanza","variable","Valor","v")
  
  n_colors <- length(unique(out1$costoMudanza))  # Número de colores necesarios
  
  # Generar una paleta de colores en función del número de colores necesarios
  
  if (n_colors == 1) {
    color_palette <- c('blue4', 'red4')
  } else if (n_colors == 2) {
    color_palette <- c('blue4', 'blue3', 'red4', 'red3')
  } else {
    color_palette_1 <- colorRampPalette(brewer.pal(n_colors, "Blues"))(n_colors)
    color_palette_2 <- colorRampPalette(brewer.pal(n_colors, "Reds"))(n_colors)
    color_palette <- c(color_palette_1, color_palette_2)
  }
  
  
  plotly_alikePref_var <- plot_ly(df_out1, x = ~alikePref, y = ~Valor, color = ~factor(v), 
                                  colors = color_palette,
                                  type = "scatter",
                                  mode="lines")

  plotly_alikePref_var <- plotly_alikePref_var %>%
    layout(xaxis = list(title = "Umbral de tolerancia"),
           yaxis = list(title = "Proporción"),
           legend = list(font = list(size = 12)))
  
  #grafico DENSIDAD variable Y ALIKEPREF fijo----
  names(out2)=c("sM","uHM",'costoMudanza',"density")
  df_out2 <- reshape::melt(out2, id.vars = c("density","costoMudanza"))
  
  df_out2=as.data.frame(df_out2)
  # df_out2=df_out2 %>% mutate(v=paste(variable,"-", costomudanza))
  df_out2=df_out2 %>% mutate(v=factor(paste(variable,paste("CM=",costoMudanza,sep=""), sep="-"),
                                      levels=c(c(sprintf("sM-CM=%d", 1:20), sprintf("uHM-CM=%d", 1:20)))))
  
  names(df_out2)=c("density","costoMudanza","variable","Valor","v")
  
  n_colors <- length(unique(out2$costoMudanza))  # Número de colores necesarios
  
  # Generar una paleta de colores en función del número de colores necesarios
  
  if (n_colors == 1) {
    color_palette <- c('blue4', 'red4')
  } else if (n_colors == 2) {
    color_palette <- c('blue4', 'blue3', 'red4', 'red3')
  } else {
    color_palette_1 <- colorRampPalette(brewer.pal(n_colors, "Blues"))(n_colors)
    color_palette_2 <- colorRampPalette(brewer.pal(n_colors, "Reds"))(n_colors)
    color_palette <- c(color_palette_1, color_palette_2)
  }
  
  
  plotly_densityRange_var <- plot_ly(df_out2, 
                                     x = ~density, 
                                     y = ~Valor, 
                                     color = ~factor(v), 
                                     colors = color_palette,
                                     type = "scatter",
                                     mode="lines")
  
  plotly_densityRange_var <- plotly_densityRange_var %>%
    layout(xaxis = list(title = "Densidad de la ciudad"),
           yaxis = list(title = "Proporción"),
           legend = list(font = list(size = 12)))

  return(list(plotly_alikePref_var,
              plotly_densityRange_var))
}
