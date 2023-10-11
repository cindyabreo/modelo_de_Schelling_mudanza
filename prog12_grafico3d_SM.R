setwd("C:/Users/cindy/OneDrive/Escritorio/iesta/shiny_MSS_CA_v2/programas_MSS_CA")
dir()

source("prog4_MSS_CA.R")

mudanza_var=seq(0,12,2)
alikePref_var=seq(0,1,0.1)
density=0.7
years=50
citySize=51
capital=c(2,2)
ahorro=c(2,2)
j=11
i=1

resultado_prog11=list()

for (j in 1:length(alikePref_var)) {
  temp_list <- list()  # Crear una lista temporal para almacenar los resultados de prog_11_aux
  for (i in 1:length(mudanza_var)) {
    temp_list[[i]] <- prog_4_aux(years, density, alikePref=alikePref_var[j], 
                                 citySize, capital, ahorro, mudanza_var[i])
  }
  resultado_prog11[[j]] <- temp_list  # Asignar la lista temporal como un elemento de resultado_prog11
}

resultado_prog11[[11]][[1]]


vector_mudanza_var <- numeric(0)
for (i in 1:11) {
  for (j in 1:7) {
    mudanza_valor <- resultado_prog11[[i]][[j]]$mudanza_var
    vector_mudanza_var <- c(vector_mudanza_var, mudanza_valor)
  }
}

vector_mudanza_var=c(rep(0,50),rep(2,50),rep(4,50),rep(6,50),rep(8,50),rep(10,50),rep(12,50))
# vector_mudanza_var=rep(vector_mudanza_var,each = 50)

vector_alikePref_var <- numeric(0)
for (i in 1:11) {
  for (j in 1:7) {
    alikePref_valor <- resultado_prog11[[i]][[j]]$alikePref
    vector_alikePref_var <- c(vector_alikePref_var, alikePref_valor)
  }
}
vector_alikePref_var=rep(vector_alikePref_var,each = 50)

vector_sm <- numeric(0)
for (i in 1:11) {
  for (j in 1:7) {
    sm_valor <- resultado_prog11[[i]][[j]]$similarMonitor_aux
    vector_sm <- c(vector_sm, sm_valor)
  }
}

base=as.data.frame(cbind(x = vector_mudanza_var,
                         y = vector_alikePref_var,
                         z = vector_sm,
                         t=rep(seq(1,50),77)))
names(base)
library(tidyverse)
base_50=base %>% filter(t==50)
a=base_50 %>% filter(x==6) 
plot(a$y,a$z)


# Cargar la biblioteca akima
# install.packages("akima")
library(akima)

# Interpolar los datos para crear una malla 3D
interp_data <- interp(base_50$x, 
                      base_50$y, 
                      base_50$z)

# Crear el gráfico de superficie 3D
library(plotly)
plot_ly(z = interp_data$z) %>%
  add_surface(x = interp_data$y,
              y = interp_data$x,
              colorscale = "Reds") %>%
  layout(
    scene = list(
      xaxis = list(title = "AlikePref", range = c(0, 1)),
      yaxis = list(title = "Mudanza", range = c(0, 12)),
      zaxis = list(title = "SM son proporción de similares", range = c(min(interp_data$z), max(interp_data$z)))
    )
  )




