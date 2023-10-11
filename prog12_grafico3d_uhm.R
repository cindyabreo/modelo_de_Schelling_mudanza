mudanza_var=seq(0,12,2)
alikePref_var=seq(0,1,0.1)
density=0.7
years=50
citySize=51
capital=c(4,10)
ahorro=c(1,2)
j=5
i=5

resultado_prog11=list()

for (j in 1:length(alikePref_var)) {
  temp_list <- list()  # Crear una lista temporal para almacenar los resultados de prog_11_aux
  for (i in 1:length(mudanza_var)) {
    temp_list[[i]] <- prog_4_aux(years, density, alikePref=alikePref_var[j], 
                                 citySize, capital, ahorro, mudanza_var[i])
  }
  resultado_prog11[[j]] <- temp_list  # Asignar la lista temporal como un elemento de resultado_prog11
}


vector_mudanza_var <- numeric(0)
for (i in 1:11) {
  for (j in 1:7) {
    mudanza_valor <- resultado_prog11[[i]][[j]]$mudanza_var
    vector_mudanza_var <- c(vector_mudanza_var, mudanza_valor)
  }
}
# vector_mudanza_var=rep(vector_mudanza_var,each = 50)
vector_mudanza_var=c(rep(0,50),rep(2,50),rep(4,50),rep(6,50),rep(8,50),rep(10,50),rep(12,50))

vector_alikePref_var <- numeric(0)
for (i in 1:11) {
  for (j in 1:7) {
    alikePref_valor <- resultado_prog11[[i]][[j]]$alikePref
    vector_alikePref_var <- c(vector_alikePref_var, alikePref_valor)
  }
}
vector_alikePref_var=rep(vector_alikePref_var,each = 50)

vector_uhm <- numeric(0)
for (i in 1:11) {
  for (j in 1:7) {
    uhm_valor <- resultado_prog11[[i]][[j]]$unHappyMonitor_aux
    vector_uhm <- c(vector_uhm, uhm_valor)
  }
}

base=as.data.frame(cbind(x = vector_mudanza_var,
                         y = vector_alikePref_var,
                         z = vector_uhm,
                         t=rep(seq(1,50),77)))
names(base)
base_50=base %>% filter(t==50)

# Cargar la biblioteca akima
# install.packages("akima")
library(akima)

# Interpolar los datos para crear una malla 3D
interp_data <- interp(base_50$x, 
                      base_50$y, 
                      base_50$z)

# Crear el gráfico de superficie 3D
plot_ly(z = interp_data$z) %>%
  add_surface(x = interp_data$x,
              y = interp_data$y,
              colorscale = "Blues") %>%
  layout(
    scene = list(
      xaxis = list(title = "Mudanza", range = c(0, 12)),
      yaxis = list(title = "AlikePref", range = c(0, 1)),
      zaxis = list(title = "uHM son proporción de infelices", range = c(min(interp_data$z), max(interp_data$z)))
    )
  )






