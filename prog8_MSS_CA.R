prog_8 <- function(years, density, alikePref, citySize, capital, ahorro, mudanza){
  set.seed(1991)
  
  #funcion auxiliar
  # source(here::here("programas_MSS","prog_7_aux.R"))
  # dir <- ("C:/Users/cindy/OneDrive/Escritorio/iesta/shiny_MSS_CA_v2/programas_MSS_CA")
  dir <- here::here("programas_MSS_CA")
  
  source(paste(dir,"prog7_MSS_CA.R",sep="/"))
  
out2<-NULL
for (j in 1:length(mudanza)){
  out <-matrix(NA,length(density), 2)
  for(i in 1:length(density)){
    out[i,] <- prog_7_aux(years = years, 
                                alikePref = alikePref, 
                                citySize = citySize, 
                                density = density[i], 
                                capital = capital, 
                                ahorro = ahorro, 
                                mudanza = mudanza[j])
  }
  out=as.data.frame(out)
  names(out)=c("sm","uhm")
  out$costomudanza=mudanza[j]
  out$density=density
  out2<-rbind(out2,out)
}
return(out2)
}
