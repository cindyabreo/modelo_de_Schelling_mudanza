prog_9 <- function(years, density, alikePref, citySize, capital, ahorro, mudanza_var){
  
  set.seed(1991)
  
  # dir <- ("C:/Users/cindy/OneDrive/Escritorio/iesta/shiny_MSS_CA_v2/programas_MSS_CA")
  dir <- here::here("programas_MSS_CA")
  
  source(paste(dir,"prog7_MSS_CA.R",sep="/"))  
  
out1<-NULL
for (j in 1:length(mudanza_var)){
  out <-matrix(NA,length(alikePref), 2)
  for(i in 1:length(alikePref)){
    out[i,] <- prog_7_aux(years = years,density=density,
                                alikePref = alikePref[i], 
                                citySize = citySize, 
                                capital = capital, 
                                ahorro = ahorro, 
                                mudanza_var = mudanza_var[j])
  }
  
  out=as.data.frame(out)
  names(out)=c("sm","uhm")
  out$costomudanza=mudanza_var[j]
  out$alikePref=alikePref
  out1<-rbind(out1,out)
}
return(out1)
}
