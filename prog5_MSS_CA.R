prog_5=function(years, density, alikePref, citySize, capital, ahorro, mudanza_var){

  set.seed(1991)
  
  resultado_prog5=list()

  for(i in 1:length(mudanza_var)){
    resultado_prog5[[i]]=prog_4_aux(years, density, alikePref, citySize, capital, ahorro, mudanza_var[i])
  }
  return(resultado_prog5)
}
