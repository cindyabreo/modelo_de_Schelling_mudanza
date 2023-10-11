dir <- here::here("programas_MSS_CA")
print(list.files(dir))
dir()
# dir <- ("C:/Users/cindy/OneDrive/Escritorio/iesta/shiny_MSS_CA_v2/programas_MSS_CA")
# dir()
source(here::here(dir,"prog1_MSS_CA.R"))
source(here::here(dir,"prog2_MSS_CA.R"))
source(here::here(dir,"prog3_MSS_CA.R"))
source(here::here(dir,"prog4_MSS_CA.R"))
source(here::here(dir,"prog5_MSS_CA.R"))
source(here::here(dir,"prog6_MSS_CA.R"))
source(here::here(dir,"prog7_MSS_CA.R"))
source(here::here(dir,"prog8_MSS_CA.R"))
source(here::here(dir,"prog9_MSS_CA.R"))
source(here::here(dir,"prog10_MSS_CA.R"))

density=0.6
citySize=51
years=50
alikePref=0.6
capital1=4
capital2=10 
capital=c(4,10) 
# mudanza_var=
mudanza=2
ahorro1=1
ahorro2=2
capital <- c(capital1, capital2)

#prog 1----
resultado_prog1=prog_1(density, citySize, years,
       alikePref, capital, mudanza,
         ahorro1, ahorro2)

saveRDS(resultado_prog1, file = "resultado_prog1.rds")

#prog 2----
#plot2
resultado=resultado_prog1
resultado_plot_dinamico=prog_2_grafico(resultado_prog1)
resultado_plot_dinamico[[1]]
resultado_plot_dinamico[[5]]

resultado_plot=prog_3_grafico(resultado_prog1)

#plot2----
ahorro=c(ahorro1,ahorro2)
mudanza_var=0
resultado_m=prog_5(years, density, alikePref, citySize, capital, ahorro, mudanza_var)
  
resultado_m_plotly=prog_6(resultado_m)


#plot3----
capital1_hoja2=4
capital2_hoja2=10
ahorro1_hoja2=1
ahorro2_hoja2=2
mudanzaVariable=c(2,6,12)
citySize_hoja2=51
density_hoja2=0.6
years_hoja2=50  
alikePref_hoja2=0.6

capital_hoja2 <- c(capital1_hoja2, capital2_hoja2)
ahorro_hoja2=c(ahorro1_hoja2,ahorro2_hoja2)
  
mudanza_var <- as.numeric(mudanzaVariable)
  
resultado_prog5=prog_5(years_hoja2, density_hoja2, alikePref_hoja2, 
      citySize_hoja2, 
      capital_hoja2, ahorro_hoja2, mudanza_var)
saveRDS(resultado_prog5, file = "resultado_prog5.rds")

prog_6(resultado_prog5)


#programa 6 y 7

capital_hoja2 <- c(capital1_hoja2, capital2_hoja2)
ahorro <- c(ahorro1_hoja2, ahorro2_hoja2)
mudanza_var <- as.numeric(mudanzaVariable)
aPrefRange <- seq(0, 1, .1)
citySize_hoja2=51
density_hoja2=0.6
years_hoja2=50
out1=prog_9(years_hoja2,density_hoja2, alikePref=aPrefRange,
            citySize_hoja2, capital_hoja2, ahorro,mudanza_var)
saveRDS(out1, file = "out1.rds")


densityRange <- seq(0.45, 0.95, .05)
out2=prog_8(years_hoja2,density=densityRange,alikePref_hoja2,
            citySize_hoja2,capital_hoja2,ahorro,mudanza_var)
saveRDS(out2, file = "out2.rds")


prog_10(out1,out2)
