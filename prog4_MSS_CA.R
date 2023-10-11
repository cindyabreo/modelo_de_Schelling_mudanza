#funcion auxiliar
prog_4_aux <- function(years, density, alikePref, citySize, capital, ahorro, mudanza_var){

  set.seed(1991)
  
  mod <- function(a, b) { 1 + ((a - 1) %% b) }
  div <- function(a, b) { 1 + ((a - 1) %/% b) }
  
  # Definiendo la composicion de la ciudad al inicio de la simulacion.
  full <- floor(citySize ^ 2 * density)
  city <- matrix(0, citySize, citySize)
  cap_city<-matrix(0, citySize, citySize)
  occupied <- sample(1:(citySize ^ 2), full, replace = TRUE)
  city[occupied] <- c(1,2)
  cap_city[occupied] <- capital  #se le asigna capital a cada agente
  
  # Vectores  
  side <- dim(city)[1]
  check <- -1:1
  unHappyMonitor <- c()  # se crean vectores de insatisfaccion y de similitud
  unHappyMonitor1 <- c() # para toda la poblacion y para cada grupo
  unHappyMonitor2 <- c()
  similarMonitor <- c()
  similarMonitor1 <- c()
  similarMonitor2 <- c()
  
  for (t in 1:years) {
    
    lastCity <- city
    unhappy <- rep(NA, length(city))
    similar <- rep(NA, length(city))
    
    # Definiendo estado del agente y composicion de los barrios. 
    for (n in which(city > 0)) {
      x <- div(n, side)
      y <- mod(n, side)
      xRegion <- mod(check + x, side)
      yRegion <- mod(check + y, side)
      region <- city[yRegion, xRegion]
      sSimilar <- sum(region == city[n]) - 1
      total <- sum(region > 0) - 1
      similar[n] <- sSimilar / total
      unhappy[n] <- (sSimilar < total * alikePref)
    }
    
    # Monitores
    whoUnhappy <- which(unhappy)
    whoUnhappy1 <- which(unhappy & city==1)
    whoUnhappy2 <- which(unhappy & city==2)
    unHappyMonitor <- c(unHappyMonitor,
                        length(whoUnhappy)/(length(which(!unhappy)) + length(whoUnhappy)))
    unHappyMonitor1 <- c(unHappyMonitor1,
                         length(whoUnhappy1)/(length(which(!unhappy & city==1)) + length(whoUnhappy1)))
    unHappyMonitor2 <- c(unHappyMonitor2,
                         length(whoUnhappy2)/(length(which(!unhappy & city==2)) + length(whoUnhappy2)))
    
    similarMonitor <- c(similarMonitor, mean(similar, na.rm = T))
    similarMonitor1 <- c(similarMonitor1, mean(similar[city==1], na.rm = T))
    similarMonitor2 <- c(similarMonitor2, mean(similar[city==2], na.rm = T))
    
    # Moviendo Agentes Insatisfechos  
    randUnhappy <- whoUnhappy[sample.int(length(whoUnhappy))]
    empty <- which(city == 0)
    for (i in randUnhappy) {
      if (cap_city[i]>mudanza_var){  #se eval?a si tiene dinero suficiente para mudarse
        dest <- sample.int(length(empty), 1) # Lugar vacio de destino
        city[empty[dest]] <- city[i] # asigno valor del agente al lugar de destino
        cap_city[empty[dest]] <- cap_city[i]-mudanza_var  #asigno el capital que ten?a menos la mudanza
        city[i] <- 0 # vacio el lugar original
        cap_city[i]<-0 #sin dinero en el lugar original
        empty[dest] <- i # agrego el lugar que quedo vacio al vector de lugares vacios
      }
    }
    los1 <- which(city ==1) #tomo todos los lugares ocupados por clase 1
    los2  <- which(city ==2) #tomo todos los lugares ocupados por clase 2
    cap_city[los1]<-cap_city[los1]+ahorro[1] #le sumo el ahorro del periodo
    cap_city[los2]<-cap_city[los2]+ahorro[2] #le sumo el ahorro del periodo
    
  }
  
  devuelve<-c(similarMonitor[length(similarMonitor)],unHappyMonitor[length(unHappyMonitor)])
  
  # return(devuelve)
  return(list(unHappyMonitor_aux = unHappyMonitor, 
              similarMonitor_aux = similarMonitor,
              mudanza_var=mudanza_var,
              alikePref=alikePref))
}
