prog_1 <- function(density, citySize,years,
                             alikePref,capital, mudanza,ahorro1,ahorro2){
  set.seed(1991)
  city_aux <- list()
  
  # Función para definir las coordenadas y regiones de c/agente.
  mod <- function(a, b){ 
    1 + ((a - 1) %% b)
  }
  div <- function(a, b){ 
    1 + ((a - 1) %/% b) 
  }
  
  # Definiendo la composición de la ciudad al inicio de la simulaci?n.
  full <- floor(citySize ^ 2 * density)
  city <- matrix(0, citySize, citySize)
  cap_city<-matrix(0, citySize, citySize)
  occupied <- sample(1:(citySize ^ 2), full)
  city[occupied] <- c(1,2)
  cap_city[occupied] <-capital  #se le asigna capital a cada agente
  
  # Vectores  
  side <- dim(city)[1]
  check <- -1:1
  unHappyMonitor <- c()
  similarMonitor <- c()
  
  for (t in 1:years) {
    t
    lastCity <- city
    unhappy <- rep(NA, length(city))
    similar <- rep(NA, length(city))
    city_aux[[t]]<-city
    
    # Definiendo estado del agente y composici?n de los barrios. 
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
    unHappyMonitor <- c(unHappyMonitor,
                        length(whoUnhappy)/(length(which(!unhappy)) + 
                                              length(whoUnhappy)))
    similarMonitor <- c(similarMonitor, mean(similar, na.rm = T))
    
    # Moviendo Agentes Insatisfechos  
    randUnhappy <- whoUnhappy[sample.int(length(whoUnhappy))]
    empty <- which(city == 0)
    
    for (i in randUnhappy) {
      if (cap_city[i]>mudanza){  #se evalúa si tiene dinero suficiente para mudarse
        dest <- sample.int(length(empty), 1) # Lugar vacio de destino
        city[empty[dest]] <- city[i] # asigno valor del agente al lugar de destino
        cap_city[empty[dest]] <- cap_city[i]-mudanza  #asigno el capital que tenía menos la mudanza
        city[i] <- 0 # vacio el lugar original
        cap_city[i]<-0 #sin dinero en el lugar original
        empty[dest] <- i # agrego el lugar que quedo vacio al vector de lugares vacios
      }
    }
    los1 <- which(city ==1) #tomo todos los lugares ocupados por clase 1
    los2  <- which(city ==2) #tomo todos los lugares ocupados por clase 2
    cap_city[los1]<-cap_city[los1]+ahorro1 #le sumo el ahorro del periodo
    cap_city[los2]<-cap_city[los2]+ahorro2 #le sumo el ahorro del periodo
    
    if (identical(lastCity, city)) { break }
  }
  
  return_list <- list(city = city,
                      # unHappyMonitor=unHappyMonitor,
                      # similarMonitor=similarMonitor,
                      city_aux=city_aux)
  return(return_list)
}

