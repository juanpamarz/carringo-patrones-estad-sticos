# -ESTIMADORES DE PARÁMETROS-

dataset <- read.csv("DatasetDistancias_Mod.csv")

# RECORRIDO 1
sensor <- dataset$Recorrido1[dataset$Recorrido1 < 200]
# PARÁMETROS INICIALES
set.seed(40)
nsample <- length(sensor) 
x <- seq(1, nsample)

  # MLE
  
  # MEDIA
  mu <- cumsum(sensor) / x
  
  # DESVIACIÓN ESTANDAR
  sensor_diff <- sensor - mu
  sensor_diff2 <- cumsum(sensor_diff^2)
  s1 <- sqrt(sensor_diff2 / x)
  
  # GRAFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "MLE RECORRIDO 1")
  points(x, mu, col = "hotpink")
  points(x, mu + s1, col = "lightgreen")
  points(x, mu - s1, col = "lightgreen")


  # EM

  # HIPERPARÁMETRO
  p <- 0.7 # Peso
  
  # PARÁMETROS INICIALES
  mu <- numeric(nsample) 
  varianza_recursiva <- numeric(nsample) 
  
  # CONDICIONES
  mu[1] <- sensor[1]
  varianza_recursiva[1] <- 1
  
  # EM RECURSIVO
  for (k in 2:nsample) {
    mu[k] <- p * mu[k - 1] + (1 - p) * sensor[k]
    varianza_recursiva[k] <- p * varianza_recursiva[k - 1] + (1 - p) * (sensor[k] - mu[k])^2
  }
  
  # GRÁFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "EM RECORRIDO 1")
  lines(x, sensor, col = "hotpink") 
  lines(x, mu, col = "navyblue")    
  
  # *********************************************************************************
  
  # RECORRIDO 2
  sensor <- dataset$Recorrido2[dataset$Recorrido2 < 400]
  
  # PARÁMETROS INICIALES
  set.seed(40)
  nsample <- length(sensor) 
  x <- seq(1, nsample)
  
  # MLE
  
  # MEDIA
  mu <- cumsum(sensor) / x
  
  # DESVIACIÓN ESTANDAR
  sensor_diff <- sensor - mu
  sensor_diff2 <- cumsum(sensor_diff^2)
  s1 <- sqrt(sensor_diff2 / x)
  
  # GRAFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "MLE RECORRIDO 2")
  points(x, mu, col = "hotpink")
  points(x, mu + s1, col = "lightgreen")
  points(x, mu - s1, col = "lightgreen")
  
  
  # EM
  
  # HIPERPARÁMETRO
  p <- 0.7 # Peso
  
  # PARÁMETROS INICIALES
  mu <- numeric(nsample) 
  varianza_recursiva <- numeric(nsample) 
  
  # CONDICIONES
  mu[1] <- sensor[1]
  varianza_recursiva[1] <- 1
  
  # EM RECURSIVO
  for (k in 2:nsample) {
    mu[k] <- p * mu[k - 1] + (1 - p) * sensor[k]
    varianza_recursiva[k] <- p * varianza_recursiva[k - 1] + (1 - p) * (sensor[k] - mu[k])^2
  }
  
  # GRÁFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "EM RECORRIDO 2")
  lines(x, sensor, col = "hotpink") 
  lines(x, mu, col = "navyblue")    
  
  # *********************************************************************************
  
  # RECORRIDO 3
  sensor <- dataset$Recorrido3[dataset$Recorrido3 < 400]
  
  # PARÁMETROS INICIALES
  set.seed(40)
  nsample <- length(sensor) 
  x <- seq(1, nsample)
  
  # MLE
  
  # MEDIA
  mu <- cumsum(sensor) / x
  
  # DESVIACIÓN ESTANDAR
  sensor_diff <- sensor - mu
  sensor_diff2 <- cumsum(sensor_diff^2)
  s1 <- sqrt(sensor_diff2 / x)
  
  # GRAFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "MLE RECORRIDO 3")
  points(x, mu, col = "hotpink")
  points(x, mu + s1, col = "lightgreen")
  points(x, mu - s1, col = "lightgreen")
  
  
  # EM
  
  # HIPERPARÁMETRO
  p <- 0.7 # Peso
  
  # PARÁMETROS INICIALES
  mu <- numeric(nsample) 
  varianza_recursiva <- numeric(nsample) 
  
  # CONDICIONES
  mu[1] <- sensor[1]
  varianza_recursiva[1] <- 1
  
  # EM RECURSIVO
  for (k in 2:nsample) {
    mu[k] <- p * mu[k - 1] + (1 - p) * sensor[k]
    varianza_recursiva[k] <- p * varianza_recursiva[k - 1] + (1 - p) * (sensor[k] - mu[k])^2
  }
  
  # GRÁFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "EM RECORRIDO 3")
  lines(x, sensor, col = "hotpink") 
  lines(x, mu, col = "navyblue")    
  
  # *********************************************************************************
  
  # RECORRIDO 4
  sensor <- dataset$Recorrido4[dataset$Recorrido4 < 400]
  
  # PARÁMETROS INICIALES
  set.seed(40)
  nsample <- length(sensor) 
  x <- seq(1, nsample)
  
  # MLE
  
  # MEDIA
  mu <- cumsum(sensor) / x
  
  # DESVIACIÓN ESTANDAR
  sensor_diff <- sensor - mu
  sensor_diff2 <- cumsum(sensor_diff^2)
  s1 <- sqrt(sensor_diff2 / x)
  
  # GRAFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "MLE RECORRIDO 4")
  points(x, mu, col = "hotpink")
  points(x, mu + s1, col = "lightgreen")
  points(x, mu - s1, col = "lightgreen")
  
  
  # EM
  
  # HIPERPARÁMETRO
  p <- 0.7 # Peso
  
  # PARÁMETROS INICIALES
  mu <- numeric(nsample) 
  varianza_recursiva <- numeric(nsample) 
  
  # CONDICIONES
  mu[1] <- sensor[1]
  varianza_recursiva[1] <- 1
  
  # EM RECURSIVO
  for (k in 2:nsample) {
    mu[k] <- p * mu[k - 1] + (1 - p) * sensor[k]
    varianza_recursiva[k] <- p * varianza_recursiva[k - 1] + (1 - p) * (sensor[k] - mu[k])^2
  }
  
  # GRÁFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "EM RECORRIDO 4")
  lines(x, sensor, col = "hotpink") 
  lines(x, mu, col = "navyblue")    
  
  # *********************************************************************************
  
  # RECORRIDO 5
  sensor <- dataset$Recorrido5[dataset$Recorrido5 < 200]
  
  # PARÁMETROS INICIALES
  set.seed(40)
  nsample <- length(sensor) 
  x <- seq(1, nsample)
  
  # MLE
  
  # MEDIA
  mu <- cumsum(sensor) / x
  
  # DESVIACIÓN ESTANDAR
  sensor_diff <- sensor - mu
  sensor_diff2 <- cumsum(sensor_diff^2)
  s1 <- sqrt(sensor_diff2 / x)
  
  # GRAFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "MLE RECORRIDO 5")
  points(x, mu, col = "hotpink")
  points(x, mu + s1, col = "lightgreen")
  points(x, mu - s1, col = "lightgreen")
  
  
  # EM
  
  # HIPERPARÁMETRO
  p <- 0.7 # Peso
  
  # PARÁMETROS INICIALES
  mu <- numeric(nsample) 
  varianza_recursiva <- numeric(nsample) 
  
  # CONDICIONES
  mu[1] <- sensor[1]
  varianza_recursiva[1] <- 1
  
  # EM RECURSIVO
  for (k in 2:nsample) {
    mu[k] <- p * mu[k - 1] + (1 - p) * sensor[k]
    varianza_recursiva[k] <- p * varianza_recursiva[k - 1] + (1 - p) * (sensor[k] - mu[k])^2
  }
  
  # GRÁFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "EM RECORRIDO 5")
  lines(x, sensor, col = "hotpink") 
  lines(x, mu, col = "navyblue")    
  
  # *********************************************************************************
  
  # RECORRIDO 6
  sensor <- dataset$Recorrido6[dataset$Recorrido6 < 400]
  
  # PARÁMETROS INICIALES
  set.seed(40)
  nsample <- length(sensor) 
  x <- seq(1, nsample)
  
  # MLE
  
  # MEDIA
  mu <- cumsum(sensor) / x
  
  # DESVIACIÓN ESTANDAR
  sensor_diff <- sensor - mu
  sensor_diff2 <- cumsum(sensor_diff^2)
  s1 <- sqrt(sensor_diff2 / x)
  
  # GRAFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "MLE RECORRIDO 6")
  points(x, mu, col = "hotpink")
  points(x, mu + s1, col = "lightgreen")
  points(x, mu - s1, col = "lightgreen")
  
  
  # EM
  
  # HIPERPARÁMETRO
  p <- 0.7 # Peso
  
  # PARÁMETROS INICIALES
  mu <- numeric(nsample) 
  varianza_recursiva <- numeric(nsample) 
  
  # CONDICIONES
  mu[1] <- sensor[1]
  varianza_recursiva[1] <- 1
  
  # EM RECURSIVO
  for (k in 2:nsample) {
    mu[k] <- p * mu[k - 1] + (1 - p) * sensor[k]
    varianza_recursiva[k] <- p * varianza_recursiva[k - 1] + (1 - p) * (sensor[k] - mu[k])^2
  }
  
  # GRÁFICO
  windows()
  plot(x, sensor, pch = 19, col = "gray30", main = "EM RECORRIDO 6")
  lines(x, sensor, col = "hotpink") 
  lines(x, mu, col = "navyblue")    
  
  
  
  
  
  
  
  
