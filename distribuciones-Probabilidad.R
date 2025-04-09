# Instalar paquetes necesarios si no están
if (!require(evd)) install.packages("evd")
if (!require(MASS)) install.packages("MASS")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(tidyr)) install.packages("tidyr")

# Cargar paquetes
library(evd)
library(MASS)
library(ggplot2)
library(tidyr)

# Leer el archivo CSV
data <- read.csv("C:/Documentos PC/UAQ - Licenciatura/Ciencia y Analítica de Datos/4° Semestre/Herramientas para Ciencia de Datos/ProyectoHerramientasCAD/DatasetDistancias_Modificado.csv") 

# Seleccionar columna (puedes cambiar a "Recorrido.2", etc.)
x <- data$Recorrido.6
x <- x[!is.na(x) & x < 100]  # Eliminar NA y valores extremos si es necesario

# Ajustar distribuciones
fit_norm <- fitdistr(x, "normal")
fit_exp <- fitdistr(x - min(x), "exponential")  # Exponencial necesita datos >= 0
lambda_pois <- mean(round(x))  # Para Poisson, datos enteros

# --- Ajuste de Gumbel manual (shape = 0) ---
loglik_gumbel <- function(params) {
  loc <- params[1]
  scale <- params[2]
  if (scale <= 0) return(Inf)
  -sum(dgumbel(x, loc = loc, scale = scale, log = TRUE))
}
fit_gumbel <- optim(par = c(mean(x), sd(x)), fn = loglik_gumbel, method = "L-BFGS-B", lower = c(-Inf, 0.001))

# Crear secuencia para graficar densidades
x_range <- seq(min(x), max(x), length.out = 1000)

# Crear data frame de densidades
dens_df <- data.frame(
  x = x_range,
  Normal = dnorm(x_range, mean = fit_norm$estimate["mean"], sd = fit_norm$estimate["sd"]),
  Exponencial = dexp(x_range - min(x), rate = fit_exp$estimate["rate"]),
  Gumbel = dgumbel(x_range, loc = fit_gumbel$par[1], scale = fit_gumbel$par[2]),
  Poisson = dpois(round(x_range), lambda = lambda_pois)
)

# Convertir a formato largo para ggplot
dens_long <- pivot_longer(dens_df, -x, names_to = "Distribución", values_to = "Densidad")

# Graficar
ggplot() +
  geom_histogram(aes(x = x, y = ..density..), bins = 30, fill = "grey80", color = "black", alpha = 0.6) +
  geom_line(data = dens_long, aes(x = x, y = Densidad, color = Distribución), size = 1) +
  labs(title = "Recorrido 2",
       x = "Valor", y = "Densidad") +
  theme_minimal()

