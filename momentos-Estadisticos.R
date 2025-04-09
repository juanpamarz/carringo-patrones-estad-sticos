dataframe <- read.csv("C:\\Users\\jpma2\\Documents\\UNIVERSIDAD\\4TO_SEMESTRE\\HERRAMIENTASPARALACIENCIADEDATOS\\PRACTICAENR\\DatasetDistancias_Modificado.csv")

# Librería para asimetría y curtosis
library(moments)

# Función de métricas con filtro de valores menores a 200
calcular_metricas <- function(col) {
  col_filtrada <- col[col < 200]  # Filtramos valores menores a 200
  c(Media = mean(col_filtrada, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(col_filtrada), decreasing = TRUE)[1])),
    Varianza = var(col_filtrada, na.rm = TRUE),
    Desviación_Estandar = sd(col_filtrada, na.rm = TRUE),
    Coef_Variacion = (sd(col_filtrada, na.rm = TRUE) / mean(col_filtrada, na.rm = TRUE)) * 100,
    Asimetria = skewness(col_filtrada, na.rm = TRUE),
    Curtosis = kurtosis(col_filtrada, na.rm = TRUE))
}

estadisticas <- as.data.frame(t(sapply(dataframe, calcular_metricas)))
estadisticas$No_Cuasi_Gausiana <- ifelse(abs(estadisticas$Asimetria) > 1 | estadisticas$Curtosis < 2 | estadisticas$Curtosis > 4, "Sí", "No")
# Mostrar resultados
print("Medidas Estadísticas:"); print(estadisticas)
print("Variables cuasi-gaussianas:"); print(rownames(estadisticas[estadisticas$No_Cuasi_Gausiana == "No", ]))
