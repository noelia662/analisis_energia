# Paso 1: Configuración inicial

# Crear los vectores de energía, consumo diario y costo por kWh
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))

# Algunos valores de consumo son NA (valores faltantes)
consumo <- c(50, 55, NA, 65, 60, 45, 70, NA, 60, 50,
             85, 90, 95, NA, 100, 80, 75, 110, 105, NA)

# Costo por kWh para cada tipo de energía
costo_kwh <- c(rep(0.12, 10), rep(0.15, 10))  # 0.12 para renovable, 0.15 para no renovable

# Paso 2: Limpieza de datos

# Reemplazar valores NA en el vector de consumo por la mediana de consumo por tipo de energía
consumo_renovable <- consumo[energia == "Renovable"]
consumo_no_renovable <- consumo[energia == "No Renovable"]

# Reemplazar NAs en consumo renovable por la mediana de consumo renovable
consumo_renovable[is.na(consumo_renovable)] <- median(consumo_renovable, na.rm = TRUE)

# Reemplazar NAs en consumo no renovable por la mediana de consumo no renovable
consumo_no_renovable[is.na(consumo_no_renovable)] <- median(consumo_no_renovable, na.rm = TRUE)

# Unir nuevamente los vectores
consumo[energia == "Renovable"] <- consumo_renovable
consumo[energia == "No Renovable"] <- consumo_no_renovable

# Paso 3: Creación del dataframe

# Crear el dataframe df_consumo
df_consumo <- data.frame(energia, consumo, costo_kwh)

# Paso 4: Cálculos

# Agregar columna de costo_total (consumo * costo por kWh)
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Calcular el total de consumo y costo por tipo de energía
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)

# Calcular la media de consumo diario por tipo de energía
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# Agregar columna de ganancia (simulación de aumento de precio del 10%)
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen

# Crear la lista resumen_energia

# Ordenar el dataframe por costo_total en orden descendente
df_consumo_ordenado <- df_consumo[order(-df_consumo$costo_total), ]

# Extraer las tres filas con el mayor costo_total
top_3_costos <- head(df_consumo_ordenado, 3)

# Crear la lista con la información requerida
resumen_energia <- list(
  dataframe_ordenado = df_consumo_ordenado,  # Dataframe ordenado
  total_consumo = total_consumo,             # Total de consumo por tipo de energía
  total_costo = total_costo,                 # Total de costo por tipo de energía
  top_3_costos = top_3_costos               # Las tres filas con el mayor costo
)

# Mostrar la lista resumen_energia
print(resumen_energia)

