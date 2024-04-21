# Instalar paquetes si es necesario
if (!require('lubridate')) install.packages('lubridate')
library(lubridate)
library(readxl)

# Fecha actual
fecha_hoy <- Sys.Date()

# Número de pólizas
num_polizas <- 100
# Tasa de interés
i <- 0.05
v <- 1/(1+i)
d <- i/(1+i)
# Duración de todas las pólizas (Años)
n <- 10



# Generar 100 fechas de inicio aleatorias dentro de los últimos n años
set.seed(123) # Para reproducibilidad
fechas_inicio <- fecha_hoy - years(10) + days(sample(0:3650, num_polizas, replace = TRUE))
# Edades aleatorias de los asegurados
edades <- sample(20:80, num_polizas, replace = TRUE) # Edad en el tiempo de suscripción
# Sexos aleatorias de los asegurados
sexos <- sample(c(0,1), num_polizas, replace = TRUE) # 0: Hombres, 1: Mujeres
# Indemnizaciones aleatorias de los asegurados (Valor asegurado)
indemnizaciones <- sample(2:10, num_polizas, replace = TRUE) * 10000000



#Cargar tablas de mortalidad (asegurados colombianos Hombres y mujeres)
mort_Hombres <- read_excel(paste(getwd(),"/TABLAS_DE_MORTALIDAD_1998-2003.xlsx",sep = ""), sheet = "Hombres")
mort_Mujeres <- read_excel(paste(getwd(),"/TABLAS_DE_MORTALIDAD_1998-2003.xlsx",sep = ""), sheet = "Mujeres")

procesar_mortalidad <- function(data) {
  # Agregar columnas para calcular la prima 
  data$px <- 1 - data$qx
  data$adx <- NA                # columna para äx
  data$adx[nrow(data)] <- 1
  
  # Calcular los valores de columna äx recursivamente desde la penúltima fila hacia arriba
  for (i in (nrow(data) - 1):1) {
    data$adx[i] <- 1 + v * data$px[i] * data$adx[i + 1]
  }
  
  # Bucle para calcular múltiples äx: ángulo n y Ax: ángulo n
  for (m in 0:9) {
    adxn_col <- paste("adxn", n-m, sep = "_")
    Axn_col <- paste("Axn", n-m, sep = "_")
    data[[adxn_col]] <- NA  # Crea una nueva columna para äx: ángulo n-m
    data[[Axn_col]] <- NA   # Crea una nueva columna para Ax: ángulo n
    
    for (i in 1:nrow(data)) {
      if (i + m <= nrow(data)) {
        data[[adxn_col]][i] <- data$adx[i] - data$lx[i+n-m] / data$lx[i] * v^(n-m) * data$adx[i+n-m]
        data[[Axn_col]][i] <- 1 - d * data[[adxn_col]][i]
      }
    }
  }
  
  return(data)
}

mort_Hombres <- procesar_mortalidad(mort_Hombres)
mort_Mujeres <- procesar_mortalidad(mort_Mujeres)


prima_individual <- function(posicion, indemnizacion) {
  # Reconoce el sexo del asegurado
  if (sexos[posicion] == 0){ 
    df = mort_Hombres} else {
      df = mort_Mujeres
    }
  # Encuentra äx: ángulo n y Ax: ángulo n 
  row  <- which(df$x == edades[posicion])
  adxn <- df$adxn_10[row]
  Axn  <- df$Axn_10[row]
  
  # Calcula prima individual
  prima <- indemnizacion * Axn/adxn
  return(prima)
}


primas_polizas <- sapply(seq_along(indemnizaciones), function(x) prima_individual(x, indemnizaciones[x]))
print(primas_polizas)



# Nuevo DataFrame que guarde las reservas año a año para cada póliza
fechas_anios <- sort(unique(as.integer(format(fechas_inicio,"%Y"))))
fechas_anios <- seq(from = min(fechas_anios), to = max(fechas_anios) + 10)

col_poliza <- paste("R_poliza", 1:num_polizas, sep = "_")
columnas <- c("Fecha", col_poliza)
df <- setNames(data.frame(matrix(ncol = length(columnas), nrow = length(fechas_anios))), columnas)
df$Fecha <- fechas_anios

df_desagregado <- data.frame(df)

# Calcular la reserva matemática para cada una de estas pólizas en cada uno de sus años
for (pol in 1:num_polizas){
  if (sexos[pol] == 0){ 
    df_m = mort_Hombres} else {
      df_m = mort_Mujeres
    }
  row_fecha  <- which( df$Fecha == as.integer(format(fechas_inicio[pol],"%Y")))
  
  for (k in 0:(n-1)){
    row_edad   <- which( df_m$x   == edades[pol])
    df[[paste("R_poliza", pol, sep = "_")]][row_fecha+k] = indemnizaciones[pol] * df_m[[paste("Axn", n-k, sep = "_")]][row_edad+k] - primas_polizas[pol] * df_m[[paste("adxn", n-k, sep = "_")]][row_edad+k]
    if (k > 0){
      df_desagregado[[paste("R_poliza", pol, sep = "_")]][row_fecha+k] = df[[paste("R_poliza", pol, sep = "_")]][row_fecha+k] - df[[paste("R_poliza", pol, sep = "_")]][row_fecha+k-1] 
    }
    df_desagregado[[paste("R_poliza", pol, sep = "_")]][row_fecha] = 0 
  }
  df[[paste("R_poliza", pol, sep = "_")]][row_fecha+n] = indemnizaciones[pol] 
}

row_fecha_cal  <- which( df$Fecha == as.integer(format(fecha_hoy,"%Y")))

# RESERVA TOTAL AL AÑO DE CÁLCULO (2024)
R_TOTAL <- sum(df[row_fecha_cal, ], na.rm = TRUE)

# Reserva realizada en 2024 acorde a las primas
R_F_CAL <- sum(df_desagregado[row_fecha_cal, ], na.rm = TRUE)


