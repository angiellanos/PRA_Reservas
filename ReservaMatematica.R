# Instalar paquetes si es necesario
if (!require('lubridate')) install.packages('lubridate')
library(lubridate)

# Número de pólizas
num_polizas <- 100

# Fecha actual
fecha_hoy <- Sys.Date()

# Generar 100 fechas de inicio aleatorias dentro de los últimos 10 años
set.seed(123) # Para reproducibilidad
fechas_inicio <- fecha_hoy - years(10) + days(sample(0:3650, num_polizas, replace = TRUE))

# Ahora podemos calcular la reserva matemática para cada una de estas pólizas
# usando un ciclo for o una función sapply/Vectorize si se prefiere vectorizar

# Este es un esqueleto básico de cómo podrías configurar el cálculo:
reservas <- sapply(fechas_inicio, function(fecha_inicio) {
  # Calcular la antigüedad de la póliza
  antiguedad <- as.numeric(difftime(fecha_hoy, fecha_inicio, units = "days")) / 365.25
  
  # Asumimos que ya tienes definidas las funciones y los valores de axn y a_x+k
  # Por lo tanto, solo debes usarlas aquí según la lógica del cálculo de la reserva
  # ...
  
  # Sustituir por el cálculo real de la reserva
  reserva_calculada <- ... 
  return(reserva_calculada)
})

# Ahora reservas es un vector con la reserva matemática para cada una de las pólizas