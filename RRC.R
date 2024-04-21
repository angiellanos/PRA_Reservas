###################################
### RESERVA DE RIESGOS EN CURSO ###
###################################


## 1. RESERVA DE PRIMA NO DEVENGADA ###

datos <- read_excel("C:/Users/joser/OneDrive/Escritorio/CALCULO_RPND/RIESGOS_VIGENTES_IN_ANONIMO.xlsx", sheet = "RIESGOS_VIGENTES")

colnames(datos)

datos$maximo <- apply(datos[, c("PRIMA_COMERCIAL", "PRIMA_COMERCIAL_SIN_DCTO")], 1, max)
datos$dias_vigencia = datos$FECHA_FIN_VIG - datos$FECHA_INICIO_VIG

fecha_corte = "2023-12-31"
fecha_corte <- as.POSIXct(fecha_corte)
datos$fecha_corte = fecha_corte

datos$maximo_fecha <- apply(datos[, c("fecha_corte", "FECHA_INICIO_VIG")], 1, max)

datos$resta_fechas <- numeric(length(datos$FECHA_FIN_VIG))
for (i in 1:length(datos$FECHA_FIN_VIG)){
  datos$resta_fechas[i] =  datos$FECHA_FIN_VIG[i] - as.POSIXct(datos$maximo_fecha)[i]
}

datos$resta_fechas_aprox <- apply(datos[, "resta_fechas", drop = FALSE], 2, ceiling)

datos$frac_riesgo_noc_ini = datos$resta_fechas_aprox/as.numeric(datos$dias_vigencia)

datos$frac_riesgo_noc_def <- ifelse(datos$dias_vigencia > 30, datos$frac_riesgo_noc_ini, 0.5)

datos$RvaNoDev = (datos$maximo - datos$GASTOS_EXPEDICION)*datos$frac_riesgo_noc_def

RPND = sum(datos$RvaNoDev)

## 2. RESERVA DE INSUFICIENCIA DE PRIMA ##


datos_rip <- read_excel("C:/Users/joser/OneDrive/Escritorio/CALCULO_RPND/RIESGOS_VIGENTES_IN_ANONIMO.xlsx", sheet = "RIP")

datos_rip$TOTAL_INGRESOS = datos_rip$PRIMA_DEVENGADA+datos_rip$INGRESOS_REASEGUROS+datos_rip$INGRESOS_FINANCIEROS

datos_rip$TOTAL_EGRESOS = datos_rip$EGRESOS_SINIESTROS+datos_rip$EGRESOS_FINANCIEROS+datos_rip$EGRESOS_OTROS

datos_rip$DIFERENCIA = datos_rip$TOTAL_EGRESOS - datos_rip$TOTAL_INGRESOS

datos_rip$DIFERENCIA_cumsum = cumsum(datos_rip$DIFERENCIA)
datos_rip$PRIMA_DEVENGADA_cumsum = cumsum(datos_rip$PRIMA_DEVENGADA)

datos_rip$FACTOR <- numeric(length(datos_rip$DIFERENCIA_cumsum))
for (i in 1:length(datos_rip$DIFERENCIA_cumsum)){
  datos_rip$FACTOR[i] = max(0, datos_rip$DIFERENCIA_cumsum[i]/datos_rip$PRIMA_DEVENGADA_cumsum[i])
}

RIP = datos_rip$FACTOR[24]*RPND

##########################

print(RPND)
print(RIP)

##########################