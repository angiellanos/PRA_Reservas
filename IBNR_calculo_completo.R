if (!require('ChainLadder')) install.packages('ChainLadder')
library(ChainLadder)
library(readxl)

if (!require('kableExtra')) install.packages('kableExtra')
library(knitr)
library(kableExtra)

# Configurar R para evitar la notación científica
options(scipen = 999)


# Definición de parámetros iniciales
lambda <- 3  # Tasa promedio de reclamaciones por año de ocurrencia
mu <- 10000  # Media de la distribución log-normal para la severidad
sigma <- 3000  # Desviación estándar de la distribución log-normal para la severidad

# Número de años de ocurrencia y desarrollo
num_years_occurrence <- 10
num_years_development <- 10
years <- 2010:2019  # Años de ocurrencia
months <- seq(12, 12 * num_years_development, by = 12)  # Meses de desarrollo

# Creando matrices para almacenar los datos de los triángulos
Reportados <- matrix(0, nrow = num_years_occurrence, ncol = num_years_development, dimnames = list(years, months))
Pagados <- matrix(0, nrow = num_years_occurrence, ncol = num_years_development, dimnames = list(years, months))

# Llenando el triángulo de reclamaciones con acumulados
for (i in 1:num_years_occurrence) {
  for (j in 1:(num_years_development - i + 1)) {
    repeat {
      num_claims <- rpois(1, lambda)
      if (num_claims > 0) break
    }
    claim_sizes <- sum(rlnorm(num_claims, meanlog = log(mu), sdlog = log(sigma)))
    Reportados[i, j] <- claim_sizes + if (j > 1) Reportados[i, j-1] else 0
  }
}

# Definición de parámetros de pago
pay_out_ratio_mean <- 0.8  # Media de la proporción de pago
pay_out_ratio_sd <- 0.1    # Desviación estándar de la proporción de pago

# Llenando el triángulo de pagos con acumulados asegurando que sean menores que los reportados
for (i in 1:num_years_occurrence) {
  for (j in 1:(num_years_development - i + 1)) {
    payout_ratio <- min(rnorm(1, mean = pay_out_ratio_mean, sd = pay_out_ratio_sd), 0.99)
    potential_payment <- payout_ratio * Reportados[i, j]
    Pagados[i, j] <- potential_payment + if (j > 1) Pagados[i, j-1] else 0
    # Asegurarse de que los pagados no superen a los reportados
    Pagados[i, j] <- min(Pagados[i, j], Reportados[i, j])
  }
}

# Imprimir los triángulos
kable(Reportados, caption = "Triángulo de Reclamaciones Reportadas (Acumulados)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
kable(Pagados, caption = "Triángulo de Pagos (Acumulados)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


Pagados[Pagados == 0] <- NA
Reportados[Reportados == 0] <- NA




##
dev_factors_5 = function(t) {
  nfil = nrow(t)
  periods = ncol(t)
  factors = numeric(periods - 1)
  for (i in 1:(periods - 1)){
    omvlue=nfil-i+1
    sumy1 <- sum(tail( na.omit(t[-omvlue, i]),5),na.rm = TRUE) 
    sumy2 <- sum(tail( na.omit(t[, i+1]),5),na.rm = TRUE)
    factors[i] <- sumy2 / sumy1  
  }
  
  return(factors)
}

dev_factors_3 = function(t) {
  nfil = nrow(t)
  periods = ncol(t)
  factors = numeric(periods - 1)
  for (i in 1:(periods - 1)){
    omvlue=nfil-i+1
    sumy1 <- sum(tail( na.omit(t[-omvlue, i]),3),na.rm = TRUE) 
    sumy2 <- sum(tail( na.omit(t[, i+1]),3),na.rm = TRUE)
    factors[i] <- sumy2 / sumy1  
  }
  
  return(factors)
}

##
age_factors_REP <- data.frame(dev_factors_av5 = dev_factors_5(Reportados))
age_factors_PAG <- data.frame(dev_factors_av5 = dev_factors_3(Pagados))

##
fdev_REP= age_factors_REP[,1]
fdev_PAG= age_factors_PAG[,1]

##
CDF_REP=NULL
CDF_PAG=NULL
CDF_REP=cumprod(rev(c(fdev_REP,1)))
CDF_PAG=cumprod(rev(c(fdev_PAG,1)))

##
Claims_REP <- numeric(nrow(Reportados))
for (i in 1:nrow(Reportados)) {
  Claims_REP[i] <- Reportados[i, ncol(Reportados) - i + 1]
}
Claims_PAG <- numeric(nrow(Pagados))
for (i in 1:nrow(Pagados)) {
  Claims_PAG[i] <- Pagados[i, ncol(Pagados) - i + 1]
}

##
ultimate_proj = data.frame(acc_year =row.names(Reportados),
                           age_acc_year= rev(colnames(Reportados)),
                           Claims_REP=Claims_REP,
                           Claims_PAG=Claims_PAG,
                           CDF_REP=round(CDF_REP,3),
                           CDF_PAG=round(CDF_PAG,3),
                           ULT_Claims_REP= round( CDF_REP*Claims_REP,0),
                           ULT_Claims_PAG= round( CDF_PAG*Claims_PAG,0)
)

dev_unpaid_claims = data.frame(acc_year =row.names(Reportados),
                               Claims_REP=Claims_REP,
                               Claims_PAG=Claims_PAG,
                               ULT_Claims_REP= round( CDF_REP*Claims_REP,0),
                               ULT_Claims_PAG= round( CDF_PAG*Claims_PAG,0),
                               Case_outst = Claims_REP - Claims_PAG,
                               IBNR_REP = round( CDF_REP*Claims_REP,0) - Claims_REP,
                               IBNR_PAG = round( CDF_PAG*Claims_PAG,0) - Claims_REP,
                               TOTAL_unpaid_REP = Claims_REP - Claims_PAG + round( CDF_REP*Claims_REP,0) - Claims_REP,
                               TOTAL_unpaid_PAG = Claims_REP - Claims_PAG + round( CDF_PAG*Claims_PAG,0) - Claims_REP
)

####################
print(ultimate_proj)
print(dev_unpaid_claims)
####################


# Al imprimir o crear tablas con kable, formatea los números para mejorar la legibilidad
# Reportados <- apply(Reportados, 1, function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))
# Pagados <- apply(Pagados, 1, function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))





