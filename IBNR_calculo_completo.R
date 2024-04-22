if (!require('ChainLadder')) install.packages('ChainLadder')
library(ChainLadder)
library(readxl)

##
triangulo_reportados <- read_excel("C:/Users/joser/Downloads/prueba factor desarrollo 1.xlsx", sheet = "REPORTADOS")
triangulo_pagados <- read_excel("C:/Users/joser/Downloads/prueba factor desarrollo 1.xlsx", sheet = "PAGADOS")

##
triangulo_reportados <- as.data.frame(triangulo_reportados)
rownames(triangulo_reportados) <- triangulo_reportados[,1]
Reportados=triangulo_reportados[,-1]

triangulo_pagados <- as.data.frame(triangulo_pagados)
rownames(triangulo_pagados) <- triangulo_pagados[,1]
Pagados=triangulo_pagados[,-1]

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
                           age_acc_year= rev(names(Reportados)),
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




