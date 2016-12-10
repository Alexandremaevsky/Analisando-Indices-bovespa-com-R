install.packages("quantmod")
install.packages("xts")
install.packages("moments")

install.packages("corrplot")
install.packages("lubridate")
install.packages("readr")

library(readr)
library(quantmod)
library(xts)
library(moments)
library(plyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(reshape2)
library(lubridate)
library(stringr)

startDate = as.Date("2016-01-01")
endDate = as.Date("2016-12-08")



DolarCambio <- as.data.frame(getFX("USD/BRL", src="yahoo",from = startDate, to= endDate, env = NULL))

DolarCambio$Data = row.names(DolarCambio)
row.names(DolarCambio) = NULL

ibovespa = read_csv(file.choose())

names(ibovespa)= "Empresa"

ibovespa <- as.matrix(ibovespa)

f = function(x) {
  temp = as.data.frame(x)
  result = as.data.frame(temp[,6])
  names(result) = names(temp)[6]
  result$Data = rownames(temp)
  return(result)
}
##klnb11.sa sanb

cotacoesEmpresas <- list()

for (i in 1:length(ibovespa)){
  cotacoesEmpresas[[i]] <- f(getSymbols(ibovespa[i], src="yahoo", auto.assign=FALSE, return.class="xts",  from=startDate, to=endDate))
}


df_tmp = join_all(dfs = list(as.data.frame(DolarCambio), as.data.frame(cotacoesEmpresas)), by ="Data")

class(dfs)


  
df_final2 = na.omit(df_tmp)
df_final2 = select(df_final2, -Data)


df_final2$Data.10 <- NULL
df_final2$Data.11 <- NULL
df_final2$Data.12 <- NULL
df_final2$Data.13 <- NULL
df_final2$Data.14 <- NULL
df_final2$Data.15 <- NULL
df_final2$Data.16 <- NULL
df_final2$Data.17 <- NULL
df_final2$Data.18 <- NULL

df_final2$Data.19 <- NULL
df_final2$Data.20 <- NULL
df_final2$Data.21 <- NULL
df_final2$Data.22 <- NULL
df_final2$Data.23 <- NULL
df_final2$Data.24 <- NULL
df_final2$Data.25 <- NULL
df_final2$Data.26 <- NULL
df_final2$Data.27 <- NULL
df_final2$Data.28 <- NULL
df_final2$Data.29 <- NULL
df_final2$Data.30 <- NULL
df_final2$Data.31 <- NULL
df_final2$Data.32 <- NULL
df_final2$Data.33 <- NULL
df_final2$Data.34 <- NULL
df_final2$Data.35 <- NULL
df_final2$Data.36 <- NULL
df_final2$Data.37 <- NULL
df_final2$Data.38 <- NULL
df_final2$Data.39 <- NULL
df_final2$Data.40 <- NULL
df_final2$Data.41 <- NULL
df_final2$Data.42 <- NULL
df_final2$Data.43 <- NULL
df_final2$Data.44 <- NULL
df_final2$Data.45 <- NULL
df_final2$Data.46 <- NULL
df_final2$Data.47 <- NULL
df_final2$Data.48 <- NULL
df_final2$Data.49 <- NULL
df_final2$Data.50 <- NULL
df_final2$Data.51 <- NULL
df_final2$Data.52 <- NULL
df_final2$Data.53 <- NULL
df_final2$Data.54 <- NULL

correlacao = cor(df_final2)


m2 = (df_final2[, c(1, runif(9,min=2, max=65))])
m2 = cor(m2)
corrplot(
      m2, 
      method="color", 
      tl.cex = 1, 
      type="full", 
      addCoef.col = "white",
      tl.srt=45
      )
