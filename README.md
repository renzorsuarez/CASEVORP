# CASEVORP - language: R

#_________________________________
#VORP - Case:

#I)

library(BatchGetSymbols)

library(quantmod)

library(tidyverse)

# Close Price of PETR4:

getSymbols("PETR4.SA", src = "yahoo", from = "2010-01-01", to = "2021-01-01")

View(PETR4.SA)

PETR4.SA <- as.data.frame(PETR4.SA)

PETR4_Cprice <- PETR4.SA %>% filter(!is.na(PETR4.SA.Close)) %>% select(PETR4.SA.Close)

View(PETR4_Cprice)

PETR4_RET <- monthlyReturn(PETR4_Cprice)

# Close Price of BOVA11:

getSymbols("^BVSP", src = "yahoo", from = "2010-01-01", to = "2021-01-01")

View(BVSP)

BVSP <- as.data.frame(BVSP)

BVSP_Cprice <- BVSP %>% filter(!is.na(BVSP.Close)) %>% select(BVSP.Close)

View(BVSP_Cprice)

BVSP_RET <- monthlyReturn(BVSP_Cprice)

#Realizando os gráficos:

plot(PETR4_RET,  main = "Gráfico Retorno PETR4", col = "black", xlab = "Data Ordenada")
lines(PETR4_RET, type = "o", col = "black")

plot(BVSP_RET,  main = "Gráfico Retorno BVSP", col = "green", xlab = "Data Ordenada")
lines(BVSP_RET, type = "o", col = "green")

#Estatisticas basicas:

mean(PETR4_RET)
median(PETR4_RET)
max(PETR4_RET)
min(PETR4_RET)
hist(PETR4_RET, freq = FALSE,main = "Histograma Retorno PETR4",
     xlab = "Retorno", col = "black")

#-------------

mean(BVSP_RET)
median(BVSP_RET)
max(BVSP_RET)
min(BVSP_RET)
hist(BVSP_RET, freq = FALSE, main = "Histograma Retorno BVSP",
     xlab = "Retorno", col = "green")

#-------------

library(ggplot2)

ggplot(PETR4_RET , aes(x = BVSP_RET, y = PETR4_RET)) + geom_point(color = "black") + 
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = "red") + ggtitle("Retorno PETR4 x Retorno BVSP")

lm(PETR4_RET~BVSP_RET)

cor(BVSP_RET, PETR4_RET)

# Calculo do beta:

beta2 <- cov(PETR4_RET, BVSP_RET)/var(BVSP_RET)
beta2
