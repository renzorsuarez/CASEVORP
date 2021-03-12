# CASEVORP - language: R

# I)

library(BatchGetSymbols)

library(ggplot2)

library(quantmod)

library(tidyverse)

# Close Price of PETR4:

getSymbols("PETR4.SA", src = "yahoo", from = "2010-01-01", to = "2021-01-01")

View(PETR4.SA)

PETR4.SA <- as.data.frame(PETR4.SA)

PETR4_Cprice <- PETR4.SA %>% filter(!is.na(PETR4.SA.Close)) %>% select(PETR4.SA.Close)

View(PETR4_Cprice)

# Monthly Returns of PETR4:

PETR4_RET <- monthlyReturn(PETR4_Cprice)

# Close Price of BOVA11:

getSymbols("^BVSP", src = "yahoo", from = "2010-01-01", to = "2021-01-01")

View(BVSP)

BVSP <- as.data.frame(BVSP)

BVSP_Cprice <- BVSP %>% filter(!is.na(BVSP.Close)) %>% select(BVSP.Close)

View(BVSP_Cprice)

# Monthly Returns of BOVA11:

BVSP_RET <- monthlyReturn(BVSP_Cprice)

# Graphics:

plot(PETR4_RET,  main = "Gráfico Retorno PETR4", col = "black", xlab = "Data Ordenada")
lines(PETR4_RET, type = "o", col = "black")

plot(BVSP_RET,  main = "Gráfico Retorno BVSP", col = "green", xlab = "Data Ordenada")
lines(BVSP_RET, type = "o", col = "green")

# Basic Statistics:

# i) PETR4:

mean(PETR4_RET)
median(PETR4_RET)
max(PETR4_RET)
min(PETR4_RET)
var(PETR4_RET)
hist(PETR4_RET, freq = FALSE,main = "Histograma Retorno PETR4",
     xlab = "Retorno", col = "black")
     
# ii) BVSP:

mean(BVSP_RET)
median(BVSP_RET)
max(BVSP_RET)
min(BVSP_RET)
var(BVSP_RET)
hist(BVSP_RET, freq = FALSE, main = "Histograma Retorno BVSP",
     xlab = "Retorno", col = "green")

# Scatter plot and correlation analysis:

PETR4_RET <- as.numeric(PETR4_RET)

BVSP_RET <- as.numeric(BVSP_RET)

data_frame <- data.frame(PETR4_RET, BVSP_RET)

ggplot(data_frame , aes(x = BVSP_RET, y = PETR4_RET)) + geom_point(color = "black") + 
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = "red") + ggtitle("Retorno PETR4 x Retorno BVSP")
        
cor(BVSP_RET, PETR4_RET)

# Regression:

summary(lm(PETR4_RET~BVSP_RET))

# Calculating the market beta of the Petrobras stock (data: 10 last years / periodicity: monthly)

beta2 <- cov(PETR4_RET, BVSP_RET)/var(BVSP_RET)
beta2

# Analysis: Firstly, based on the basic statistics, we can say that the Petrobras stock has already generated greater returns, both positive and negative, than the Ibovespa index, with its maximum and minimum values being higher in module. The variance of the stock's returns are higher than the index variance, leading to the belief that, given greater volatility, the stock brings more risk to the shareholder. Another calculation that reinforces such a risk analysis is the asymmetry observed in the histogram of returns and in the distances between means and medians, such distance its bigger for PETR4 returns. When comparing the 2 assets returns, a important statistics is the correlation. With a positive correlation of 0.8, we can say that this 2 assets are positively correlated indicating a directly proportional relationship between the returns. To complete the risk and return analysis I calculated the beta of PETR4. With a market beta of 1.7793, we can say that in the last 10 years the Petrobras stock has a higher risk than the market index (so if the index increase 1%, the stock price will increase 1.7793%), but if well managed it can generate higher returns than the benchmark.
