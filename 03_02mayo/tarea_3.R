
library(tidyverse)
library(ggfortify)
library(forecast)



# Función para grafica acf y pacf ----
acf_pacf_fun <- function(vec_ts, title_chr = "" ){
  require(ggfortify)
  require(gridExtra)
  
  gg.1 <- autoplot(acf(vec_ts, plot = F, lag = 30), main = "", ylab = "ACF")
  gg.2 <- autoplot(pacf(vec_ts, plot = F, lag = 30), main = "", ylab = "Partial ACF")  
  gridExtra::grid.arrange(gg.1, gg.2, ncol = 2, top = title_chr)
}


# 1a. 
n_sims <- 50

sigma2 <- 4
theta1 <- 0.7
Theta1 <- 0.6

set.seed(10871002)
z_vec <- rnorm(n_sims, mean = 0, sd = sqrt(sigma2))
y_vec <- NULL
x_vec <- NULL
x_vec[1:5] <- z_vec[1:5]

# Loop para generar el proceso 
for(i in 6:n_sims){
  x_vec[i] <- x_vec[i-1] + x_vec[i-4] - x_vec[i-5] + 
    z_vec[i] - theta1*z_vec[i-1] + Theta1*z_vec[i-4] - theta1*Theta1*z_vec[i-5]
}
x_vec

tibble(n_sim = 1:n_sims, 
       x_vec = x_vec) %>% 
  ggplot( aes(x = n_sim, y = x_vec) ) + 
  geom_line(size = .3) + 
  ylab(expression(paste("Y"["t"]^{}) ) )  + 
  xlab("tiempo") + 
  ggtitle( expression( paste("Proceso SARIMA(0,1,1)(0,1,1)"[4]^{}) ))



# 1b. 
ts_vec <- ts(x_vec, start = 1, frequency = 4)
autoplot(decompose(ts_vec))
acf_pacf_fun(ts_vec, title_chr = "Proceso SARIMA(0,1,1)(0,1,1)")

# 2b. 
vec_lag <- diff(x_vec, lag = 1) # y_vec - lag(y_vec) #
ts_vec_lag <- ts(vec_lag, start = 2, frequency = 4)
acf_pacf_fun(ts_vec_lag, title_chr = "Proceso Diferenciado 1 paso")

# 2c. 
vec_lag <- diff(x_vec, lag = 4) # y_vec - lag(y_vec) #
ts_vec_lag4 <- ts(vec_lag, start = 4, frequency = 4)
acf_pacf_fun(ts_vec_lag4, title_chr = "Proceso Diferenciado 4 pasos")

