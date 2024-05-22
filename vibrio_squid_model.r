library(tidyverse)
library(deSolve)



model1 <- function(t, y, parms){
  
  # Parameters
  c <- parms$c
  m <- parms$m
  
  p <- parms$p
  
  n <- parms$n
  KL <- parms$KL
  Kd <- parms$Kd
  
  LL <- parms$LL
  alpha <- parms$alpha
  KI <- parms$KI
  
  # State variables
  A <- y["A"]
  R <- y["R"]
  Lb <- y["Lb"]
  I <- y["I"]
  
  # Differential equations
  dA <- A * (c * R - A * m)
  dR <- p * R - c * A * R
  # dR <- 0
  dLb <- ( (A ^ n) / (KL + A ^ n) ) - Kd * Lb
  dI <- alpha * (LL - Lb + I) - KI * I
  
  list(c(dA = dA, dR = dR, dLb = dLb, dI = dI))
}


y_0 <- c(A = 0.01, R = 0.01, Lb = 0, I = 0)
parms <- list(c = 1, m = 1, p = 1, n = 2, KL = 1, Kd = 1, LL = 10, alpha = 1, KI = 2)


Res <- ode(y = y_0, times = 1:100, func = model1, parms = parms) %>%
  as_tibble()

Res %>%
  mutate(Lc = Lb - I) %>%
  pivot_longer(-time, names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = time, y = value)) +
  facet_wrap(~variable, scales = "free_y") + 
  geom_line() +
  theme_classic()




