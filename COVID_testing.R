library(deSolve)

# 1. Create a training dataset

#parameter_data = read.csv("mers_pj.csv")

##parameter_data = read.csv("mers_oneday.csv")
##parameter_data = parameter_data[21:47,]

#parameter_data = parameter_data[21:47,]

##parameter_data_2 = read.csv("mers_oneday.csv")


parameter_data_covid_ = read.csv("./covid19_case.csv")

parameter_data_covid = parameter_data_covid_[1:18,]

parameter_data_covid = parameter_data_covid_[19:61,]


vector <- c(1,1,0,0)
vector_2 <- c(1,1,0,0)




# data.frame
# 데이터 나누기 필요?


# 2. Define a mathematical model to fit the data
sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

Stack_function_original <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * I * S/N - beta * A * S/N - beta * H * S/N
    dE <- -beta * I * S/N + beta * A * S/N + beta * H * S/N - sigma * E
    dA <- (1-gamma)*sigma*E-k1*A
    dI <- gamma * sigma * E - lambda_ * I
    dH = (labmda_ * I) - (k2 * H) - (delta * H)
    dR = (k1 * A) + (k2 * H) + (delta * H)    
    
    return(list(c(dS, dE, dA, dI)))
  })
}

# Before control
Stack_function <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    #N = 49520000
    dS <- -(beta_1 * (A * S)/N) - (beta_2 * (I * S)/N) - (beta_3 * (H * S)/N)
    dE <-  (beta_1 * (A * S)/N) + (beta_2 * (I * S)/N) + (beta_3 * (H * S)/N) - (1/4.3 * E)
    dA <- (1-gamma)*1/4.3*E-(1/5*A)
    dI <- (gamma * 1/4.3 * E) - (1/5 * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H)
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H)    
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}



# Before control_covid
Stack_function <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    #N = 49520000
    dS <- -(beta_1 * (A * S)/N) - (beta_2 * (I * S)/N) - (beta_3 * (H * S)/N)
    dE <-  (beta_1 * (A * S)/N) + (beta_2 * (I * S)/N) + (beta_3 * (H * S)/N) - (1/3 * E)
    dA <- (1-gamma)*1/3*E-(1/5*A)
    dI <- (gamma * 1/3 * E) - (1/5 * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H)
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H)    
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}


# After control
Stack_function_2 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -(beta_1 * A * S/N * l_1) - (beta_2 * I * S/N * l_2) - (beta_3 * H * S/N * l_3)
    dE <- (beta_1 * A * S/N * l_1) + (beta_2 * I * S/N * l_2) + (beta_3 * H * S/N*l_3) - (1/4.3 * E) - (d_1 * E)
    dA <- (1-gamma)*1/4.3*(E-1/5*A)-(d_2 * A)
    dI <- (gamma * 1/4.3 * E) - (1/5 * I) - (d_3 * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H) - (d_4 * I) 
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H) + (d_1 * E) + (d_2 * A) + (d_3 * I) + (d_4 * I)     
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}

Stack_function_2 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -(beta_1 * A * S/N * l_1) - (beta_2 * I * S/N * l_2) - (beta_3 * H * S/N * l_3)
    dE <- (beta_1 * A * S/N * l_1) + (beta_2 * I * S/N * l_2) + (beta_3 * H * S/N*l_3) - (1/4.3 * E) - (d_1 * E)
    dA <- (1-gamma)*1/4.3*(E-1/5*A)-(d_2 * A)
    dI <- (gamma * 1/4.3 * E) - (1/5 * I) - (d_3 * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H) - (d_4 * I) 
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H) + (d_1 * E) + (d_2 * A) + (d_3 * I) + (d_4 * I)     
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}


Stack_function_3 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -(beta_1 * A * S/N * l_1) - (beta_2 * I * S/N * l_2) - (beta_3 * H * S/N * l_3)
    dE <- (beta_1 * A * S/N * l_1) + (beta_2 * I * S/N * l_2) + (beta_3 * H * S/N*l_3) - (1/4.3 * E) - (d * E)
    dA <- (1-gamma)*1/4.3*(E-1/5*A)-(d * A)
    dI <- (gamma * 1/4.3 * E) - (1/5 * I) - (d * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H) - (d * I) 
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H) + (d * E) + (d * A) + (d * I) + (d * I)     
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}

Stack_function_3 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -(0.8079 * A * S/N * l_1) - (0.8667 * I * S/N * l_2) - (0.0193 * H * S/N * l_3)
    dE <- (0.8079 * A * S/N * l_1) + (0.8667 * I * S/N * l_2) + (0.0193 * H * S/N*l_3) - (1/4.3 * E) - (d * E)
    dA <- ((1-0.0531)*1/4.3*E) - (1/5*A)-(d * A)
    dI <- (0.0531 * 1/4.3 * E) - (1/5 * I) - (d * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H) - (d * I) 
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H) + (d * E) + (d * A) + (d * I) + (d * I)     
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}


## After control covid
#0.87, 0.88, 0.7, 0.9
Stack_function_3 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -(0.87 * A * S/N * l_1) - (0.88 * I * S/N * l_2) - (0.7 * H * S/N * l_3)
    dE <- (0.87 * A * S/N * l_1) + (0.88 * I * S/N * l_2) + (0.7 * H * S/N*l_3) - (1/3 * E) - (d * E)
    dA <- (1-0.9)*1/3*(E-1/5*A)-(d * A)
    dI <- (0.9 * 1/3 * E) - (1/5 * I) - (d * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H) - (d * I) 
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H) + (d * E) + (d * A) + (d * I) + (d * I)     
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}


N = 49520000
# 원래는 바로 Data Frame을 이용하는게 맞으나 Stack 함수를 내부에서 이용
# 하기 때문에 초기값만 설정해 준다. (Data Frame에서 이용)'

# Before control
Mers_Model_function <- function(data, P) {
  
  
  init <- c(S = N-40, E = 30, A = 30, I = 1, H = 16, R = 0)
  parameters <- c(beta_1 = P[1], beta_2 = P[2], beta_3 = P[3], gamma = P[4])
  times <- seq(0, 19, by = 1)
  out <- as.data.frame(ode(y = init, times = times, func = Stack_function, parms = parameters))
  out$time <- NULL
  
  return(out)
}

#P <- c(0.8756, 0.7833, 0.4568, 0.0348)

#P <- c(0.8756, 0.7833, 0.4568, 0.0382)

P <- c(0.8079, 0.8667, 0.0193, 0.0531)

Mers_Model_function(parameter_data, P)


# After control
Mers_Model_function_2 <- function(data, P) {
  

  init <- c(S = N-40, E = 16, A = 16, I = 1, H = 0, R = 0)
  parameters <- c(beta_1 = P[1], beta_2 = P[2], beta_3 = P[3], gamma = P[4])
  times <- seq(0, 19, by = 1)
  out <- as.data.frame(ode(y = init, times = times, func = Stack_function, parms = parameters))
  out$time <- NULL
  
  
  return(out)
}

P <- c(0.87, 0.78, 0.45, 0.03)

Mers_Model_function_2(data, P)

#############################
########## MERs Model
#############################

N = 49520000
#49514322 2770.27977 1457.95048 81.743285 38.514239 1366.896708
Mers_Model_function_3 <- function(data, P) {
  
  N = 49520000
  ##init <- c(S = N-4000, E = 2353, A = 1027, I = 43, H = 20, R = 1047)
  # <- c(S = 49514322, E = 2770, A = 1457, I = 43, H = 38, R = 1366)
  init <- c(S = N-40, E = 16, A = 16, I = 1, H = 0, R = 0)
  #parameters <- c(l_1 = P[1], l_2 = P[2], l_3 = P[3], d = P[4])
  times <- seq(0, 34, by = 1)
  out <- as.data.frame(ode(y = init, times = times, func = Stack_function, parms = parameters))
  out$time <- NULL
  
  
  return(out)
}

##P <- c(0.97, 0.38, 0.45, 0.25)
# P <- c(0.97, 0.88, 0.45, 0.25) Model 2 keep
# P <- c(0.99, 0.88, 0.45, 0.197) Model 2 Keep
# P <- c(0.87, 0.88, 0.45, 0.170) Model 2 Keep
# P <- c(0.77, 0.88, 0.45, 0.15) Model 2 Keep
P <- c(0.77, 0.88, 0.45, 0.15)
Mers_Model_function_3(data, P)

library(ggplot2)
p1 <- ggplot(parameter_data, aes(x = X, y = cum.I)) + geom_point()

result <- Mers_Model_function_3(parameter_data,P)

parameter_data[,'I_predicted'] <- result$I + 45   #38

p2 <- p1 + geom_line(data = parameter_data, aes(x = X, y = I_predicted), color = 'red') 
p2+ theme_bw() + labs(x = "[X]", y = "I")+
  scale_y_continuous(breaks=seq(0, 26, 1)) + lims(y=c(0, 300)) + ggtitle("Spreading of Infectious Disease") 




####################################################################
########### COVID Modeling
####################################################################


#N = 49520000
#49514322 2770.27977 1457.95048 81.743285 38.514239 1366.896708  Mers Model_1 State
#51825384 8709.66176 533.87865 4800.43582 1735.160478 1363.027395 COVID Model_1 State
N = 51842524

Mers_Model_function_covid_1 <- function(data, P) {
  
  N = 51825384
  ##init <- c(S = N-4000, E = 2353, A = 1027, I = 43, H = 20, R = 1047)
  init <- c(S = 51825384, E = 8709, A = 533, I = 4800, H = 1735, R = 1363)
  #init <- c(S = N-40, E = 16, A = 16, I = 10, H = 0, R = 0)
  #parameters <- c(beta_1 = P[1], beta_2 = P[2], beta_3 = P[3], gamma = P[4])
  parameters <- c(l_1 = P[1], l_2 = P[2], l_3 = P[3], d = P[4])
  times <- seq(0, 42, by = 1)
  out <- as.data.frame(ode(y = init, times = times, func = Stack_function_3, parms = parameters))
  out$time <- NULL
  
  
  return(out)
}

##P <- c(0.97, 0.38, 0.45, 0.25)
# P <- c(0.97, 0.88, 0.45, 0.25) Model 2 keep
# P <- c(0.99, 0.88, 0.45, 0.197) Model 2 Keep
# P <- c(0.87, 0.88, 0.45, 0.170) Model 2 Keep
# P <- c(0.77, 0.88, 0.45, 0.15) Model 2 Keep
P <- c(0.77, 0.88, 0.45, 0.3)



P <- c(0.87, 0.88, 0.7, 0.9)  # Model_1 (2/16~3/21)
P <- c(0.8756, 0.7833, 0.4568, 0.0382) # 논문의 Model_1

P <- c(0.77, 0.88, 0.45, 0.232)

Mers_Model_function_covid_1(data, P)

#Mers_Model_function_3(data, P)

library(ggplot2)
p1 <- ggplot(parameter_data_covid, aes(x = X, y = I.cum)) + geom_point()

result <- Mers_Model_function_covid_1(parameter_data,P)

#result <- Mers_Model_function_3(parameter_data,P)

parameter_data_covid[,'I_predicted'] <- result$I   #38

p2 <- p1 + geom_line(data = parameter_data_covid, aes(x = X, y = I_predicted), color = 'red') 
p2+ theme_bw() + labs(x = "Time[one day]", y = "The accumulated incidence")+
  scale_y_continuous(breaks=seq(0, 43, 1)) + lims(y=c(0, 20000)) + ggtitle("Spreading of COVID-19 After 3") 







N = 51842524

Mers_Model_function_train_test <- function(data, P) {
  
  N = 518425240
  #init <- c(S = N-4000, E = 2353, A = 1027, I = 43, H = 20, R = 1047)
  #init <- c(S = 49514322, E = 2770, A = 1457, I = 43, H = 38, R = 1366)
  #init <- c(S = N-40, E = 16, A = 16, I = 10, H = 0, R = 0)
  init <- c(S = 51825384, E = 8709, A = 533, I = 4800, H = 1735, R = 1363)
  parameters <- c(l_1 = P[1], l_2 = P[2], l_3 = P[3], d = P[4])
  #parameters <- c(beta_1 = P[1], beta_2 = P[2], beta_3 = P[3], gamma = P[4])
  times <- seq(0, 800, by = 1)
  out <- as.data.frame(ode(y = init, times = times, func = Stack_function_3, parms = parameters))
  out$time <- NULL
  
  
  return(out)
}

#P <- c(0.32, 0.91, 0.53, 0.05)
P <- c(0.97, 0.88, 0.45, 0.25) # Model_2
Mers_Model_function_train_test(data, P)



# 3. Define the objective function ( = cost or evaluation function)

nparam= 4  # 2개 더 늘릴 예정

Eval_function = function(data, P){ # P: parameter vector
  #vmax=P[1]; Ks=P[2]; ki=P[3]; # Random parameters will be assigned
  
  #print(data$cum.I)
  I <- as.vector(data$I.cum)
  #print(I)
  output <- Mers_Model_function_covid_1(data = data, P=P)
  #print(output$I)
  I_predicted <- as.vector(output$I)
  #print(I_predicted)
  # [Add your code here!] Calculate the difference between the data and the model result
  error <- I - I_predicted 
  SSE = sum(error^2)
  return(SSE)
}

length(parameter_data$cum.I)

output <- Mers_Model_function_3(parameter_data,c(0.5,0.5, 0.5, 0.5))

length(output$I)

Estimation_output <- Eval_function(data = parameter_data_covid, P = c(0.5,0.5, 0.5, 0.5))

Estimation_output


# 4. Generate a random number vector

new.seed = as.integer(runif(1)*2e9) # Make a new integer seed
#new.seed = 1234  #If you fix this value (instead of runif), the results are reproducible
cat("Random seed: ", new.seed, "\n")
set.seed(new.seed) # This function generates 624 random number vector.
# This random vector will be used in GenSA function. Don't worry if you don't understand.
# It can be saved and restored but not by altered by user
# tmp = .Random.seed # save the random number vector 
# tmp = .Random.seed; runif(10) # generate 10 random deviates using the vector
par = runif(4)


# Set SA option

min.global = 0      # Expected global minimum
tol = 1e-13         # Tolerance
max.iteration = 150 # Maximum number of iteration



# 6. Set the search range of each parameter
#par <- c(0.5,0.5,0.5,0.5)
lower <- c(0,0,0,0) 
upper <- c(1,1,1,1)
library(GenSA)

nparam = 4
out_estimation_result <- GenSA(par = par, lower = lower, upper = upper, fn = Eval_function, data = parameter_data_covid,
                               control=list(threshold.stop=min.global+tol,
                                            verbose=FALSE, maxit=max.iteration))

print("SA-driven parameter estimation result:") 
print(out_estimation_result[c("value","par","counts")]) 



library(ggplot2)

#X <- seq(1,6,1)

p1 <- ggplot(parameter_data_covid, aes(x = X, y = I.cum)) + geom_point()

result <- Mers_Model_function_covid_1(parameter_data,out_estimation_result$par)

parameter_data_covid[,'I_predicted'] <- result$I

p2 <- p1 + geom_line(data = parameter_data_covid, aes(x = X, y = I_predicted), color = 'red') 
p2+ theme_bw() + labs(x = "[X]", y = "I")+
  scale_y_continuous(breaks=seq(0, 40, 1)) + lims(y=c(0, 20000)) + ggtitle("Spreading of COVID-19") 



+ annotate("text", x = 40, y = 200, label = "[I] = 0")  



#P <- c(0.32, 0.91, 0.53, 0.05)
#P <- c(0.97, 0.88, 0.45, 0.30)
# P <- c(0.97, 0.38, 0.45, 0.29) # 매우 이상함?

#P <- c(0.87, 0.88, 0.45, 0.170) # Model 2 Keep 

P <- c(0.67, 0.88, 0.45, 0.15)
P <- c(0.37734053, 0.90000000, 0.30000000, 0.04962591)
P <- c(0.37734053, 0, 0, 0.04962591)

P <- c(0.87, 0.88, 0.7, 0.9)  # Model_1 (2/16~3/21)


P <- c(0.8615463, 0.7363360, 0.4037696, 0.9667556)    # Model 2 keep
P <- c(0.7844305, 1.0000000, 1.0000000, 0.2419557)    # Model 2 Keep

result_2 <- Mers_Model_function_train_test(parameter_data, P)

train_test <- data.frame(X = seq(62,862,1), I_predicted_more = result_2$I, S_predicted_more = result_2$S, E_predicted_more = result_2$E, A_predicted_more = result_2$A, H_predicted_more = result_2$H, R_predicted_more = result_2$R)
#train_test <- data.frame(X = seq(62,862,1), S_predicted_more = result_2$S)
#train_test <- data.frame(X = seq(62,862,1), E_predicted_more = result_2$E)
#train_test <- data.frame(X = seq(62,862,1), A_predicted_more = result_2$A)
#train_test <- data.frame(X = seq(62,862,1), H_predicted_more = result_2$H)
#train_test <- data.frame(X = seq(62,862,1), R_predicted_more = result_2$R)

ggplot() + geom_line(data = train_test, aes(x = X, y = I_predicted_more), color = 'I_predicted_more', lwd = 1.5) +   
  geom_line(data = train_test, aes(x = X, y = S_predicted_more), color = 'blue', lwd = 1.5) + 
  geom_line(data = train_test, aes(x = X, y = E_predicted_more), color = 'yellow', lwd = 1.5) +
  geom_line(data = train_test, aes(x = X, y = A_predicted_more), color = 'gray', lwd = 1.5) +

  scale_y_continuous(breaks=seq(0, 40, 1)) + lims(y=c(0, 25000)) + 
  labs(x = "Time[one day]", y = "The accumulated incidence") + ggtitle("Prediction of spread COVID-19 after(4/15)") +
  geom_vline(xintercept=117, linetype = 'dotted', color='gray', size = 1.5) + geom_hline(yintercept=11814, linetype='dotted', color='gray', size=1.5) +
  annotate("text", x = 90, y = 12500, label = "[11814]", size = 5) +
  scale_color_manual(values = c(
    'I_predicted_more' = 'red')) +
  labs(color = 'Y series')


ggplot(train_test) + geom_line(mapping = aes(x = X, y = I_predicted_more, color = 'Infected(감염자)'),lwd = 1.5) +   
  #geom_line(mapping=aes(x = X, y = S_predicted_more, color = 'Suspectible(위험인구)'), lwd = 1.5) + 
  geom_line(mapping=aes(x = X, y = E_predicted_more, color = 'Exposed(잠복기)'), lwd = 1.5) +
  geom_line(mapping=aes(x = X, y = A_predicted_more, color = 'Asymptomatic(무증상 감염)'), lwd = 1.5) +
  
  scale_y_continuous(breaks=seq(0, 40, 1)) + lims(y=c(0, 25000)) + 
  labs(x = "Time[one day]", y = "The accumulated incidence") + ggtitle("Prediction of spread COVID-19 after(4/15)") +
  geom_vline(xintercept=117, linetype = 'dotted', color='gray', size = 1.5) + geom_hline(yintercept=11814, linetype='dotted', color='gray', size=1.5) +
  annotate("text", x = 90, y = 12500, label = "[11814]", size = 5) +
  scale_color_manual(values = c(
    'Infected(감염자)' = 'red',
    #'Suspectible(위험인구)' = 'blue',
    'Exposed(잠복기)' = 'yellow',
    'Asymptomatic(무증상 감염)' = 'gray'
    )) +
  labs(color = 'prediction of COVID-19', size = 100)


ggplot(train_test) + geom_line(mapping=aes(x = X, y = S_predicted_more, color = 'Suspectible(위험인구)'), lwd = 1.5) + 
  geom_line(mapping=aes(x = X, y = H_predicted_more, color = 'Hospital(입원자)'), lwd = 1.5) +
  geom_line(mapping=aes(x = X, y = R_predicted_more, color = 'Removed(회복/사망)'), lwd = 1.5) +
  scale_y_continuous(breaks=seq(0, 40, 1000000)) + lims(y=c(0, 55000000)) + 
  labs(x = "Time[one day]", y = "The accumulated incidence") + ggtitle("Prediction of spread COVID-19 after(4/15)") +
  geom_vline(xintercept=117, linetype = 'dotted', color='gray', size = 1.5) + geom_hline(yintercept=11814, linetype='dotted', color='gray', size=1.5) +
  annotate("text", x = 90, y = 12500, label = "[11814]", size = 5) +
  scale_color_manual(values = c(
    'Suspectible(위험인구)' = 'blue',
    'Hospital(입원자)' = 'gray',
    'Removed(회복/사망)' = 'green'
  )) + labs(color = 'prediction of COVID-19', size = 100)







scale_fill_discrete(guide = guide_legend( 
                                         keywidth = 2, 
                                         keyheight = 2))

geom_line(data = train_test, aes(x = X, y = H_predicted_more), color = 'orange', lwd = 1.5) +
geom_line(data = train_test, aes(x = X, y = R_predicted_more), color = 'green', lwd = 1.5)

result_2

11814


