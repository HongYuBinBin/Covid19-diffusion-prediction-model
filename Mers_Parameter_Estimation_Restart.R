
library(deSolve)

# 1. Create a training dataset

parameter_data = read.csv("./System_Biology/mers_pj.csv")
#parameter_data = read.csv("./System_Biology/mers_oneday.csv")

vector <- c(1,1,0)
vector_2 <- c(1,1,0)


parameter_data <- parameter_data[1:20,]

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
    dH = (lambda * I) - (k2 * H) - (delta * H)
    dR = (k1 * A) + (k2 * H) + (delta * H)  
    
    return(list(c(dS, dE, dA, dI)))
  })
}


Stack_function <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -(beta_1 * (A * S)/N) - (beta_2 * (I * S)/N) - (beta_3 * (H * S)/N)
    dE <-  (beta_1 * (A * S)/N) + (beta_2 * (I * S)/N) + (beta_3 * (H * S)/N) - (1/4.3 * E)
    dA <- ((1-gamma)*1/4.3)*E-(1/5*A)
    dI <- (gamma * 1/4.3 * E) - (1/5 * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H)
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H)    
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}
# k2, lambda = 1/7
# sigma = 1/7.5

Stack_function_2 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -(beta_1 * A * S/N * l_1) - (beta_2 * H * S/N * l_2) - (beta_3 * I * S/N * l_3)
    dE <- (beta_1 * A * S/N * l_1) + (beta_2 * H * S/N * l_2) + (beta_3 * I * S/N) - (1/5.2 * E * l_3) - (1/5.2 * E) - (d_1 * E)
    dA <- (1-gamma)*1/5.2*(E-1/5*A)-(d_2 * A)
    dI <- (gamma * 1/5.2 * E) - (1/5 * I) - (d_3 * I)
    dH = (1/5 * I) - (1/7 * H) - (1/15.16 * H) - (d_4 * I) 
    dR = (1/5 * A) + (1/7 * H) + (1/15.16 * H) + (d_1 * E) + (d_2 * A) + (d_3 * I) + (d_4 * I)     
    
    return(list(c(dS, dE, dA, dI, dH, dR)))
  })
}
N = 49520000
# 원래는 바로 Data Frame을 이용하는게 맞으나 Stack 함수를 내부에서 이용
# 하기 때문에 초기값만 설정해 준다. (Data Frame에서 이용)
Mers_Model_function <- function(data, P) {
  
  N = 49520000
  init <- c(S = N-40, E = 30, A = 30, I = 1, H = 16, R = 0)
  parameters <- c(beta_1 = P[1], beta_2 = P[2], beta_3 = P[3], gamma = P[4])
  times <- seq(0, 19, by = 1)
  out <- as.data.frame(ode(y = init, times = times, func = Stack_function, parms = parameters))
  out$time <- NULL
  
  
  return(out)
}

P <- c(0.87, 0.78, 0.45, 0.03)

Mers_Model_function(data, P)

Mers_Model_function_2 <- function(data, P) {
  
  N = 49520000
  init <- c(S = N-40, E = 16, A = 16, I = 1, H = 0, R = 0)
  parameters <- c(beta_1 = P[1], beta_2 = P[2], beta_3 = P[3], gamma = P[4])
  times <- seq(0, 46, by = 1)
  out <- as.data.frame(ode(y = init, times = times, func = Stack_function, parms = parameters))
  out$time <- NULL
  
  
  return(out)
}

P <- c(0.87, 0.78, 0.45, 0.03)

Mers_Model_function_2(data, P)


Mers_Model_function_train_test <- function(data, P) {
  
  N = 49520000
  init <- c(S = N-40, E = 16, A = 16, I = 0, H = 0, R = 0)
  parameters <- c(beta_1 = P[1], beta_2 = P[2], beta_3 = P[3], gamma = P[4])
  times <- seq(0, 300, by = 1)
  out <- as.data.frame(ode(y = init, times = times, func = Stack_function, parms = parameters))
  out$time <- NULL
  
  
  return(out)
}

P <- c(0.87, 0.78, 0.45, 0.03)

Mers_Model_function_train_test(data, P)



# 3. Define the objective function (=cost or evaluation function)

nparam= 4  # 2개 더 늘릴 예정

Eval_function = function(data, P){ # P: parameter vector
  #vmax=P[1]; Ks=P[2]; ki=P[3]; # Random parameters will be assigned
  
  I <- as.vector(data$cum.case)
  #print(I)
  output <- Mers_Model_function(data = data, P=P)
  I_predicted <- as.vector(output$I)
  #print(I_predicted)
  # [Add your code here!] Calculate the difference between the data and the model result
  error <- I - I_predicted 
  SSE = sum(error^2)
  return(SSE)
}



Estimation_output <- Eval_function(data = parameter_data, P = c(0.5,0.5, 0.5, 0.5))

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



lower <- c(0,0,0,0)
upper <- c(1,1,1,1)


# 7. Run SA to estimate (optimize) the unkonwn parameters

library(GenSA)

nparam = 4
out_estimation_result <- GenSA(par = par, lower = lower, upper = upper, fn = Eval_function, data = parameter_data,
                           control=list(threshold.stop=min.global+tol,
                                        verbose=FALSE, maxit=max.iteration))

print("SA-driven parameter estimation result:") 
print(out_estimation_result[c("value","par","counts")]) 


out_estimation_result



library(ggplot2)

#X <- seq(1,6,1)

p1 <- ggplot(parameter_data, aes(x = X, y = cum.case)) + geom_point()

result <- Mers_Model_function(parameter_data,out_estimation_result$par)

parameter_data[,'I_predicted'] <- result$I

p2 <- p1 + geom_line(data = parameter_data, aes(x = X, y = I_predicted), color = 'red') 
p2+ theme_bw() + labs(x = "[X]", y = "I")+
  scale_y_continuous(breaks=seq(0, 40, 1)) + lims(y=c(0, 300)) + ggtitle("Spreading of Infectious Disease") 
+ annotate("text", x = 40, y = 200, label = "[I] = 0")  

#########################################################################################
############### Train test
#########################################################################################
result_2 <- Mers_Model_function_train_test(parameter_data, out_estimation_result$par)

train_test <- data.frame(X = seq(1,301,1), I_predicted_more = result_2$I)

ggplot() + geom_line(data = train_test, aes(x = X, y = I_predicted_more), color = 'red')




init <- c(S = 1-1e-6, I = 1e-6, 0.0)
parameters <- c(beta = 1.4247, gamma = 0.14286)
times <- seq(0, 70, by = 1)
out <- as.data.frame(ode(y = init, times = times, func = Estimation_objective_function, parms = parameters))
out$time <- NULL


matplot(times, out, type = "l", xlab = "Time", ylab = "Susceptibles and Recovereds", main = "SIR Model", lwd = 1, lty = 1, bty = "l", col = 2:4)
legend(40, 0.7, c("Susceptibles", "Infecteds", "Recovereds"), pch = 1, col = 2:4)



