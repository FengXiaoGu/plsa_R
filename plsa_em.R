

#P(w|z)

reestimate_beta_num <- function(K, m_tf, gamma){
  
  start.time <- Sys.time()
  tfDim <- dim(m_tf)
  M <- tfDim[2]
  V <- tfDim[1]
  
  beta_numerator <- matrix(0,K,V)
  for(k in (1:K)){
    beta_numerator[k,] <- rowSums(m_tf * gamma[,,k])
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #print("Execution of reestimate_beta_num")
  #print(time.taken)
  
  beta_numerator
}

reestimate_beta_denom <- function(beta_numerator){
  start.time <- Sys.time()
  
  beta_denom <- rowSums(beta_numerator)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #print("Execution of reestimate_beta_denom")
  #print(time.taken)
  
  beta_denom
}

#P(w_j|z_k)
reestimate_beta <- function(K, m_tf, gamma){
  
  start.time <- Sys.time()

  beta_numerator <- reestimate_beta_num(K, m_tf, gamma)
  beta_denom <- reestimate_beta_denom(beta_numerator)
  
  beta <- beta_numerator / beta_denom
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #print("Execution of reestimate_beta")
  #print(time.taken)

  beta
}

#P(d|z)

reestimate_theta_num <- function(K, m_tf, gamma){
  
  start.time <- Sys.time()
  
  tfDim <- dim(m_tf)
  M <- tfDim[2]
  V <- tfDim[1]
  
  theta_num <- matrix(0,M,K)
  for(k in (1:K)){
    theta_num[,k] = colSums(m_tf * gamma[,,k])
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #print("Execution of reestimate_theta_num2")
  #print(time.taken)
  
  theta_num
}

reestimate_theta_denom <- function(m_tf){
  
  start.time <- Sys.time()
  
  theta_denom <- colSums(m_tf)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #print("Execution of reestimate_theta_denom")
  #print(time.taken)
  
  theta_denom
}

#P(z_k|d_i)
reestimate_theta <- function(K, m_tf, gamma){
  
  start.time <- Sys.time()
  
  theta_num <- reestimate_theta_num(K,m_tf,gamma)
  theta_denom <- reestimate_theta_denom(m_tf)
  
  theta <- theta_num / theta_denom
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #print("Execution of reestimate_theta")
  #print(time.taken)
  theta
}

gamma_numerator <- function(theta, beta){
  
  start.time <- Sys.time()
  K <- dim(beta)[1]
  V <- dim(beta)[2]
  M <- dim(theta)[1]
  
  gamma_num <- array(0, dim=c(V, M, K))
  
  for(j in 1:M){
      gamma_num[,j,] <- t(theta[j,] * beta)
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  #print("Execution of gamma_numerator2")
  #print(time.taken)
  
  gamma_num
}

gamma_numerator2 <- compiler::cmpfun(gamma_numerator)

gamma_denom <- function(theta, beta){
  
  start.time <- Sys.time()
  
  # gamma_denom <- apply(gamma_num, MARGIN=c(1, 2), sum) is too slow
  
  gamma_denom <- t(theta %*% beta)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #print("Execution of gamma_denom")
  #print(time.taken)
  
  gamma_denom
}

reestimate_gamma <-  function(theta, beta){
  
  K <- dim(beta)[1]
  
  start.time <- Sys.time()
  gamma_num <- gamma_numerator(theta, beta)
  
  gamma_denom <- gamma_denom(theta, beta)
  
   divide.start.time <- Sys.time()
  gamma <- array(0, dim=c(V, M, K))
  
  for(k in 1:K){
    gamma[,,k] <- gamma_num[,,k] / gamma_denom
  }
  
   divide.end.time <- Sys.time()
   divide.time.taken <- divide.end.time - divide.start.time
   #print("Execution of gamma divide")
   #print(divide.time.taken)
  
  

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  # print("Execution of reestimate_gamma")
  # print(time.taken)
  gamma
}

genDist <- function(size){
  v <- runif(size)
  v/sum(v)
}

initBeta <- function(K, m_tf){

  tfDim <- dim(m_tf)
  V <- tfDim[1]
  
  t(replicate(K, genDist(V)))
}

initTheta <- function(K, m_tf){
  
  tfDim <- dim(m_tf)
  M <- tfDim[2]

  t(replicate(M, genDist(K)))
}

calculateLogLikelihood <- function(m_tf, beta, theta){
  sum(m_tf * t(log(theta %*% beta)))
}

printParameters <- function(parametersName, parameters){
  print(paste("##################################### ", parametersName," ##################################"))
  print(parameters[1:5,1:5])
}

plsaEM <- function(K, m_tf, iter = 10){
  
  start.time <- Sys.time()
  # initialize params
  beta <- initBeta(K, m_tf)
  #printParameters("BETA", beta)
  #check_beta(beta)
  
  theta <- initTheta(K, m_tf)
  #printParameters("THETA", theta)
  #check_theta(theta)
  
  logLikelihood <- calculateLogLikelihood(m_tf, beta, theta)
  #print(paste("initial Log likelihood is = ", logLikelihood))
  
  
  liks <- rep(0,iter + 1)
  liks[1] <- logLikelihood
  
  for(i in (1:iter)) { 
    
    iter.start.time <- Sys.time()
    
    # E step
    gamma <- reestimate_gamma(theta, beta)
    #check_gamma(gamma)
    #printParameters("GAMMA", gamma[,,1])
    
    # M step
    beta <- reestimate_beta(K, m_tf, gamma)
    #check_beta(beta)
    #printParameters("BETA", beta)
    
    theta <- reestimate_theta(K, m_tf, gamma)
    #check_theta(theta)
    #printParameters("THETA", theta)
    
    logLikelihood <- calculateLogLikelihood(m_tf, beta, theta)
    liks[i+1] <- logLikelihood
    
    iter.end.time <- Sys.time()
    iter.time.taken <- iter.end.time - iter.start.time
     print(paste("Execution of sinlge iteration of plsaEM, K=",K," iter=",i))
     print(iter.time.taken)
    
    #print(paste("Iteration ", i))
    print(paste("Log likelihood is = ", logLikelihood, "Difference is = ", liks[i+1] - liks[i]))
    # print(paste("Difference is = ", liks[i+1] - liks[i]))
    
  }
  
  # plot(1:(iter+1), liks)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("Execution of plsaEM")
  print(time.taken)
  # liks
  logLikelihood
}
