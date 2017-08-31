

A <- matrix(2,10,10)
B <- matrix(3,10,10)

C <- outer(A,B, FUN="*")

length(C)

gamma_denom <- gamma_denom(gamma_num)
gamma_denom2 <- apply(gamma_num, MARGIN=c(1, 2), sum)

identical(gamma_denom, gamma_denom2)

all.equal(gamma_denom, gamma_denom2)


compare <- function(x,y){
  
  tol = 1e-50
  
  n <- dim(gamma_denom)[1]
  m <- dim(gamma_denom)[2]
  
  for(i in 1:n){
    for(j in 1:m){
      if(abs(x[i,j]-y[i,j]) > tol){
        print(paste(x, " is different than ", y))
      }
    }
  }
}

compare(gamma_denom, gamma_denom2)


gamma1 <- array(0, dim=c(V, M, K))
for(i in 1:V){
  for(j in 1:M){
    gamma1[i,j,] <- gamma_num[i,j,] / gamma_denom[i,j]
  }
}
dim(gamma1)

gamma2 <- array(0, dim=c(V, M, K))

for(k in 1:K){
  gamma2[,,k] <- gamma_num[,,k] / gamma_denom
}
dim(gamma2)


gamma1 <- reestimate_gamma(theta,beta)
gamma2 <- reestimate_gamma2(theta,beta)
all.equal(gamma1, gamma2)

m_tf <- as.matrix(tf)

beta1 <- reestimate_beta(K, m_tf, gamma)
dim(beta1)
beta1[2,3] <- 1
check_beta(beta1)


beta_num <- reestimate_beta_num(K, m_tf, gamma)
beta_num2 <- reestimate_beta_num(K, m_tf, gamma)

A <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 3, ncol = 4)
A

b <- c(2,3,4)

A/b


f <- function(v){
  print(v)
}

genDist <- function(size){
  v <- runif(size)
  v/sum(v)
}


A <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12, 13, 14, 15, 16, 17,18), nrow = 6, ncol = 3)
A

B <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12, 13, 14, 15), nrow = 3, ncol = 5)
B

C <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12, 13, 14, 15), nrow = 3, ncol = 5)
C

crossprod(B, C)


C
outer(A,B)

dim(A %o% B)



theta <- initTheta(K=10, m_tf)
beta <- initBeta(K=10, m_tf)

# test gamma numerator

gamma1 <- gamma_numerator(theta,beta)
gamma2 <- gamma_numerator2(theta,beta)

all.equal(gamma1, gamma2)

theta[1,] * beta[,1]

gamma1[1,1,] # == theta[1,] * beta[,1]
gamma2[1,1,] # != theta[1,] * beta[,1]

gamma_num <- array(0, dim=c(V, M, K))
gamma_num[,1,] = t(theta[1,] * beta)
gamma_num[1,1,]

# test gamma denominator

gamma_num1 <- gamma_numerator(theta,beta)
gamma_denom1 <- gamma_denom(gamma_num1)

gamma_denom2 <- gamma_denom2(theta,beta)

dim(gamma_denom1)
dim(gamma_denom2)

all.equal(gamma_denom1, gamma_denom2)


dim(tb)
tb[,1]


dim(gamma_num[,1,])
dim(tb)


gamma_num[1,1,]

dim(gamma_num[,1,])

dim(gamma_num[1,,])

a1 <- crossprod(m_tf[i,], gamma[i,,k])
a1

a2 <- m_tf[i,] %*% gamma[i,,k]
a2


beta_numerator[k,] <- tcrossprod(m_tf, gamma[,,3])

x <- c(1,2,3)

A
B
C
S

S = numeric(3)
for(i in 1:3){
  S[i]= B[i,] %*% C[i,]
}

for(i in 1:3){
  S[i]= crossprod(B[i,], C[i,])
}
S

beta <- initBeta(10, m_tf)
theta <- initTheta(10, m_tf)

gamma <- reestimate_gamma(theta, beta)

gamma[1:10,1:10,1]

beta_num1 <- reestimate_beta_num(K, m_tf, gamma)
beta_num2 <- reestimate_beta_num2(K, m_tf, gamma)
beta_num3 <- reestimate_beta_num3(K, m_tf, gamma)

all.equal(beta_num1, beta_num2)
all.equal(beta_num1, beta_num3)

beta_denom1 <- reestimate_beta_denom(beta_num1)
beta_denom2 <- reestimate_beta_denom2(beta_num1)

all.equal(beta_denom1, beta_denom2)

theta_num1 <- reestimate_theta_num(K, m_tf, gamma)
theta_num2 <- reestimate_theta_num2(K, m_tf, gamma)

all.equal(theta_num1, theta_num2)


theta_denom1 <- reestimate_theta_denom(m_tf)
theta_denom2 <- reestimate_theta_denom2(m_tf)

all.equal(theta_denom1, theta_denom2)


















