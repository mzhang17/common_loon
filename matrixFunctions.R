rm(list = ls()) 

library(tidyverse)  

#this is Meirun's local path to reach the ACLC drive 
base_dir<-"C:/Users/meiru/Downloads/loon"  

# to import the productivity estimates supply a csv file name  
in_fname<-"Productivity_estimates.csv"  

# development code - stuff I run a lot when i'm developing code so I save it in this # setup code chunk so as to be able to get to it quickly. This code chunk gets transferred from file to file in my development environment #fname <- file.choose()}


constructMatrix2<-function(p)
{
  
p$Fa = with(p,(Pa^(10/12))*b*m*r)

# Construct a 4-stage model with 0,1,2 year olds and adults as the 4 stage classes
X<-with(p,matrix(c(0,sigma_j,0,0,
  0,0,sigma_j,0,
  0,0,0,sigma_j,
  Fa,0,0,Pa),4,4))

lam<-Re(eigen(X)$values[1])
#use the growth rate of the 4 stage matrix to collapse to a 2-stage (immature and adult) model
p$gamma_j = with(p,((sigma_j/lam)^(2))/sum(sapply(0:(2),function(x){(sigma_j/lam)^(x)})))

p$Gj = with(p,sigma_j*gamma_j)
p$Pj = with(p,sigma_j*(1-gamma_j))


two_stage_matrix <- with(p,matrix(c(Pj,Gj,Fa,Pa),byrow=FALSE,2,2))
lam2<-Re(eigen(two_stage_matrix)$values[1])
result <- list(two_stage_matrix = two_stage_matrix, lam2 = lam2)
return(result)
}

projectPopulation2<-function(p,N)
{
   # use matrix multiplication to project the population numbers to the next year
  result <- constructMatrix2(p)
  result$two_stage_matrix %*% N

}



constructMatrix4<-function(p)
{
  
p$Fa = with(p,(Pa^(10/12))*b*m*r)

# Construct a 4-stage model with 0,1,2 year olds and adults as the 4 stage classes
X<-with(p,matrix(c(0,sigma_j,0,0,
  0,0,sigma_j,0,
  0,0,0,sigma_j,
  Fa,0,0,Pa),4,4))

lam4<-Re(eigen(X)$values[1])
result <- list(X = X, lam4 = lam4)
  return(result)
}

projectPopulation4<-function(p,N)
{
   # use matrix multiplication to project the population numbers to the next year
  result <- constructMatrix4(p)
  result$X %*% N

}



constructMatrix3 <- function(p) {
  
  # Define fecundity rate for adults (same as before)
  p$Fa <- with(p, (sigma_a^(10/12))*b_a*m*r)
  p$Fy <- with(p, (sigma_a^(10/12))*b_y*m*r)
  # Construct a 5-stage model with:
  # 0-year-old, 1-year-old, 2-year-old, 3-year-old non-breeding adult, and 4-year-old+ adults.
  X <- with(p, matrix(c(
    0,0,0,Fy,Fy,Fy,Fa,
    sigma_j,0,0,0,0,0,0,
    0,sigma_j,0,0,0,0,0,
    0,0,sigma_j,0,0,0,0,
    0,0,0,sigma_y,0,0,0,
    0,0,0,0,sigma_y,0,0,
    0,0,0,0,0,sigma_y,sigma_a
  ), 7, 7, byrow = TRUE))
  
  lam <- Re(eigen(X)$values[1]) #save lambda in global environment for reference

  p$gamma_j <- with(p, ((sigma_j / lam)^(2)) / sum(sapply(0:(2), function(x) {(sigma_j / lam)^(x)})))
  p$Gj <- with(p, sigma_j * gamma_j)
  p$Pj <- with(p, sigma_j * (1 - gamma_j))
  p$gamma_y <- with(p, ((sigma_y / lam)^(2)) / sum(sapply(0:(2), function(x) {(sigma_y / lam)^(x)})))
  p$Py <- with(p, sigma_y * (1 - gamma_y))
  p$Gy <- with(p, sigma_y * gamma_y)


  # Construct the 3-stage matrix
  three_stage_matrix <- with(p, matrix(c(
    # Juvenile stage
    Pj, Fy, Fa,
    # Young adult stage
    Gj,Py,0,
    # Adult stage (breeding adults)
    0, Gy, sigma_a
  ), 3, 3, byrow = TRUE))
  lam3 <- Re(eigen(three_stage_matrix)$values[1])
  result <- list(three_stage_matrix = three_stage_matrix, lam3 = lam3)
  return(result)
}

projectPopulation3<-function(p,N)
  {
  # use matrix multiplication to project the population numbers to the next year
  result <- constructMatrix3(p)
  result$three_stage_matrix %*% N
  }


constructMatrix7 <- function(p) {
  
  # Define fecundity rate for adults (same as before)
  p$Fa <- with(p, (sigma_a^(10/12))*b_a*m*r)
  p$Fy <- with(p, (sigma_a^(10/12))*b_y*m*r)
 
  X <- with(p, matrix(c(
    0,0,0,Fy,Fy,Fy,Fa,
    sigma_j,0,0,0,0,0,0,
    0,sigma_j,0,0,0,0,0,
    0,0,sigma_j,0,0,0,0,
    0,0,0,sigma_y,0,0,0,
    0,0,0,0,sigma_y,0,0,
    0,0,0,0,0,sigma_y,sigma_a
  ), 7, 7, byrow = TRUE))
  
  lam7 <- Re(eigen(X)$values[1]) #save lambda in global environment for reference
  result <- list(X = X, lam7 = lam7)
  return(result)
}

projectPopulation7<-function(p,N)
  {
  # use matrix multiplication to project the population numbers to the next year
  result <- constructMatrix7(p)
  result$X %*% N
  }
