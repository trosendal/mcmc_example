##Generate simulated data

# generate data with given parameters.
# consumption amounts of sources:
M <- c(10,5,1)
# Type specific parameters: (modifying factors between 0-1, summing to one)
t <- c(0.05,0.15,0.2,0.3,0.3)
# Source specific parameters:
s <- c(10,20,50)
# prevalence of types in each source, conditionally on positivity:
type <- 5
p <- matrix(NA,3,type)
p[1,1:type] <- c(0.1,0.1,0.2,0.2,0.4)# a positive has to be one of the types 
p[2,1:type] <- c(0.2,0.2,0.4,0.1,0.1)# a positive has to be one of the types 
p[3,1:type] <- c(0.4,0.2,0.2,0.1,0.1)# a positive has to be one of the types 
# generate number of cases of each type:
lambda <- numeric()
y<-numeric()
set.seed(1)
for(i in 1:type){ 
lambda[i] <- t[i]*( M[1]*s[1]*p[1,i] + M[2]*s[2]*p[2,i] + M[3]*s[3]*p[3,i] )
y[i] <- rpois(1,lambda[i])  # cases of type i 
}
a<-list(T=type, y=y, p=p, M=M)

##Generate priors
require(mc2d)
#source specific parameters:
ps<-c(runif(length(s), min(0), max(100)))
#type specific parameters:
pT<- as.vector(rdirichlet(1, c(1,1,1,1,1)))
p<-list(s=ps, t=pT)

## Run the model in "model.txt" with JAGS You need to install JAGS on your machine
## prior to installing the 'rjags' package.  The results below should
## be similar to those in the BUGS example.

library(rjags)
jags <- jags.model("model.txt",
                   data = a,
                   inits = p,
                   n.chains = 1,
                   n.adapt = 1000)

mcmc.samples <- coda.samples(model = jags,
                             variable.names = c('lambda', 's', 't'),
                             n.iter = 10000,
                             thin = 10)

summary(mcmc.samples)
