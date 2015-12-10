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

##Write the data to a bugs dataset
require(R2WinBUGS); require(BRugs)

data <- tempfile(fileext = ".txt")
inits <- tempfile(fileext = ".txt")
bugs.data(a, digits = 5, dir = "", data.file = data)
bugs.data(p, digits = 5, dir = "", data.file = inits)

## Run the model in model.txt

monitor<-c('lambda', 's', 't')

output<-bugs(data,
             inits, parameters.to.save=monitor, 
             model.file='model.txt',
             n.chains=1, n.iter=10000, n.burnin=1000,
             n.thin=10,
             debug=FALSE, DIC = TRUE, digits=5, codaPkg=FALSE,
             ## bugs.directory="c:/Program Files/OpenBUGS/OpenBUGS322/",
             program="OpenBUGS",
             working.directory=NULL, clearWD=FALSE,
             ## useWINE=.Platform$OS.type != "windows", WINE=NULL,
             ## newWINE=TRUE, WINEPATH=NULL, bugs.seed=NULL, summary.only=FALSE,
             save.history=!summary.only, over.relax = FALSE)

output
