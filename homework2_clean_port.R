rm(list=ls())
data<- read.csv(file.choose(), header=FALSE)

#C1
nrow(data)
bwght <- data$V4
faminc <- data$V1
cigs <- data$V10
reg1 <- lm(formula = bwght~cigs + faminc)
summary(reg1)
reg2 <- lm(formula = bwght~cigs)
summary(reg2)



#C2
rm(list=ls())
c2data<- read.csv(file.choose(), header=FALSE)
price <- c2data$V1
sqrft <- c2data$V5
bdrms <- c2data$V3
c2reg1 <- lm(formula = price~sqrft + bdrms)
summary(c2reg1)
nrow(c2data)



#C4
rm(list=ls())
c4data<- read.csv(file.choose(), header=FALSE)
atndrte <- c4data$V6
priGPA <- c4data$V3
ACT <- c4data$V4
summary(atndrte)
summary(priGPA)
summary(ACT)
c4reg1 <- lm(formula = atndrte ~ priGPA + ACT)
summary(c4reg1)
nrow(c4data)

#C8
rm(list=ls())
c8data<- read.csv(file.choose(), header=FALSE)
nrow(subset(c8data, c8data[,6]==1))
nettfa <- c8data$V7
inc <- c8data$V2
age <- c8data$V5
c8reg1 <- lm(formula = nettfa ~ inc + age)
summary(c8reg1)

singlehh <- subset(c8data, c8data[,6]==1)
nettfa <- singlehh$V7
inc <- singlehh$V2
age <- singlehh$V5
regsingle <- lm(formula = nettfa ~ inc + age)
summary(regsingle)

#5
rm(list=ls())
n <- 1000
X <- runif(n, -1,1)
v <- rnorm(n)
P <- runif(n, -1,1)
u <- rnorm(n)
Z <- 0.2*X + v
Y <- 0.5*X + 3*Z + P + u
reg1 <- lm(formula = Y ~ X)
summary(reg1)
plot(reg1)
reg2 <- lm(formula = Y ~ X+Z)
summary(reg2)
reg3 <- lm(formula = Y ~ X+Z+P)
summary(reg3)
