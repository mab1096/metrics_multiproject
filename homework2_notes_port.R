rm(list=ls())
data<- read.csv(file.choose(), header=FALSE)
View(data)
#C1

bwght <- data$V4
faminc <- data$V1
cigs <- data$V10

reg1 <- lm(formula = bwght~cigs + faminc)
summary(reg1)

reg2 <- lm(formula = bwght~cigs)
summary(reg2)
nrow(data)




#C2
rm(list=ls())
c2data<- read.csv(file.choose(), header=FALSE)
View(c2data)

price <- c2data$V1
sqrft <- c2data$V5
bdrms <- c2data$V3
c2reg1 <- lm(formula = price~sqrft + bdrms)

summary(c2reg1)
nrow(c2data)





#C3
rm(list=ls())
c3data<- read.csv(file.choose(), header=FALSE)
View(c3data)
atndrte <- c3data$V6
priGPA <- c3data$V3
ACT <- c3data$V4

summary(atndrte)
summary(priGPA)
summary(ACT)

c3reg1 <- lm(formula = atndrte ~ priGPA + ACT)
summary(c3reg1)
nrow(c3data)





#C4
rm(list=ls())
c4data<- read.csv(file.choose(), header=FALSE)
View(c4data)


#Condition: Exact amount
singlehh <- subset(c4data, c4data[,6]==1)
nrow(singlehh)
#Or
nrow(subset(c4data, c4data[,6]==1))


nettfa <- c4data$V7
inc <- c4data$V2
age <- c4data$V5
c4reg1 <- lm(formula = nettfa ~ inc + age)
summary(c4reg1)


#SINGLE
nettfa <- singlehh$V7
inc <- singlehh$V2
age <- singlehh$V5
regsingle <- lm(formula = nettfa ~ inc + age)
summary(regsingle)

plot(regsingle)





#5


rm(list=ls())
n <- 100
X <- runif(n)
u <- rnorm(n)
Y <- 2*X + u

mean(Y)
cor(X,Y)



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
