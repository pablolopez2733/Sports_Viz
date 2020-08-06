#attempt to simulate premier league with poisson

#ARSENAL VS CHELSEA (FA CUP)
#veamos si jala
lam1 <- 3.5*(21/17)
lam2 <- 2.5*(17/21)

df <- data.frame(NumGoals=c(0,1,2,3,4,5))

p.city <- rep(NA, 6)
p.madrid <- rep(NA, 6)
for (i in 1:6){
 
  p.city[i] <- dpois( df$NumGoals[i],lam1)
  p.madrid[i] <- dpois( df$NumGoals[i],lam2)
}

df$ProbCity <- p.city
df$ProbMadrid <- p.madrid




