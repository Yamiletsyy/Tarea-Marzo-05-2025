theta <- seq(0, 2*pi, length=100)
plot(x,y,type="n",xlim=c(0,100),ylim=c(0,100),asp=1)

for(i in 1:50){
  x0 <- runif(1,0,100)
  y0 <- runif(1,0,100)
  r <- runif(1,5,15)
  col <- sample(c("red","blue","green","black"),1)
  
  lines(x0 + r*cos(theta), y0 + r*sin(theta), col=col)
}