
set.seed(123)

aa <- 1
hh <- 3
kk <- 2

yy <- seq(100)/20

xx <- aa*(yy - kk)^2 + hh


png()

plot(xx,yy, xlim=c(0,max(xx)), ylim=c(0,5), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
mtext(side=1, text=expression("Risk "(sigma)), line=1.5, cex=1.5)
mtext(side=2, text="Expected return", line=1.5, cex=1.5)




nn <- 1000

ii <- data.frame(x=rep(NA,nn), y=rep(NA,nn))

ss <- seq(0,5,length=nn)

for(i in 1:nn) {
	ii[i,"y"] <- runif(1, 0, 5)
	ii[i,"y"] <- max(rnorm(1, 3, 5),0)
	ii[i,"y"] <- ss[i]
	ii[i,"x"] <- runif(1, aa*(ii[i,"y"] - kk)^2 + hh, max(xx))
}



points(ii[1,"x"], ii[1,"y"], pch=16)

points(ii$x, ii$y, pch=16)

lines(xx, yy, lwd=2, col="black")

lines(xx[yy>2], yy[yy>2], lwd=4, col="red")

