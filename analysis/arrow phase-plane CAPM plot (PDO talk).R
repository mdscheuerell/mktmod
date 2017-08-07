



# set up naming & ordering scheme
longnames <- c(
"Wenaha",
"Grande Ronde",
"Catherine",
"Minam",
"Lostine",
"Imnaha",
"SF Salmon",
"Secesh",
"EFSF Salmon",
"Big",
"Sulphur",
"Bear Valley",
"Marsh",
"Loon",
"Camas",
"Yankee Fork",
"Valley",
"UM Salmon",
"LM Salmon",
"EF Salmon",
"Lemhi")
name.ord <- c(6,3,5,4,2,1,12,10,15,14,13,11,9,7,8,20,21,19,18,17,16)
names.mat <- data.frame(ord=name.ord, ID=names.pop)
names.mat <- names.mat[order(names.mat$ord),]
names.mat$long <- longnames


#-----------
# plot data
#-----------

# set up colors; group by MPG
col.pal <- c(rainbow(6, start=0.05, end=0.12),
			 rainbow(3, start=0.2, end=0.45, v=0.7),
			 rainbow(6, start=0.8, end=0.95, v=0.85),
			 rainbow(6, start=0.55, end=0.7))

# ts plots of ln(Sw)
dev.new(height=5, width=9)
par(mai=c(0.9,0.9,0,0), omi=rep(0.1,4))
matplot(t.index, prod.pop[,names.mat$ID], type="n", col=col.pal, lwd=1.5,
		ylab="ln[spawners/ha]", xlab="Year", cex.axis=1.2, cex.lab=1.5, las=1)
matplot(t.index, prod.pop[,names.mat$ID], type="n", col=col.pal, lwd=1.5,
		ylab="ln[R/S]", xlab="Brood year", cex.axis=1.2, cex.lab=1.5, las=1)
abline(h=0, lty="dashed")
matlines(t.index, prod.pop[,names.mat$ID], type="l", col=col.pal, lwd=1.5)

png("spawner density vs year (64-06).png", width=9, height=5, units="in", res=300)
dev.off()


#--------------------------
# plot data & market index
#--------------------------

# ts plots of ln(Sw)
dev.new(height=5, width=9)
par(mai=c(0.9,0.9,0,0), omi=rep(0.1,4))
matplot(t.index, prod.pop[,names.mat$ID], type="n", col=col.pal, lwd=1.5,
		ylab="ln[R/S] or PDO", xlab="Brood year", cex.axis=1.2, cex.lab=1.5, las=1)
matplot(t.index, prod.pop[,names.mat$ID], type="n", col=col.pal, lwd=1.5,
		ylab="ln[spawners/ha] or NPGO", xlab="Year", cex.axis=1.2, cex.lab=1.5, las=1)
abline(h=0, lty="dashed")
matlines(t.index, prod.pop[,names.mat$ID], type="l", col=col.pal, lwd=1.5)
lines(t.index, prod.mkt, lwd=3)

png("spawner density vs year (PDO mkt 64-06).png", width=9, height=5, units="in", res=300)
dev.off()


#-----------------------------
# plot data & risk-free index
#-----------------------------

# ts plots of ln(Sw)
dev.new(height=5, width=9)
par(mai=c(0.9,0.9,0,0), omi=rep(0.1,4))
matplot(t.index, prod.pop[,names.mat$ID], type="n", col=col.pal, lwd=1.5,
		ylab="ln[R/S]", xlab="Brood year", cex.axis=1.2, cex.lab=1.5, las=1)
las=1)
abline(h=0, lty="dashed")
matlines(t.index, prod.pop[,names.mat$ID], type="l", col=col.pal, lwd=1.5)
lines(free.sub$year, free.sub$free, lwd=3)

png("prod vs year (JD Rf).png", width=9, height=5, units="in", res=300)
dev.off()


#-------------
# plot alphas 
#-------------

alphas <- alphas[,names.mat$ID]

# total num of ts
NN <- dim(alphas)[2]

# set up colors; group by MPG
col.pal <- c(rainbow(6, start=0.05, end=0.12),
			 rainbow(3, start=0.2, end=0.45, v=0.7),
			 rainbow(6, start=0.8, end=0.95, v=0.85),
			 rainbow(6, start=0.55, end=0.7))

# num of rows for plot
nr <- 3
# num of cols for plot
nc <- 7

# set graphics params
dev.new(width=9,height=5)
par(mfcol=c(nr,nc), mai=c(0.1,0.07,0.2,0.07), omi=c(0.6,0.6,0,0))

# get extent of y-limits
yext <- c(floor(min(alphas-2*alphas.se, na.rm=T)/0.5)*0.5,
		  ceiling(max(alphas+2*alphas.se, na.rm=T)/0.5)*0.5)
		  
yext <- c(floor(min(alphas-2*alphas.se, na.rm=T)/0.1)*0.1,
		  ceiling(max(alphas+2*alphas.se, na.rm=T)/0.1)*0.1)
		  
# draw plots
for(i in 1:NN) {
	plot(t.index, alphas[,i], type="n",
		 ylim=yext, xaxt="n", yaxt="n",
		 xlab="", ylab="", main=names.mat[i,"long"])
	rect(par()$usr[1], -1, par()$usr[2], 1, col=gray(0.9), border=gray(0.9))
	abline(h=0, lty="dashed")
	box()
	lines(t.index, alphas[,i], col=col.pal[i], lwd=2.2)
	lines(t.index, alphas[,i]+2*alphas.se[,i], col=col.pal[i], lwd=0.7)
	lines(t.index, alphas[,i]-2*alphas.se[,i], col=col.pal[i], lwd=0.7)
#	text(min(t.index)+5, yext[2]-0.5, names.pop[i])	
	if(i<=nr | nc==1) {
		axis(side=2, at=seq(ceiling(yext[1]),floor(yext[2])), las=1)
#		axis(side=2, at=seq(yext[1],yext[2])[(seq(yext[1],yext[2])%%1)==0])
		#axis(side=2, at=seq(0,yext[2], 2), las=1, tick=TRUE)
		}
	if(i%%nr==0) {
		axis(side=1, at=t.index[(t.index %% 10)==0])
		}
	}
#mtext("Brood year", side=1, outer=TRUE, line=3, cex=1.2)
mtext("Year", side=1, outer=TRUE, line=3, cex=1.2)
mtext(expression(alpha[t]), side=2, outer=TRUE, line=2.2, cex=1.5)

png("Sp alpha vs year (PDO mkt 64-06).png", width=9, height=5, units="in", res=300)
dev.off()


#------------
# plot betas
#------------

betas <- betas[,names.mat$ID]

# total num of ts
NN <- dim(betas)[2]

# set up colors; group by MPG
col.pal <- c(rainbow(6, start=0.05, end=0.12),
			 rainbow(3, start=0.2, end=0.45, v=0.7),
			 rainbow(6, start=0.8, end=0.95, v=0.85),
			 rainbow(6, start=0.55, end=0.7))

# inches of indent for panel labels
marg <- 0.05
# x-indent
xin <- par()$usr[1] + marg/par()$pin[1]*(par()$usr[2] - par()$usr[1])
# y-indent
yin <- par()$usr[4] - marg/par()$pin[2]*(par()$usr[4] - par()$usr[3])

# num of rows for plot
nr <- 3
# num of cols for plot
nc <- 7

# set graphics params
dev.new(width=9,height=5)
par(mfcol=c(nr,nc), mai=c(0.1,0.07,0.2,0.07), omi=c(0.6,0.6,0,0))

# get extent of y-limits
yext <- c(floor(min(betas-2*betas.se, na.rm=T)/0.5)*0.5,
		  ceiling(max(betas+2*betas.se, na.rm=T)/0.5)*0.5)
yext <- c(floor(min(betas-2*betas.se, na.rm=T)/0.1)*0.1,
		  ceiling(max(betas+2*betas.se, na.rm=T)/0.1)*0.1)


# draw plots
for(i in 1:NN) {
	plot(t.index, betas[,i], type="n",
		 ylim=yext, xaxt="n", yaxt="n",
		 xlab="", ylab="", main=names.mat[i,"long"])
	rect(par()$usr[1], -1, par()$usr[2], 1, col=gray(0.9), border=gray(0.9))
	abline(h=0, lty="dashed")
	box()
	lines(t.index, betas[,i], col=col.pal[i], lwd=2.2)
	lines(t.index, betas[,i]+2*betas.se[,i], col=col.pal[i], lwd=0.7)
	lines(t.index, betas[,i]-2*betas.se[,i], col=col.pal[i], lwd=0.7)
	#text(xin, yin, names.pop[i], adj=c(0,1))	
	if(i<=nr | nc==1) {
#		axis(side=2, at=seq(ceiling(yext[1]/2)*2,floor(yext[2]/2)*2), las=1)
#		axis(side=2, at=seq(yext[1],yext[2])[(seq(yext[1],yext[2])%%1)==0])
		axis(side=2, at=seq(-1,2), las=1, tick=TRUE)
		}
	if(i%%nr==0) {
		axis(side=1, at=t.index[(t.index %% 10)==0])
		}
	}
#mtext("Brood year", side=1, outer=TRUE, line=3, cex=1.2)
mtext("Year", side=1, outer=TRUE, line=3, cex=1.2)
mtext(expression(beta[italic(t)]), side=2, outer=TRUE, line=2.2, cex=1.5)

png("Sp beta vs year (PDO mkt 64-06).png", width=9, height=5, units="in", res=300)
dev.off()


#-------------------
# plot dynamic CAPM
#-------------------

# total num of ts
NN <- dim(betas4)[2]

# set up colors
col.choices <- c("red2","orange","green3","dodgerblue","darkviolet")
col.pal <- rep(col.choices,each=10)[1:TT]

# get asset surplus with Rf != 0
prod.sur <- prod.pop[,names.mat$ID] - (diag(prod.mkt)-diag(Rf))%*%betas4

# rearrange returns & betas
pds <- prod.sur[,names.mat$ID]
bts <- betas4[,names.mat$ID]

# build list of plot data
plot.dat <- list()
for(i in 1:NN) {
 	plot.dat[[i]] <- data.frame(beta=bts[!is.na(pds[,i]),i],
 								return=pds[!is.na(pds[,i]),i],
 								color=col.pal[!is.na(pds[,i])],
 								stringsAsFactors=FALSE)
	}

# num of rows for plot
nr <- 3
# num of cols for plot
nc <- 7

# set graphics params
#dev.new(height=9, width=6.5)
dev.new(height=5, width=9)
#par(mfrow=c(nr,nc), mai=rep(0.07,4), omi=c(0.6,0.6,0,0))
par(mfcol=c(nr,nc), mai=c(0.1,0.07,0.2,0.07), omi=c(0.6,0.6,0,0))

# get extent of x-limits
xext <- c(floor(min(betas4, na.rm=T)/0.5)*0.5,
		  ceiling(max(betas4, na.rm=T)/0.5)*0.5)
# get extent of y-limits
yext <- c(floor(min(prod.sur, na.rm=T)/0.1)*0.1,
		  ceiling(max(prod.sur, na.rm=T)/0.1)*0.1)

# draw plots
for(i in 1:NN) {
	plot(plot.dat[[i]]$beta, plot.dat[[i]]$return, type="n",
		 xlim=xext, ylim=yext, xaxt="n", yaxt="n",
		 xlab="", ylab="", main=names.mat[i,"long"])
	rect(-1, par()$usr[3], 1, par()$usr[4], col=gray(0.9), border=gray(0.9))
	abline(h=0, lty="dashed")
	abline(v=0, lty="dashed")
	box()
	s <- seq(length(plot.dat[[i]]$beta)-1)
	arrows(plot.dat[[i]]$beta[s], plot.dat[[i]]$return[s],
		   plot.dat[[i]]$beta[s+1], plot.dat[[i]]$return[s+1],
		   length = 0.07, angle = 15,
	       code = 2, col=gray(0.3))
	for(j in 1:length(plot.dat[[i]]$beta)) {
		points(plot.dat[[i]]$beta[j], plot.dat[[i]]$return[j],
				col=gray(0.3), bg=plot.dat[[i]]$color[j], pch=21, cex=1.2)
		}
	if(i<=nr | nc==1) {
		axis(side=2, at=seq(ceiling(yext[1]/2)*2,floor(yext[2]/2)*2,2), las=1)
		}
	if(i%%nr==0) {
		axis(side=1, at=seq(ceiling(yext[1]),floor(yext[2])))
		}
	}
#mtext(expression(beta[italic(t)]), side=1, outer=TRUE, line=3, cex=1.5)
mtext(expression(paste("Forecasted systematic risk in year ",italic(t),"+1")), side=1, outer=TRUE, line=3.5, cex=1.3)
#mtext("Adjusted return", side=2, outer=TRUE, line=2.5, cex=1.2)
mtext(expression(paste("Adjusted return in year ",italic(t),"+1")),
	  side=2, outer=TRUE, line=2.4, cex=1.4)


#par(mfg=c(2,3))

png("Sp beta vs return (PDO mkt 0 risk 64-06).png", width=9, height=5, units="in", res=300)
dev.off()


#-------------------
# plot beta vs prod
#-------------------

# total num of ts
NN <- dim(betas)[2]

# set up colors
col.choices <- c("red2","orange","green3","dodgerblue","darkviolet")
col.pal <- rep(col.choices,each=10)[1:TT]

# get asset surplus
prod.sur <- prod.pop - alphas + betas*prod.mkt
prod.sur <- prod.pop - prod.mkt

# rearrange returns & betas
pds <- prod.sur[,names.mat$ID]
bts <- betas[,names.mat$ID]

# build list of plot data
plot.dat <- list()
for(i in 1:NN) {
 	plot.dat[[i]] <- data.frame(beta=bts[!is.na(pds[,i]),i],
 								return=pds[!is.na(pds[,i]),i],
 								color=col.pal[!is.na(pds[,i])],
 								stringsAsFactors=FALSE)
	}

# num of rows for plot
nr <- 3
# num of cols for plot
nc <- 7

# set graphics params
#dev.new(height=9, width=6.5)
dev.new(height=5, width=9)
#par(mfrow=c(nr,nc), mai=rep(0.07,4), omi=c(0.6,0.6,0,0))
par(mfcol=c(nr,nc), mai=c(0.1,0.07,0.2,0.07), omi=c(0.6,0.6,0,0))

# inches of indent for panel labels
marg <- 0.05
# x-indent
xin <- par()$usr[2] - marg/par()$pin[1]*(par()$usr[2] - par()$usr[1])
# y-indent
yin <- par()$usr[4] - marg/par()$pin[2]*(par()$usr[4] - par()$usr[3])

# get extent of x-limits
xext <- c(floor(min(betas, na.rm=T)/0.5)*0.5,
		  ceiling(max(betas, na.rm=T)/0.5)*0.5)
# get extent of y-limits
yext <- c(floor(min(prod.sur, na.rm=T)/0.1)*0.1,
		  ceiling(max(prod.sur, na.rm=T)/0.1)*0.1)

# draw plots
for(i in 1:NN) {
	plot(plot.dat[[i]]$beta, plot.dat[[i]]$return, type="n",
		 xlim=xext, ylim=yext, xaxt="n", yaxt="n",
		 xlab="", ylab="", main=names.mat[i,"long"])
	rect(-1, par()$usr[3], 1, par()$usr[4], col=gray(0.9), border=gray(0.9))
	abline(h=0, lty="dashed")
	abline(v=0, lty="dashed")
	box()
	s <- seq(length(plot.dat[[i]]$beta)-1)
	arrows(plot.dat[[i]]$beta[s], plot.dat[[i]]$return[s],
		   plot.dat[[i]]$beta[s+1], plot.dat[[i]]$return[s+1],
		   length = 0.07, angle = 15,
	       code = 2, col=gray(0.3))
	for(j in 1:length(plot.dat[[i]]$beta)) {
		points(plot.dat[[i]]$beta[j], plot.dat[[i]]$return[j],
				col=gray(0.3), bg=plot.dat[[i]]$color[j], pch=21, cex=1.2)
		}
	#text(xin, yin, names.mat[i,"long"], adj=c(1,1))	
	#text(xin, yin, names.mat[i,"long"], adj=c(1,1), bg="white")	
#	if(i%%nc==1 | nc==1) {
	if(i<=nr | nc==1) {
		axis(side=2, at=seq(ceiling(yext[1]/2)*2,floor(yext[2]/2)*2,2), las=1)
		}
#	if(i>(nc*nr-nc)) {
	if(i%%nr==0) {
		axis(side=1, at=seq(ceiling(yext[1]),floor(yext[2])))
		}
	if(i==1) {
#		legend(x=2.49, y=4.3,
#		legend(x=c(1,xext[2])+0.01, y=c(3,3),
#		legend(x=1.01, y=3.6,
#			   legend=c("1960s","1970s","1980s","1990s","2000s"), cex=0.9,
#		       pch=rep(21,5), col=gray(0.3),
#		       bty="o", box.col="white", box.lwd=0.1, bg="white",
#		       pt.bg = col.choices,
#		       pt.cex=1.2, x.intersp=0.7, y.intersp=1.1,
#		       adj = c(0, 0.5))
		}
	}
#mtext(expression(beta[italic(t)]), side=1, outer=TRUE, line=3, cex=1.5)
mtext(expression(paste("Systematic risk in year ",italic(t))), side=1, outer=TRUE, line=3.5, cex=1.3)
#mtext("Adjusted return", side=2, outer=TRUE, line=2.5, cex=1.2)
mtext(expression(paste("Adjusted return in year ",italic(t))),
	  side=2, outer=TRUE, line=2.4, cex=1.4)


#par(mfg=c(2,3))

png("beta vs return (PDO mkt index).png", width=9, height=5, units="in", res=300)
dev.off()





