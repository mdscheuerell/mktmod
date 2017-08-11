


#-------------
# user inputs
#-------------

# first brood year to consider
yr.first <- 1964

# last brood year to consider
yr.last <- 2006

# time lag for market index
lag <- 2

# name of market index file
mkt.file <- "PDO.csv"

# name of risk-free index file
free.file <- "zero.csv"

# name of data file
data.file <- "ICTRT_Chinook_R-S_data_v130701.txt"

# popns to exclude?
pops.out <- c("Tucannon", "Pahsimeroi")

# MPG abbreviations
mpg.ID <- c("GR", "MF", "SF", "SR")

# response var; can be 1 of 3 options:
# "log.Sw" = log_e of (natural-origin) spawners per ha
# "log.Rw.Sw" = log_e of (natural-origin) recruits per spawner
# "resids" = resids from popn-specific Ricker model
resp.var <- "log.Rw.Sw"

# should asset index be z-scored
z.flag <- FALSE


#-------
# inits
#-------

# load necessary pkgs
library(reshape)
library(MARSS)


#----------------
# data retrieval
#----------------

# years of interest
t.index <- seq(yr.first, yr.last)

# length of time period
TT <- length(t.index)

# get mkt index file
mkt.table <- read.csv(mkt.file, header=TRUE, sep=",")

# get subset of mkt index
mkt.sub <- mkt.table[mkt.table$year>=(yr.first+lag) & mkt.table$year<=(yr.last+lag),]

# get mkt index file
free.table <- read.csv(free.file, header=TRUE, sep=",")

# get subset of mkt index
free.sub <- free.table[free.table$year>=yr.first & free.table$year<=yr.last,]

# get data file
data.table <- read.table(data.file, header=TRUE, sep="\t")

# change Imnaha name for easier sorting later
data.table[,"code"] <- sub("IRMAI", "GR.IR", data.table[,"code"])

# drop any popns
data.table <- data.table[!(data.table$popn %in% pops.out),]

# get subset of data
data.sub <- subset(data.table,
				   brood.yr>=yr.first & brood.yr<=yr.last,
				   select=c(mpg, code, brood.yr, nSw, ha, log.Rw.Sw))

# names of popns
names.pop <- colnames(cast(data.sub, brood.yr ~ code, value="nSw")[,-1])

# "productivity" as resids from Ricker model
if(resp.var == "resids") {
	data.sub[,"resids"] <- NA
	for(i in 1:length(names.pop)) {
		id <- (data.sub[,"code"] %in% names.pop[i]) &
			   !is.na(data.sub[,"nSw"]) &
			   !is.na(data.sub[,"log.Rw.Sw"])
		mm <- lm(data.sub[id,"log.Rw.Sw"] ~ data.sub[id,"nSw"])
		data.sub[id,"resids"] <- mm$residuals
		}
	}

if(resp.var == "log.Sw") {
	data.sub[,"log.Sw"] <- NA
	id <- (data.sub[,"nSw"] != 0) & !is.na(data.sub[,"nSw"])
	data.sub[id,"log.Sw"] <- log(data.sub[id,"nSw"]/data.sub[id,"ha"])
	}


#---------------------------
# calculate rates of return 
#---------------------------

# 1) assets (ie, pop-level Ricker residuals)
prod.pop <- cast(data.sub, brood.yr ~ code, value=resp.var)[,-1]

# z-score for easier comparison
if(z.flag) {
	for(i in 1:dim(prod.pop)[2]) {
		mm <- mean(prod.pop[,i], na.rm=TRUE)
		sd <- sqrt(var(prod.pop[,i], na.rm=TRUE))
		prod.pop[,i] <- (prod.pop[,i]-mm)/sd
		}
	}

# 2) market  (ie, mean annual resids for esu)
prod.mkt <- mkt.sub[,-1]
# z-score for easier comparison
prod.mkt <- (prod.mkt-mean(prod.mkt))/sqrt(var(prod.mkt))

# 3) risk-free (ie, stock replacement; ln[R/S] = 0)
Rf <- free.sub[,"free"]
free.1st <- free.sub[1,"year"]
yr.diff <- yr.first - free.1st
if(yr.diff < 0) { Rf <- c(rep(NA,abs(yr.diff)),Rf) }
Rf <- rep(0,length(prod.mkt))


#---------------
# CAPM in MARSS
#---------------

# empty list for model results
mod.res <- list()

# empty beta forecasts
betas4 <- betas4.se <- NULL

# empty alphas & betas
alphas <- alphas.se <- betas <- betas.se <- NULL

# number of regr params
mm <- 2

# CAPMs for each MPG
for(i in 1:length(mpg.ID)) {
	
	# print loop ID
	print(paste("i = ",i,"; MPG = ",mpg.ID[i], sep=""))
	
	# get popns
	prod.mpg <- as.matrix(prod.pop[,grep(mpg.ID[i], colnames(prod.pop))])

	# number of assets
	nn <- dim(prod.mpg)[2]
	
	# Process eqn
	# B is Identity
	BB <- "identity"
	# U is col vec of 0's
	UU <- "zero"
	# Q will be block diagonal
	QQ <- matrix(list(0),mm*nn,mm*nn)
	# upper L block for alpha
	aa <- 1:nn
	#QQ[aa,aa] <- "alpha.cov"
	#diag(QQ[aa,aa]) <- "alpha.var"
	diag(QQ[aa,aa]) <- paste0("alpha",seq(nn))
	for(j in 1:(nn-1)) {
		for(k in (j+1):nn) {
			QQ[j,k] <- paste0("alpha",j,k)
			QQ[k,j] <- paste0("alpha",j,k)
		}
	}
	# lower R block for beta
	bb <- aa + nn
	#QQ[bb,bb] <- "beta.cov"
	#diag(QQ[bb,bb]) <- "beta.var"
	diag(QQ[bb,bb]) <- paste0("beta",seq(nn))
	for(j in 1:(nn-1)) {
		for(k in (j+1):nn) {
			QQ[j+nn,k+nn] <- paste0("beta",j,k)
			QQ[k+nn,j+nn] <- paste0("beta",j,k)
		}
	}
	
	QQ <- "unconstrained"
	## QQ <- "diagonal and equal"
	
	# Observation eqn
	# Z is array of regr variables (ie, 1's and market returns)
	ZZ <- array(NA, c(nn,mm*nn,TT))
	for(t in 1:TT) {
		ZZ[,,t] <- cbind(1,prod.mkt[t]) %x% diag(nn)
		}
	# A is col vec of 0's
	AA <- "zero"
	# assume same obs var & cov
	#RR <- matrix(list(0),nn,nn)
	#RR[aa,aa] <- "obs.cov"
	#diag(RR[aa,aa]) <- "obs.var"
	RR <- "diagonal and equal"
	
	# Initial states
	#x0 <- rbind(matrix("pi.a",nn,1), matrix("pi.b",nn,1))
	#V0 <- 100*diag(mm*nn)
	#x0 <- rbind(matrix(0,nn,1), matrix(1,nn,1))
	#V0 <- matrix(list(0),mm*nn,mm*nn)
	#diag(V0[aa,aa]) <- "var.a"
	#diag(V0[bb,bb]) <- "var.b"
	 
	# starting values for regr parameters
	inits.list <- list(x0=matrix(0, mm*nn, 1))
	
	# list of model matrices & vectors
	mod.list <- list(B=BB, U=UU, Q=QQ, Z=ZZ, A=AA, R=RR)
	#mod.list <- list(B=BB, U=UU, Q=QQ, Z=ZZ, A=AA, R=RR, x0=x0, V0=V0)
	
	# list of control values
	con.list <- list(safe=TRUE, allow.degen=FALSE,
	                 conv.test.slope.tol=2, abstol=1e-04, maxit=5e04)
	
	# fit multivariate DLM
	mod.res[[i]] <- MARSS(t(prod.mpg), inits=inits.list, model=mod.list, control=con.list)
	
	# get list of Kalman filter output
	kf.out = MARSSkfss(mod.res[[i]])

	# ts of forecasted betas
	betas4 <- cbind(betas4,t(kf.out$xtt1[(1:nn)+nn,]))

	# ts of SE of betas; nxT matrix
	betas4.se <- cbind(betas4.se,sqrt(t(apply(kf.out$Vtt1,3,diag)[(1:nn)+nn,])))

	# ts of alphas
	alphas <- cbind(alphas,t(mod.res[[i]]$states[1:nn,]))
	# ts of SE{alphas}
	alphas.se <- cbind(alphas.se,t(mod.res[[i]]$states.se[1:nn,]))
	
	# ts of betas
	betas <- cbind(betas,t(mod.res[[i]]$states[(1:nn)+nn,]))
	# ts of SE{betas}
	betas.se <- cbind(betas.se,t(mod.res[[i]]$states.se[(1:nn)+nn,]))
	
	}

# assign names
colnames(alphas) <- colnames(alphas.se) <- names.pop
colnames(betas) <- colnames(betas.se) <- names.pop
colnames(betas4) <- colnames(betas4.se) <- names.pop

# save model results
save(list=ls(), file=paste0("dlmCAPM_",resp.var,"_",sub(".csv","",mkt.file,),"_v",Sys.Date(),".RData"))


#-------------
# plot alphas 
#-------------

# total num of ts
NN <- dim(alphas)[2]

# set up colors; group by MPG
col.pal <- c(rainbow(6, start=0.04, end=0.11),
			 rainbow(6, start=0.55, end=0.65),
			 rainbow(3, start=0.32, end=0.4),
			 rainbow(6, start=0.8, end=0.9))
# set colors
col.GR <- rainbow(length(popns.GR), start=0.05, end=0.12)
col.MF <- rainbow(length(popns.MF), start=0.8, end=0.95, v=0.85)
col.SF <- rainbow(length(popns.SF), start=0.2, end=0.45, v=0.7)
col.SR <- rainbow(length(popns.SR), start=0.55, end=0.7)

# num of rows for plot
nr <- 7
# num of cols for plot
nc <- 3

# set graphics params
dev.new(height=9,width=6.5)
par(mfrow=c(nr,nc), mai=rep(0.075,4), omi=c(0.6,0.6,0,0))

# get extent of y-limits
yext <- c(floor(min(alphas-2*alphas.se, na.rm=T)/0.5)*0.5,
		  ceiling(max(alphas+2*alphas.se, na.rm=T)/0.5)*0.5)
# draw plots
for(i in 1:NN) {
	plot(t.index, alphas[,i], type="n",
		 ylim=yext, xaxt="n", yaxt="n",
		 xlab="", ylab="", main="")
	rect(par()$usr[1], -1, par()$usr[2], 1, col=gray(0.9), border=gray(0.9))
	abline(h=0, lty="dashed")
	box()
	lines(t.index, alphas[,i], col=col.pal[i], lwd=2.2)
	lines(t.index, alphas[,i]+2*alphas.se[,i], col=col.pal[i], lwd=0.7)
	lines(t.index, alphas[,i]-2*alphas.se[,i], col=col.pal[i], lwd=0.7)
	text(min(t.index)+5, yext[2]-0.5, names.pop[i])	
	if(i%%nc==1 | nc==1) {
		axis(side=2, at=seq(yext[1],yext[2])[(seq(yext[1],yext[2])%%1)==0])
		#axis(side=2, at=seq(0,yext[2], 2), las=1, tick=TRUE)
		}
	if(i>(nc*nr-nc)) {
		axis(side=1, at=t.index[(t.index %% 5)==0])
		}
	}
mtext("Brood year", side=1, outer=TRUE, line=3, cex=1.2)
mtext(expression(alpha[t]), side=2, outer=TRUE, line=2.2, cex=1.5)


#------------
# plot betas
#------------

# total num of ts
NN <- dim(alphas)[2]

# set up colors; group by MPG
col.pal <- c(rainbow(6, start=0.04, end=0.11),
			 rainbow(6, start=0.55, end=0.65),
			 rainbow(3, start=0.28, end=0.38),
			 rainbow(6, start=0.8, end=0.9))

# inches of indent for panel labels
marg <- 0.05
# x-indent
xin <- par()$usr[1] + marg/par()$pin[1]*(par()$usr[2] - par()$usr[1])
# y-indent
yin <- par()$usr[4] - marg/par()$pin[2]*(par()$usr[4] - par()$usr[3])

# num of rows for plot
nr <- 7
# num of cols for plot
nc <- 3

# set graphics params
dev.new(height=9,width=6.5)
par(mfrow=c(nr,nc), mai=rep(0.075,4), omi=c(0.6,0.6,0,0))

# get extent of y-limits
yext <- c(floor(min(betas-2*betas.se, na.rm=T)/0.5)*0.5,
		  ceiling(max(betas+2*betas.se, na.rm=T)/0.5)*0.5)
# draw plots
for(i in 1:NN) {
	plot(t.index, betas[,i], type="n",
		 ylim=yext, xaxt="n", yaxt="n",
		 xlab="", ylab="", main="")
	rect(par()$usr[1], -1, par()$usr[2], 1, col=gray(0.9), border=gray(0.9))
	abline(h=0, lty="dashed")
	box()
	lines(t.index, betas[,i], col=col.pal[i], lwd=2.2)
	lines(t.index, betas[,i]+2*betas.se[,i], col=col.pal[i], lwd=0.7)
	lines(t.index, betas[,i]-2*betas.se[,i], col=col.pal[i], lwd=0.7)
	text(xin, yin, names.pop[i], adj=c(0,1))	
	if(i%%nc==1 | nc==1) {
		axis(side=2, at=seq(ceiling(yext[1]),floor(yext[2])))
		#axis(side=2, at=seq(0,yext[2], 2), las=1, tick=TRUE)
		}
	if(i>(nc*nr-nc)) {
		axis(side=1, at=t.index[(t.index %% 5)==0])
		}
	}
mtext("Brood year", side=1, outer=TRUE, line=3, cex=1.2)
mtext(expression(beta[italic(t)]), side=2, outer=TRUE, line=2.2, cex=1.5)

png("beta vs year (UC mkt index).png", width=6.5, height=9, units="in", res=600)
dev.off()


#-------------------
# plot beta vs prod
#-------------------

# total num of ts
NN <- dim(alphas)[2]

# set up colors
col.pal <- rainbow(TT, start=0, end=0.8, alpha=0.6)

# get asset surplus
prod.sur <- prod.pop - alphas + betas*prod.mkt
prod.sur <- prod.pop - prod.mkt

# num of rows for plot
nr <- 7
# num of cols for plot
nc <- 3

# set graphics params
dev.new(height=9,width=6.5)
par(mfrow=c(nr,nc), mai=rep(0.075,4), omi=c(0.6,0.6,0,0))

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
yext <- c(floor(min(prod.sur, na.rm=T)/0.5)*0.5,
		  ceiling(max(prod.sur, na.rm=T)/0.5)*0.5)

# draw plots
for(i in 1:NN) {
	plot(betas[,i], prod.sur[,i], type="n",
		 xlim=xext, ylim=yext, xaxt="n", yaxt="n",
		 xlab="", ylab="", main="")
	rect(-1, par()$usr[3], 1, par()$usr[4], col=gray(0.9), border=gray(0.9))
	abline(h=0, lty="dashed")
	abline(v=0, lty="dashed")
	box()
	lines(betas[,i], prod.sur[,i], col="gray",lwd=1)
	for(j in 1:TT) {
#		points(betas[j,i], prod.sur[j,i], col=col.pal[j], pch=16, cex=1.3)
		points(betas[j,i], prod.sur[j,i], col=col.pal[j], pch=16, cex=(1-j/TT+1))
		}
	text(xin, yin, names.pop[i], adj=c(1,1))	
	if(i%%nc==1 | nc==1) {
		axis(side=2, at=seq(ceiling(yext[1]),floor(yext[2])))
		}
	if(i>(nc*nr-nc)) {
		axis(side=1, at=seq(ceiling(yext[1]),floor(yext[2])))
		}
	}
mtext(expression(beta[italic(t)]), side=1, outer=TRUE, line=3, cex=1.5)
mtext("Adjusted return", side=2, outer=TRUE, line=2.5, cex=1.2)

png("beta vs return (UC mkt index).png", width=6.5, height=9, units="in", res=600)
dev.off()


#----------------------------
# plot ts of betas & returns
#----------------------------

# total num of ts
NN <- dim(betas)[2]

# num of rows for plot
nr <- 7
# num of cols for plot
nc <- 3

# set graphics params
dev.new(height=9,width=6.5)
par(mfrow=c(nr,nc), mai=rep(0.075,4), omi=c(0.6,0.6,0,0))

# inches of indent for panel labels
marg <- 0.05
# x-indent
xin <- par()$usr[1] + marg/par()$pin[1]*(par()$usr[2] - par()$usr[1])
# y-indent
yin <- par()$usr[4] - marg/par()$pin[2]*(par()$usr[4] - par()$usr[3])

# get extent of y-limits
yext <- c(floor(min(betas, na.rm=T)/0.5)*0.5,
		  ceiling(max(betas, na.rm=T)/0.5)*0.5)
# draw plots
for(i in 1:NN) {
	plot(t.index, betas[,i], type="n",
		 ylim=yext, xaxt="n", yaxt="n",
		 xlab="", ylab="", main="")
	rect(par()$usr[1], -1, par()$usr[2], 1, col=gray(0.9), border=gray(0.9))
	abline(h=0, lty="dashed")
	box()
	lines(t.index, betas[,i], col="steelblue", lwd=2.2)
	lines(t.index, prod.sur[,i], col="indianred", lwd=2.2)
	text(xin, yin, names.pop[i], adj=c(0,1))	
	if(i%%nc==1 | nc==1) {
		#axis(side=2, at=seq(ceiling(yext[1]),floor(yext[2]),2))
		axis(side=2, at=seq(ceiling(yext[1]),floor(yext[2])),labels=c(-2,NA,0,NA,2,NA,4))
		}
	if(i>(nc*nr-nc)) {
		axis(side=1, at=t.index[(t.index %% 5)==0])
		}
	}
mtext("Brood year", side=1, outer=TRUE, line=3, cex=1.2)
#mtext(expression(beta[italic(t)]), side=2, outer=TRUE, line=2.2, cex=1.5)
mtext(text=c(expression(beta)," or ","Adjusted return"),
       col=c("steelblue","black","indianred"), adj=c(0.37,0.4,0.54),
       padj=0.5, side=2, outer=TRUE, line=3, cex=1.5)

png("ts of beta & return (UC mkt index).png", width=6.5, height=9, units="in", res=600)
dev.off()


