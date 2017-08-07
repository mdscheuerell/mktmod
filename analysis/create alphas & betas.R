
# empty beta forecasts
betas4 <- betas4.se <- NULL

# empty alphas & betas
alphas <- alphas.se <- betas <- betas.se <- NULL

# CAPMs for each MPG
for(i in 1:length(mpg.ID)) {
	
	# print loop ID
	print(paste("i = ",i,"; MPG = ",mpg.ID[i], sep=""))
	
	# get popns
	prod.mpg <- as.matrix(prod.pop[,grep(mpg.ID[i], colnames(prod.pop))])

	# number of assets
	nn <- dim(prod.mpg)[2]
	
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
