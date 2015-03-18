## Bethany Levick, University of Liverpool 2014 ##
## iterative sampling of the burrow data 
## to run repeats of the model with different samples
## to ensure correction of sampling is valid
require("MASS")
setwd(DIRECTORY HERE)
#open data
dat <- read.csv( "kaz_14-04-15.csv", header=T )
# set up environment for occupancy as a binary response
# and set up data frame to run models from
source( "occupancy-setup.r" )

## set up model to run
## give the model here, to be run inside the boot.model function
runModel <- function(){
## enter model exactly as it should be run
## inputs will be taken in boot environment
model <- glmmPQL( occupied ~ lat + tree + season + allpl, random=~1|sector/burrow,
	family="binomial", data=factor_df )
return(model)
}

## where nruns is the number of runs
## dataset is the dataframe to take samples from and run model with
## bootby is the parameter to subsample from (i.e. burrow)
nruns <- 100
dataset <- factor_df
bootby <- "burrow"

source("boot-model.r")			## to run the "boot"

save( boots, file="occupancy-boot.RData" )

#####################################################################################################
## once the iterative modelling has been run once, run from here
## using the saved .RDa file
require("MASS")
setwd(DIRECTORY HERE)
#open data
dat <- read.csv( "kaz_14-04-15.csv", header=T )
# set up environment for occupancy as a binary response
# and set up data frame to run models from
source( "occupancy-setup.r" )

## to load the list object boots
load( "occupancy-boot.RData" )

## remake the list object as a data frame
results <- boots[[1]]

for(k in 2:length(boots)){
results <- rbind( results, boots[[k]] )
}

confidence <- data.frame( factors=boots[[1]]$factor, coeff.lo=c(rep(0,(nrow(boots[[1]])))), coeff.m=c(rep(0,(nrow(boots[[1]])))),
	coeff.hi=c(rep(0,(nrow(boots[[1]])))), p.lo=c(rep(0,(nrow(boots[[1]])))), p.m=c(rep(0,(nrow(boots[[1]])))),
		p.hi=c(rep(0,(nrow(boots[[1]])))) )

coeff.list <- list()
p.list <- list()

fac <- unique( results$factor )

for(k in 1:length(fac)){
tmp <- results[results$factor==fac[k],]
coeff.list[[k]] <- as.numeric(tmp$coeff)
p.list[[k]] <- as.numeric(tmp$p)
} 		
	
for(k in 1:length(fac)){
confidence$coeff.m[k] <- mean(coeff.list[[k]], na.rm=T)
se <- sd(coeff.list[[k]], na.rm=T)/sqrt(length(coeff.list[[k]]))
confidence$coeff.lo[k] <- mean(coeff.list[[k]], na.rm=T) - (1.96 * se)
confidence$coeff.hi[k] <- mean(coeff.list[[k]], na.rm=T) + (1.96 * se)

confidence$p.m[k] <- mean(p.list[[k]], na.rm=T)
se <- sd(p.list[[k]], na.rm=T)/sqrt(length(p.list[[k]]))
confidence$p.lo[k] <- mean(p.list[[k]], na.rm=T) - (1.96 * se)
confidence$p.hi[k] <- mean(p.list[[k]], na.rm=T) + (1.96 * se)
}

######################################################################################################################
## plot comparison with estimates from original glmm
## run the model
model <- glmmPQL( occupied ~ lat + tree + season + allpl, random=~1|sector/burrow,
	family="binomial", data=factor_df )
	
## save values
summ <- summary(model)
## save results in matrix object
results <- summ$tTable
coeff <- results[,1]
p <- results[,5]

## for coeff graph ##

## trim "season" from labels to fit on plot better
nams <- c("Latitude", "Tree", "AU12", "SP13", "SU12", "Clay")
##########
par( pty="s", mfrow=c(1,2) )

plot( 0,0, type="n", xlim=c(0,nrow(confidence)), ylim=c(-3,3), xaxt="n",
	xlab="factor", ylab="coefficient estimate", main="95% CI for confidence estimates")
axis( side=1, at=seq(1:length(confidence$factor)), labels=nams )

#for(k in 1:7){
points( c(1:length(confidence$coeff.lo)), confidence$coeff.lo, pch=16, col=rgb(1,0,0,0.4), cex=2 )
points( c(1:length(confidence$coeff.m)), confidence$coeff.m, pch=16, col="black" )
points( c(1:length(confidence$coeff.hi)), confidence$coeff.hi, pch=16, col=rgb(1,0,0,0.4), cex=2 )
#}

## add on estimates from model
points( c(1:((length(coeff))-1)), coeff[2:length(coeff)], pch=16, col="green" )

## for p value graph ##

plot( 0,0, type="n", xlim=c(0,nrow(confidence)), ylim=c(0,0.5), xaxt="n",
	xlab="factor", ylab="P value", main="95% CI for P values")
axis( side=1, at=seq(1:length(confidence$factor)), labels=nams )

#for(k in 1:8){
points( c(1:length(confidence$p.lo)), confidence$p.lo, pch=16, col=rgb(1,0,0,0.4), cex=2 )
points( c(1:length(confidence$p.m)), confidence$p.m, pch=16, col="black" )
points( c(1:length(confidence$p.hi)), confidence$p.hi, pch=16, col=rgb(1,0,0,0.4), cex=2 )
#}

## add on estimates from model
points( c(1:((length(p))-1)), p[2:length(p)], pch=16, col="green" )

abline( h=0.05, col="blue" )

legend( "topleft", pch=c(16, 16, 16), col=c("red", "black", "green"), legend=c("95% CI", "boot estimate", "original model estimate" ),
	bty="n" )	