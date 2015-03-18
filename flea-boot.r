## Beth Levick, University of Liverpool 2014
## to run model iteratively sampling from burrows
require("MASS")
###############################################################################################################################
### Flea Index ###
setwd(DIRECTORY HERE)
#open data
dat <- read.csv( "kaz_14-04-15.csv", header=T )
## source code to set up data environment and dat frame containing predictors and response
source( "flea-setup.r" )

## set up model to run
## give the model here, to be run inside the boot.model function
runModel <- function(){
## enter model exactly as it should be run
## inputs will be taken in boot environment
model <- glmmPQL( findex ~ season + tindex, random=~1|sector,
	family="quasipoisson", data=factor_df )	
return(model)
}

## where nruns is the number of runs
## dataset is the dataframe to take samples from and run model with
## bootby is the parameter to subsample from (i.e. burrow)
nruns <- 100
dataset <- factor_df
bootby <- "burrow"

source("boot-model.r")			## to run the "boot"

save( boots, file="flea-boot.RData" )

####################################################################################
## to convert these saved results into a usable format
## once the above has been run once and the .RDa saved, run from here
require("MASS")
setwd( DIRECTORY HERE )
#open data
dat <- read.csv( "kaz_14-04-15.csv", header=T )
## source code to set up data environment and dat frame containing predictors and response
source( "flea-setup.r" )

## to load the list object boots
load( "flea-boot.RData" )

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

## run the minimal model
model <- glmmPQL( findex ~ season + tindex, random=~1|sector/burrow,
	family="quasipoisson", data=factor_df )	
	
## save values from minimal model
summ <- summary(model)
## save results in matrix object
results <- summ$tTable
coeff <- results[,1]
p <- results[,5]

## for coeff graph ##

## trim season names down for labels
confidence$factors <- gsub( "season", "", confidence$factors )
confidence$factors <- gsub( "20", "", confidence$factors )

par( pty="s", mfrow=c(1,2) )

plot( 0,0, type="n", xlim=c(0,nrow(confidence)), ylim=c(-3,3), xaxt="n",
	xlab="factor", ylab="coefficient estimate", main="95% CI for confidence estimates")
axis( side=1, at=seq(1:length(confidence$factor)), labels=confidence$factor )

#for(k in 1:7){
points( c(1:length(confidence$coeff.lo)), confidence$coeff.lo, pch=16, col=rgb(1,0,0,0.4), cex=2 )
points( c(1:length(confidence$coeff.m)), confidence$coeff.m, pch=16, col="black" )
points( c(1:length(confidence$coeff.hi)), confidence$coeff.hi, pch=16, col=rgb(1,0,0,0.4), cex=2 )
#}

## add on estimates from model
points( c(1:((length(coeff))-1)), coeff[2:length(coeff)], pch=16, col="green" )

## for p value graph ##

plot( 0,0, type="n", xlim=c(0,nrow(confidence)), ylim=c(0,0.8), xaxt="n",
	xlab="factor", ylab="P value", main="95% CI for P values")
axis( side=1, at=seq(1:length(confidence$factor)), labels=confidence$factor )

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