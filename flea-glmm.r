## Beth Levick, University of Liverpool 2014
## to run GLMMs using glmmPQL from MASS package
## To run models predicting flea index
require("MASS")
###############################################################################################################################
### Flea Index ###
setwd( DIRECTORY HERE )
#open data
dat <- read.csv( "kaz_14-04-15.csv", header=T )
## source code to set up data environment and dat frame containing predictors and response
source( "flea-setup.r" )
####################################################################################################

## landscape factors
model1 <- glmmPQL( findex ~ sand + allpl + loam + factorB, random=~1|sector/burrow,
	family="quasipoisson", data=factor_df )
	
## non mixed non quasi model to get likelihood
model1nmnq <- glm( findex ~ sand + allpl + loam + factorB, family="poisson", data=factor_df )
x <- logLik(model1nmnq)
## non mixed quasi model to get dispersion
model1nm <- glm( findex ~ sand + allpl + loam + factorB, family="quasipoisson", data=factor_df )	
y <- summary(model2nm)$dispersion
## estimate AIC
AIC <- x/y

### additional factors 
model2 <- glmmPQL( findex ~ pocc_season + tree + lat + season + tindex, random=~1|sector/burrow,
	family="quasipoisson", data=factor_df )	

## non mixed non quasi model to get likelihood
model2nmnq <- glm( findex ~ pocc_season + tree + lat + season + tindex, family="poisson", data=factor_df )
x <- logLik(model2nmnq)
## non mixed quasi model to get dispersion
model2nm <- glm( findex ~ pocc_season + tree + lat + season + tindex, family="quasipoisson", data=factor_df )	
y <- summary(model2nm)$dispersion
## estimate AIC
AIC <- x/y

model3 <- glmmPQL( findex ~ lat + season + tindex, random=~1|sector/burrow,
	family="quasipoisson", data=factor_df )

## non mixed non quasi model to get likelihood
model3nmnq <- glm( findex ~ lat + season + tindex, family="poisson", data=factor_df )
## logLik
x <- logLik(model3nmnq)
## non mixed quasi model to get dispersion parameter
model3nm <- glm( findex ~ lat + season + tindex, family="quasipoisson", data=factor_df )	
## dispersion parameter
y <- summary(model3nm)$dispersion
## estimate AIC
AIC <- x/y
	
model4 <- glmmPQL( findex ~ season + tindex, random=~1|sector/burrow,
	family="quasipoisson", data=factor_df )
	
## non mixed non quasi model to get likelihood
model4nmnq <- glm( findex ~ season + tindex, family="poisson", data=factor_df )
## logLik
x <- logLik(model4nmnq)
## non mixed quasi model to get dispersion parameter
model4nm <- glm( findex ~ season + tindex, family="quasipoisson", data=factor_df )	
## dispersion parameter
y <- summary(model4nm)$dispersion
## estimate AIC
AIC <- x/y

################################################################
## to generate plot
x <- predict( model4, data=factor_df$tindex, type="response", level=0 )

layout(matrix(c(1,1,2), 1, 3, byrow = TRUE))
plot( factor_df$tindex, factor_df$findex, type="p", col="darkgrey", pch=16, main="Burrow flea burden and tick burden",
	xlab="Tick burden", ylab="Flea burden" )
points( factor_df$tindex, x, col="red", pch=16 )	

## to add a legend outside of the plot
plot( 0,0,type="n", xlab="", ylab="", main="Legend", bty="n", axes=F, xlim=c(0,3), ylim=c(0,2) )
points( 0.5,2, pch=16, col="darkgrey" )
text( 1.5,2,labels="Data" )
points( 0.5,1,pch=16, col="red" )
text( 1.5,1, labels="Prediction" )

