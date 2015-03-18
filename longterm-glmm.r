## Beth Levick, University of Liverpool 2014
## to run GLMMs using glmmPQL from MASS package
## For model predicting long term occupancy patterns
require("MASS")
###############################################################################################################################
### Long term occupancy ###
setwd( DIRECTORY HERE )	#directory location of files
dat <- read.csv("transitions.csv", header=T, stringsAsFactors=F)
## set up environment for transitions file
## generates warnings due to data coercion, not an error
source( "longterm-setup.r" )

## non mixed models for AICs added 15/02/23

## landscape
model1 <- glmmPQL( sea_occ ~ factor2 + sand + loam + binfactor3 + allpl, random=~1|sector,
	family="binomial", data=factor_df, weights=wgt )
	
## non mixed model for AIC
model1nm <- glm( sea_occ ~ factor2 + sand + loam + binfactor3 + allpl, family="binomial", data=factor_df, weights=wgt )
AIC(model1nm)

## interaction
model1a <- glmmPQL( sea_occ ~ factor2 + sand + loam + binfactor3 * allpl, random=~1|sector,
	family="binomial", data=factor_df, weights=wgt )
	
## non mixed model for AIC
model1anm <- glm( sea_occ ~ factor2 + sand + loam + binfactor3 * allpl, family="binomial", data=factor_df, weights=wgt )
AIC(model1anm)

## significantly lower in the model with the interaction, so keep it to the next step

## additional factors
model2 <- glmmPQL( sea_occ ~ tree + lat + sand + binfactor3 * allpl, random=~1|sector,
	family="binomial", data=factor_df, weights=wgt )
	
## as non mixed model to get AIC
model2nm <- glm( sea_occ ~ tree + lat + sand + binfactor3 * allpl, family="binomial", data=factor_df, weights=wgt )	
AIC(model2nm)

## minimise
model3 <- glmmPQL( sea_occ ~ lat + sand + binfactor3 * allpl, random=~1|sector,
	family="binomial", data=factor_df, weights=wgt )	
#                        Value Std.Error  DF   t-value p-value
#(Intercept)        -162.18381 19.776317 503 -8.200911  0.0000
#lat                   3.54712  0.437234 503  8.112635  0.0000
#sand1                 0.67707  0.268848 503  2.518391  0.0121
#binfactor31           0.23259  0.201896 503  1.152024  0.2499
#allpl1                0.40945  0.263444 503  1.554227  0.1208
#binfactor31:allpl1   -1.73326  0.492770 503 -3.517376  0.0005

## as non mixed model to get AIC
model3nm <- glm( sea_occ ~ lat + sand + binfactor3 * allpl, family="binomial", data=factor_df, weights=wgt )
AIC(model3nm)

## Although this is significantly higher, no good reason to keep an NS term
## Lower AIC in previous model due to more parameters

## single term backwards deletion to identify if significant improvement to drop any terms
drop1(model3nm)