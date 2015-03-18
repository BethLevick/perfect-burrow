## Beth Levick, University of Liverpool 2014
## to run GLMMs using glmmPQL from MASS package
## For model predicting occupancy
require("MASS")
###############################################################################################################################
### Occupancy ###
## data set up 
## ensure data file and set up files stored in same directory as this file ##
setwd( DIRECTORY HERE )	#directory location of files
#open data
dat <- read.csv( "kaz_14-04-15.csv", header=T )
# set up environment for occupancy as a binary response
source( "occupancy-setup.r" )

## landscape
model1 <- glmmPQL( occupied ~ factor2 + sand + loam + binfactor3 + allpl, random=~1|sector/burrow,
	family="binomial", data=factor_df )
	
## Non mixed model for AIC
model1nm <- glm( occupied ~ factor2 + sand + loam + binfactor3 + allpl, family="binomial", data=factor_df )
AIC(model1nm)

## interaction
model1a <- glmmPQL( occupied ~ factor2 + sand + loam + binfactor3 * allpl, random=~1|sector/burrow,
	family="binomial", data=factor_df )

## non mixed model for AIC score
model1anm <- glm( occupied ~ factor2 + sand + loam + binfactor3 * allpl, family="binomial", data=factor_df )
AIC(model1anm)

## Adding interaction term does not significantly improve the model
## So we drop this and add in the additional factors to clay soils (allpl) only

## add factors
model2 <- glmmPQL( occupied ~ lat + tree + season + allpl, random=~1|sector/burrow,
	family="binomial", data=factor_df )
	
## non mixed model for AIC score
model2nm <- glm( occupied ~ lat + tree + season + allpl, family="binomial", data=factor_df )
AIC(model2nm)

## As all terms are significant, use drop1 to identify any that are significant improvements to remove
drop1(model2nm)

## no significant improvement to remove any terms