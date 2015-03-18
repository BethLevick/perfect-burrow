## Bethany Levick, University of Liverpool 2013 ##
## Code to set up environment to run binary model of occupancy ##

## delete duplicate row in main data frame
dat <- dat[-2349,]
###################################################################################################################################
## set up new data frame with predictive factors from main data frame
factor_df <- data.frame( landscape=dat$Bscharact_all )

## First set up environment so occupancy is a binary response ##
## classify occupancy data as 1 and 0 of occupied or not
factor_df$occupied <- dat$occ						## copy across occupancy data
## This step is necessary because 0 and 1 are used in the data, so become overwritten
factor_df$occupied[factor_df$occupied!=4] <- 5		
factor_df$occupied[factor_df$occupied==4] <- 6		## Accurately labels as on/off and then rewrites with conventional 1 and 0 below
factor_df$occupied[factor_df$occupied==5] <- 0		## then replace the 5's and 6's with 1's and 0's
factor_df$occupied[factor_df$occupied==6] <- 1
factor_df$season <- as.factor(dat$season)	#just added on from main data frame as no data dropped so same length
factor_df$sector <- as.factor(dat$sector)	# re classify sector as character to ensure categorical
factor_df$lat <- dat$lat

factor_df$tree <- as.factor(dat$tree)			#presence or absence of tree
factor_df$tree[factor_df$tree=="no data"] <- NA	#replace "no data" with NA

# add burrows & sectors for random effect
factor_df$burrow <- as.factor(dat$NAMEBS)
factor_df$sector <- as.factor(dat$sector)

#function to perform regex search on a character string of the regex reg
#where l is the string to search and reg is the exact regex e.g. "1" or "[56]"
#will return a 1 if present and a 0 if absent in the character string
#neg is the value to print if the regex is not found in the string
#pos is the value to print if the regex is found in the string
classByRegex <- function(cs, reg, neg, pos){
val <- neg										#initally set the value to 0
x <- grep( reg, cs, value=F, perl=T )			#perform regex search
if(length(x)>0){								#if regex found, then length of result will be >0
val <- pos										#in which case specify value to be 1
}
return(val)										#in either case, return the value
}

## example usage, for sand (class =1)
##factor_df$sand <- sapply( factor_df$landscape, FUN=classByRegex, reg="1", neg=0, pos=1 )

## in place of factor 1 (ordinal sand) and factor 4 (alluvial plain)
## separate descriptors for sand, loam and alluvial plain
factor_df$factor2 <- sapply( factor_df$landscape, FUN=classByRegex, reg="[56]", neg=0, pos=1 )
factor_df$binfactor3 <- sapply( factor_df$landscape, FUN=classByRegex, reg="[DEF87]", neg=0, pos=1 )
factor_df$sand <- sapply( factor_df$landscape, FUN=classByRegex, reg="1", neg=0, pos=1 )
factor_df$sandloam <- sapply( factor_df$landscape, FUN=classByRegex, reg="2", neg=0, pos=1 )
factor_df$clayloam <- sapply( factor_df$landscape, FUN=classByRegex, reg="3", neg=0, pos=1 )
factor_df$allpl <- sapply( factor_df$landscape, FUN=classByRegex, reg="4", neg=0, pos=1 )
factor_df$loam <- sapply( factor_df$landscape, FUN=classByRegex, reg="[23]", neg=0, pos=1 )

# recode season
factor_df$binseason <- sapply( factor_df$season, FUN=classByRegex, reg="SP2013", neg="not spring", pos="spring" )

# convert latitude to region, with 45.5 degrees north being the cut off for the north and south regions
factor_df$reg <- factor_df$lat
factor_df$reg[factor_df$reg>45.4] <- "N"
factor_df$reg[factor_df$reg!="N"] <- "S"

## ensure set as factor
factor_df$sand <- as.factor(factor_df$sand)
factor_df$sandloam <- as.factor(factor_df$sandloam)
factor_df$clayloam <- as.factor(factor_df$clayloam)
factor_df$allpl <- as.factor(factor_df$allpl)
factor_df$loam <- as.factor(factor_df$loam)
factor_df$factor2 <- as.factor(factor_df$factor2)
factor_df$binfactor3 <- as.factor(factor_df$binfactor3)
factor_df$binseason <- as.factor(factor_df$binseason)
factor_df$reg <- as.factor(factor_df$reg)

