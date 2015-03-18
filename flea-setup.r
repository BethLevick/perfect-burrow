## Bethany Levick, University of Liverpool 2013 ##
## Code to set up environment for running flea gamms ##
#################################################################################################################################
## Need to clean the dat a bit ##
## need to replace all the na's wit NA
#dat$findex[dat$findex=="na"] <- NA
### Do this for the whole dat set
dat[dat=="na"] <- NA
dat$tree[dat$tree=="no data"] <- NA
## and ensure coercion to numeric format
dat$findex <- as.numeric(dat$findex)
dat$tindex <- as.numeric(dat$tindex)
#################################################################################################################################
# set up predictive factor dat frame
## first need to get all the dat we need to copy across from the main frame
factor_df <- data.frame( landscape=dat$Bscharact_all )
factor_df$findex <- dat$findex
factor_df$tindex <- dat$tindex
factor_df$lat <- dat$lat
factor_df$season <- as.factor(dat$season)
factor_df$tree <- as.factor(dat$tree)

# add burrow for random effect
factor_df$burrow <- as.factor(dat$NAMEBS)
factor_df$sector <- as.factor(dat$sector)

## copy across occupancy dat
factor_df$occ <- dat$occ						
factor_df$vis <- dat$occ

# then need to remove NA's from dat
factor_df <- na.omit( factor_df )

##################################################################################################################
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


## then generate new dat dependent on what remains in the dat set
## for landscape predictors
factor_df$factor2 <- sapply( factor_df$landscape, FUN=classByRegex, reg="[56]", neg=0, pos=1 )
factor_df$sand <- sapply( factor_df$landscape, FUN=classByRegex, reg="1", neg=0, pos=1 )
factor_df$sandloam <- sapply( factor_df$landscape, FUN=classByRegex, reg="2", neg=0, pos=1 )
factor_df$clayloam <- sapply( factor_df$landscape, FUN=classByRegex, reg="3", neg=0, pos=1 )
factor_df$allpl <- sapply( factor_df$landscape, FUN=classByRegex, reg="4", neg=0, pos=1 )
factor_df$loam <- sapply( factor_df$landscape, FUN=classByRegex, reg="[23]", neg=0, pos=1 )
factor_df$factorB <- sapply( factor_df$landscape, FUN=classByRegex, reg="[DEF78]", neg=0, pos=1 )

factor_df$sand <- as.factor(factor_df$sand)
factor_df$sandloam <- as.factor(factor_df$sandloam)
factor_df$clayloam <- as.factor(factor_df$clayloam)
factor_df$allpl <- as.factor(factor_df$allpl)
factor_df$loam <- as.factor(factor_df$loam)
factor_df$factorB <- as.factor(factor_df$factorB)

##############################################################################################
# convert latitude to region, with 45.5 degrees north being the cut off for the north and south regions
factor_df$reg <- factor_df$lat
factor_df$reg[factor_df$reg>45.4] <- "N"
factor_df$reg[factor_df$reg!="N"] <- "S"
factor_df$reg <- as.factor(factor_df$reg)
##############################################################################################
## classify occupancy dat as 1 and 0 of occupied or not
## This step is necessary because 0 and 1 are used in the dat, so become overwritten
factor_df$occ[factor_df$occ!=4] <- 5		
factor_df$occ[factor_df$occ==4] <- 6		## Accurately labels as on/off and then rewrites with conventional 1 and 0 below
factor_df$occ[factor_df$occ==5] <- 0		## then replace the 5's and 6's with 1's and 0's
factor_df$occ[factor_df$occ==6] <- 1

## classify occupancy dat as 1 and 0 of visited or not
## This step is necessary because 0 and 1 are used in the dat, so become overwritten
factor_df$vis[factor_df$vis!=3] <- 5		
factor_df$vis[factor_df$vis==3] <- 6		## Accurately labels as on/off and then rewrites with 1 and 0 below
factor_df$vis[factor_df$vis==5] <- 0		## then replace the 5's and 6's with 1's and 0's
factor_df$vis[factor_df$vis==6] <- 1
##############################################################################################
## then add columns of occupancy and visited proportions across the seasons/sectors
factor_df$pocc <- c(rep(0,(nrow(factor_df))))
factor_df$pvis_nocc <- c(rep(0,(nrow(factor_df))))
factor_df$pocc_season <- c(rep(0,(nrow(factor_df))))
factor_df$pvis_season <- c(rep(0,(nrow(factor_df))))
sector <- unique(dat$sector)
season <- unique(dat$season)

for (k in 1:nrow(factor_df)){

tmp <- dat[dat$sector==factor_df$sector[k],]	#subset dat to where sector at position k true

####################################### Overall occupied in sector
tmp.o <- tmp[tmp$occ==4,]	#find number of occupied burrows in that sector 

factor_df$pocc[k] <- nrow(tmp.o)/nrow(tmp)	#proportion of all burrows in sector that are occupied, across all seasons
####################################### Overall visited of non occupied in sector
tmp.v <- tmp[tmp$occ==3,]	#all visited burrows
noc <- nrow(tmp)-nrow(tmp.v)-nrow(tmp.o)	#find how many burrows are neither occupied or visited

factor_df$pvis_nocc[k] <- nrow(tmp.v)/noc	#proportion of all non occupied burrows that are visited
####################################### Occupied in specific season/sector

tmp.s <- tmp[tmp$season==factor_df$season[k],]	#subset the original set where sector true to where season at position k true

tmp.so <- tmp.s[tmp.s$occ==4,]	#find burrows in this season and sector that are occupied

factor_df$pocc_season[k] <- nrow(tmp.so)/nrow(tmp.s)	#find the proportion of burrows in this season/sector that are occupied
####################################### Visited of non occupied in specific season/sector

tmp.sv <- tmp.s[tmp.s$occ==3,]	#burrows in this season and sector that are visited
	
noc <- nrow(tmp)-nrow(tmp.sv)-nrow(tmp.so)	#find how many burrows are neither occupied or visited

factor_df$pvis_season[k] <- nrow(tmp.sv)/noc	#proportion of all non occupied burrows that are visited
}

#################################################################################################################################