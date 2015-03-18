## Bethany Levick, University of Liverpool, 2013 ##
## Generating landscape predictors for flea models ##
## open working drive
setwd( DIRECTORY HERE )
#open data
data <- read.csv( "kaz_14-04-15.csv", header=T )
## source functions for landscape predictions
source("myclasses_functions.r")
#################################################################################################################################
## Need to clean the data a bit ##
## need to replace all the na's wit NA
#data$findex[data$findex=="na"] <- NA
### Do this for the whole data set
data[data=="na"] <- NA
data$tree[data$tree=="no data"] <- NA
## and ensure coercion to numeric format
data$findex <- as.numeric(data$findex)
data$tindex <- as.numeric(data$tindex)
data$sector <- as.factor(data$sector)
################################################################################################################################
fleas <- landscapeRepresentQuant( data, 31, 22, median )
fleas$logmed <- log10(fleas$med.norm)

## remove NA data
fleas <- na.omit(fleas)
#plot
par( pty="s" )
plot( log10(fleas$med.norm), xlab="landscape", ylab="log median flea index", 
main="flea index across landscape descriptions", xaxt="n", pch=16 )
axis( side=1, at=c(1:length(fleas$ltype)), labels=fleas$ltype, tick=T )
box()
grid()

##########################################################################################################

## setting three categories as 
# bottom to -0.5, then to 0.5 then to 2.0
## add column with logged data
classes_f <- list()	#empty list
classes_f[[1]] <- fleas[1:47,]
classes_f[[2]] <- fleas[48:85,]
classes_f[[3]] <- fleas[86:131,]

#generate descriptor matrix
desc_f <- descriptorMatrix(classes_f)

# convert this to percentages DOWN classes
for (k in 1:ncol(desc_f)){
total <- sum(desc_f[,k])
desc_f[,k] <- (desc_f[,k]/total)*100	#convert these values to percentages of all in class, for comparisons
}
# plot descriptor matrix
par( pty="s" )
plotDescriptorMatrix( desc_f )

# plot
## set layout of plot area
layout(matrix(c(1,1,2), 1, 3, byrow = TRUE))
## plot matrix
plotDescriptorMatrix(desc_oo.c)
## plot legend
## add a legend plot
plot( c(rep(0.5,100)), seq(1,100,1), pty="p", pch=15, xlim=c(0,1), ylim=c(0,100), xaxt="n", 
	ylab="percentage of descriptions in class containing characteristic", main="legend", bty="n", col=cols, xlab="" )

#############################################################################################################
#generate descriptior matrix 
desc_f <- descriptorMatrix(classes_f)
# convert to percentages ACROSS rows
for (k in 1:nrow(desc_f)){
total <- sum(desc_f[k,])
desc_f[k,] <- (desc_f[k,]/total)*100	#convert these values to percentages of all in class, for comparisons
}
# plot descriptor matrix
par( pty="s" )
plotDescriptorMatrix( desc_f )