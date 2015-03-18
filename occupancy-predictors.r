## Bethany Levick, University of Liverpool, 2013 ##
## Generating landscape predictors for occupancy models ##
## open working drive
setwd( DIRECTORY HERE )
#open data
data <- read.csv( "kaz_14-04-15.csv", header=T )
## source functions for landscape predictions
source("myclasses_functions.r")
################################################################################################################################
# generate initial data frame, ordered by percentage meeting occupancy level - OCCUPIED
occupied_propocc <- landscapeRepresent( 31, data, 4, 13 )

# plots to identify the classes
par( pty="s" )
plot( log10(occupied_propocc[,4]), xlab="landscape", ylab="log proportion of occupied burrows", 
main="proportion of all occupied burrows made up by occupied burrows of this landscape", xaxt="n", pch=16 )
axis( side=1, at=c(1:length(occupied_propocc$ltype)), labels=occupied_propocc$ltype, tick=T )
box()
grid()

################################################################################################################################

# identify classes and generate list object
classes_oo.c <- list()	#empty list
classes_oo.c[[1]] <- occupied_propocc[1:98,] 
classes_oo.c[[2]] <- occupied_propocc[99:128,]
classes_oo.c[[3]] <- occupied_propocc[129:172,]

# then remake the matrix
desc_oo.c <- descriptorMatrix(classes_oo.c)

## For percentages DOWN classes
# convert this to percentages
for (k in 1:ncol(desc_oo.c)){
total <- sum(desc_oo.c[,k])
desc_oo.c[,k] <- (desc_oo.c[,k]/total)*100	#convert these values to percentages of all in class, for comparisons
}

# plot
## set layout of plot area
layout(matrix(c(1,1,2), 1, 3, byrow = TRUE))
## plot matrix
plotDescriptorMatrix(desc_oo.c)
## plot legend
## add a legend plot
plot( c(rep(0.5,100)), seq(1,100,1), pty="p", pch=15, xlim=c(0,1), ylim=c(0,100), xaxt="n", ylab="descriptions
	w/ individual characteristic (%)", main="legend", bty="n", col=cols, xlab="" )

################################################################################################################################

## For percentages ACROSS descriptor codes

#remake the matrix
desc_oo.c <- descriptorMatrix(classes_oo.c)

# convert to percentages 
for (k in 1:nrow(desc_oo.c)){
total <- sum(desc_oo.c[k,])
desc_oo.c[k,] <- (desc_oo.c[k,]/total)*100	#convert these values to percentages of all in class, for comparisons
}
 
# plot
par( pty="s" )
plotDescriptorMatrix(desc_oo.c)