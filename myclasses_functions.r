## Function to generate ordered data frame of percentage of occupancy status
# represented by burrows of each unique landscape type description
# takes the number of the columns containing descriptions, the data frame, the number code for occupancy, and the column 
# containing occupancy
landscapeRepresent <- function(descriptcol, dataframe, ol, ocol ){

x <- unique(dataframe[,descriptcol])

ltype <- data.frame( ltype=x, occ=c(rep(0, (length(x)))) )

for (k in 1:length(x)){
l <- x[k]
tmp <- subset(dataframe, dataframe[,descriptcol]==l)	#burrows of this landscape type

occ <- which(tmp[,ocol]==ol)	#occupied burrows of this landscape type

ltype$occ[k] <- length(occ)/nrow(tmp)	#what fraction this landscape are of all occupied burrows

tmp.o <- subset(dataframe, dataframe[,ocol]==ol)	#all burrows that are occupied

propall <- length(occ)/nrow(tmp.o)	#proportion of ALL occupied burrows that occupied burrows of this landscape type make up
ltype$p_ofocc[k] <- propall
 
ltype$norm[k] <- propall/nrow(tmp)	#normalise this by dividing by the number of burrows of this landscape type
}

ltype.o <- ltype[ order(ltype[,4]), ]

return( ltype.o )

}
## Example usage for occupied burrows
# test <- landscapeRepresent( 28, data, 4, 10 )
# occ is 10 bschar is 28

###################################################################################
## Function to generate ordered data frame of percentage of a quantitative output variable
# represented by burrows of each unique landscape type description
# takes the number of the columns containing descriptions, the data frame, and the column containing the quantitative output
landscapeRepresentQuant <- function(dataframe, descriptcol, quantcol, func ){

x <- unique(dataframe[,descriptcol])	#find each unique landscape description
x <- as.character(x)	#convert to character to allow the regex to run

#set up new data frame
ltype <- data.frame( ltype=x, med.resp=c(rep(0, (length(x)))),
	med.norm=c(rep(0, (length(x)))) )	

for (k in 1:length(x)){	#for each unique landscape description
l <- x[k]	#take description

tmp <- subset(dataframe, dataframe[,descriptcol]==l)	#data rows where this landscape description found

non.na <- subset(tmp, tmp[,quantcol]>-1) #find all results that are non na, allowing for 0's

	if( nrow(non.na)>0){	#for all rows where non na		
	ltype$med.resp[k] <- func(tmp[,quantcol], na.rm=TRUE)	#find median response for burrows of this description
	m <- func(tmp[,quantcol], na.rm=TRUE)	#set this as a variable
	ltype$med.norm[k] <- m/nrow(non.na)	#normalise this by dividing by the number of burrows of this landscape type
	}
	
}
ltype.o <- ltype[ order(ltype$med.norm), ]

return( ltype.o )
}

# test <- landscapeRepresentQuant(trans, 7, 9, mean)

###################################################################################
## Function to generate descriptor matrix from list object containing data frame
## subsetted into different occupancy level classes
descriptorMatrix <- function(classlist){

descriptors <- c("A", "B", "C", "D", "E", "F", "K", "BLANK", "1", "2", "3", "4", "5", "6", "7", "8")	#descriptors
desc_mat <- matrix( ncol=(length(classlist)), nrow=length(descriptors))	#generate matrix to store numbers in each class/descriptor category
rownames(desc_mat) <- descriptors	#set as rownames

class_names <- vector()
	for (k in 1:length(classlist)){
	class_names[k] <- paste("class", k, sep="")	#generate class names by pasting in a loop
	}
colnames(desc_mat) <- class_names	#set this as column names for matrix


	for (i in 1:length( classlist )){
	tmp <- classlist[[i]]	#take each subset dataframe in turn from the list object
		for (k in 1:length( descriptors ) ){	#for each descriptor in turn
		d <- descriptors[k]	#take this descriptor
		tmp1 <- grep( d, tmp$ltype, perl=TRUE, value=FALSE )	#find it as a regex in the column of the data frame
		desc_mat[k,i] <- sum(tmp1)	#each column of mat is a class (i) and each row is a descriptor (j) - give total for this comb.
		}
	}
	
return(desc_mat)

}
#dmat_uo <- descriptorMatrix(classes_uo)

## plot descriptor matrix
# takes desc, a descriptor matrix generated above and a conditional of whether to plot the legend
plotDescriptorMatrix <- function(desc){

#cols <- rainbow( 100, start=0, end=4/6 )	#generate rainbow that starts at red and ends at blue
## using greyscale instead as easier to interpret - 29/01/14
cols <- gray.colors(100, start = 0.7, end = 0.1, gamma = 2.2, alpha = NULL)
# reverse the descriptor labels for plotting onto the image() plot, to do with how image() treats matrix
descriptor_labels <- c("8", "7", "6", "5", "4", "3", "2", "1", "BLANK", "K", "F", "E", "D", "C", "B", "A")
# generate class names as colname labels
class_names <- vector()
	for (k in 1:ncol(desc)){
	class_names[k] <- paste("class", k, sep=" ")	#generate class names by pasting in a loop
	}

# plot
image(t(desc), col=cols, main="Heatmap of descriptors", axes=F )	#plot image of matrix
grid(nx=(ncol(desc)), ny=16, col="black", lty=1)	#add a grid to see cells easier
mtext(text=rev(descriptor_labels), side=2, line=0.3, at=seq(0,1,0.065), las=2, cex=0.8)	#add labels on y
axis( side=1, at=seq(0,1,length.out=ncol(desc)), labels=class_names)	# add lables on x
box()	

## 15-02-24
## need to add a legend


}

# Example usage
# plotDeescriptorMatrix( desc_oo )

## plot occupancy matrix
## to plot matrix giving occupancy in each sector across each season
plotOccupancyMatrix <- function(desc, title){

#cols <- rainbow( 100, start=0, end=4/6 )	#generate rainbow that starts at red and ends at blue
## using greyscale instead as easier to interpret - 29/01/14
cols <- gray.colors(100, start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL)
# reverse the descriptor labels for plotting onto the image() plot, to do with how image() treats matrix
descriptor_labels <-c(11742, 10544, 10531, 10512, 9123, 7934)
class_names <- c("AU2011", "SU2012", "AU2012", "SP2013")


# plot
image(t(desc), col=cols, main=title, axes=F )	#plot image of matrix
grid(nx=(ncol(desc)), ny=6, col="black", lty=1)	#add a grid to see cells easier
mtext(text=descriptor_labels, side=2, line=0.3, at=seq(0,1,0.2), las=2, cex=0.8)	#add labels on y
axis( side=1, at=seq(0,1,length.out=ncol(desc)), labels=class_names)	# add lables on x
box()	

}












