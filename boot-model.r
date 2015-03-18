## Code to run a model using only 1 instance of a pseudoreplicated factor ##
## finds all examples of a replicant, and where it occurs more than once, only allows 1 into the sample ##
## i.e. takes each burrow in turn, if burrow appears more than once, only take 1 at random ##
## generates samples of data through this strategy ##
## runs the model as specified in runModel
## parameters are set in individual file

## bootModel 
## Function to boot model by particular factor
bootModel <- function( nruns, in.form, dataset, bootby ){

results.list <- list()	#empty list to push results to 

bc <- which( colnames(dataset)==bootby )	#find the column number for the bootby

bootfac <- unique(dataset[,bc])	#unique values for factor to bootby from data set

for(i in 1:nruns){	#repeat for number of runs specified

	set <- data.frame( dataset )	#make template data frame to store sample from 1st row of input data frame
	set <- set[1,]					#just keep the first row

	for(k in 1:length( bootfac )){	#for each unique value of factor to boot by
		tmp <- dataset[dataset[,bc]==bootfac[k],]	#subset to where this burrow is true
		x <- sample( (1:(nrow(tmp))), 1, replace=FALSE )	#from 1:number of records, select 1 number
															#so if only 1 record exists, 1 record will always be picked
															#if multiple records exist, one is taken at random each time
		set[k,] <- tmp[x,]								#take this number row from the tmp subset and push to data frame
		}
		
	set <- set[2:nrow(set),]	#remove the first template line to get a proper sample
	
	## run the model specified in the function given in the main file
	model <- runModel()

	# store results
	summ <- summary(model)
	results <- summ$tTable
	
	#print( results )		#unhash for testing

	# extract and store bits we need of results
	factors <- (rownames(results))[2:(length(rownames(results)))]
	
	results.table <- data.frame( factor=factors, 
		coeff=c(rep(0,(length(factors)))), p=c(rep(0,(length(factors)))) )	#construct a blank data frame to push results to

	results.table$coeff <- results[2:(nrow(results)),1]	#add in coefficients, miss int.

	results.table$p <- results[2:(nrow(results)),5]	#add in P, miss int.

	results.list[[i]] <- results.table
	
}

return( results.list )
	
}

## generates a list object result
## each object in the list is a data frame giving each factor, its associated coefficients and p value
## this is given for every run 1:nruns
boots <- bootModel( nruns, in.form, dataset, bootby )