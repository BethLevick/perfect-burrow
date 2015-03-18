Read me file for Dryad respository http://dx.doi.org/10.5061/dryad.4472d 
Levick et al 2014 "The perfect burrow, but for what?..."
Bethany Levick, University of Liverpool 2014
b.a.levick@liverpool.ac.uk
--------------------------------------------------------------------------------------------------------------------------------------------
----UPDATE ON PREVIOUS SUBMISSION----
When these files and the associated manuscript were initially submitted, the flea model was run using a correction of the data to estimate
Xenopsylla index, rather than flea index. On review we feel that this is less appropriate and so these files, and the associated results
are generated using the flea index as recorded in the original data set. The data files remain as the initial submission.
--------------------------------------------------------------------------------------------------------------------------------------------
Overview of files
------------------
Input data files:
Filename		File type	Description
kaz_14-04-15	.csv		Field records of burrow properties, occupancy, flea and tick numbers
							Used for occupancy, flea index and Xenopsylla index models
transitions		.csv		Field records of repeat visits to burrows to record occupancy
							Used for long term occupancy model

In each instance, the data files are rearranged from the original to make a usable format in the "-setup" .R files.
These comprise of a response variable column (the different ones given below), and then a similar set of predictor variable columns 

Response variable column (1 in each formatted data set):
Column name					Data format			Data description
Occupied					Factor				Binary record of whether a burrow was recorded as occupied or not
Flea index					Numeric				Flea burden (as number of fleas in burrow/number of animals in burrow)
Sea_Occ						Numeric				Proportion of all recorded seasons (6) where the burrow is occupied

Predictor variable columns (multiple in each formatted data set):
Column name					Data format			Data description
Landscape					Factor				Field record of landscape using descriptors indicated in Table 1 of the manuscript
Landscape type (e.g. sand)	Factor				Binary record of whether this landscape type is present in the field record
Tindex						Factor				Binary record of whether a tree was found on the burrow
Findex						Numeric				Tick burden (as number of ticks in burrow/number of animals in burrow)
Lat							Numeric				Latitude position in degrees
Season						Factor				Trapping season where record was made
Burrow						Factor				Individual burrow ID
Sector						Factor				Sector ID

The long term data set also has one additional unique data column
Column name					Data format			Data description
wgt							numeric				Weighting of the data row (i.e. how many seasons this burrow is recorded in)

The results of the iterative ("booted") models are stored as R working environments, with the file name format "model-boot.RDa".
--------------------------------------------------------------------------------------------------------------------------------------------

Code files
------------------
All the code files are written in the R statistical computing language (R Core Team, 2014). 
For each model there is a baseline of 2 files, using occupancy as an example:
File name				Purpose			
occupancy-glmm.r		"Main" file for running the models. Contains reduced copies of model output
occupancy-setup.r		Converts the raw data into the factor data frame used in the -glmm file to run the model

For all except the long term occupancy file, there is a -boot file, used to generate and then analyse the results of running
the model through iterative burrow samples
File name				Purpose	
occupancy-boot.r 		To run the "booted" (iterative) model to test the robustness of the random effects structure

Then in the case of occupancy and fleas, there is an additional file
File name				Purpose
occupancy-predictors.r	Generating the landscape factors for inclusion in the modelm, does not need to be run to run the model

myclasses_functions.r provides background functions used across the predictor generation files.