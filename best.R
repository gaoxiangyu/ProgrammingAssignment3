best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## Check that state and outcome are valid
	if (!(state %in% data[,7]))
		stop("invalid state")
	if (outcome!="heart attack" && outcome!="heart failure" && outcome!="pneumonia")
		stop("invalid outcome")
	## Return hospital name in that state with lowest 30-day death rate
	if(outcome=="heart attack"){
		deathratecol <- 11
	}
	if(outcome=="heart failure"){
		deathratecol <- 17
	}
	if(outcome=="pneumonia"){
		deathratecol <- 23
	}
	hospitalsIndex <- which(state==data[,7])
	deathrate <- as.numeric(data[hospitalsIndex,deathratecol])
	minIndex <- hospitalsIndex[which(min(deathrate,na.rm=TRUE) == deathrate)]
	min(data[minIndex,2])
}