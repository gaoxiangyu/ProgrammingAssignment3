rankhospital <- function(state, outcome, num = "best"){
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## Check that state and outcome are valid
	if (!(state %in% data[,7]))
		stop("invalid state")
	if(outcome=="heart attack"){
		deathratecol <- 11
	}else	if(outcome=="heart failure"){
		deathratecol <- 17
	}else if(outcome=="pneumonia"){
		deathratecol <- 23
	}else{
		stop("invalid outcome")
	}
	## Return hospital name in that state with the given rank 30-day death rate
	hospitals <- data[state==data[,7],c(2,deathratecol)]
	hospitals[,2] <- as.numeric(hospitals[,2])
	hospitals <- hospitals[!is.na(hospitals[,2]),]
	hospitals <- hospitals[order(hospitals[2],hospitals[1]),]
	if(num == "best")
		hospitals[1,1]
	else if(num == "worst")
		hospitals[nrow(hospitals),1]
	else if(num > nrow(hospitals))
		NA
	else
		hospitals[num,1]
}