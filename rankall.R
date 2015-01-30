rankall <- function(outcome, num = "best"){
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## Check that state and outcome are valid
	if (outcome == "heart attack"){
		deathratecol <- 11
	} else if (outcome == "heart failure"){
		deathratecol <- 17
	} else if (outcome == "pneumonia"){
		deathratecol <- 23
	} else {
		stop("invalid outcome")
	}
	## For each state, find the hospital of the given rank
	states <- unique(data[,7])
	states <- states[order(states)]
	statename <- vector("character", length(states))
	hospitalname <- vector("character", length(states))
	i <- 0
	for (state in states) {
		i <- i+1
		statename[i] <- as.character(state)
		hospitals <- data[state==data[,7],c(2,deathratecol)]
		hospitals[,2] <- as.numeric(hospitals[,2])
		hospitals <- hospitals[!is.na(hospitals[,2]),]
		## hospitals[,1] <- as.character(hospitals[,1])
		hospitals <- hospitals[order(hospitals[2],hospitals[1]),]
		if (num == "best")
			hospitalname[i] <- hospitals[1,1]
		else if (num == "worst")
			hospitalname[i] <- hospitals[nrow(hospitals),1]
		else if (num > nrow(hospitals))
			hospitalname[i] <- NA
		else
			hospitalname[i] <- hospitals[num,1]
	}
	## Return a data frame with the hospital names and the (abbreviated) state name
	data.frame(hospital=hospitalname,state=statename,row.names=statename)
}