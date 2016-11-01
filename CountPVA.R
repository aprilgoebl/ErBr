## 	CountPVA Simple function

# 	Extract variable:
# Each lambda for the duration of the study as "Sitemu"	
# r squared	as "R.sq"
# mu with lower and upper 95% CI as "mu.s" with three columns of "mu.s$fit", "mu.s$lwr", and "mu.s$upr" 
# the set of year intervals of the start year of study and each consecutive addition of years as "years"
#		"years$RangeStart" and "years$RangeEnd"
# Lambda and the lower and upper 95% CIs as "growth.exp" with "growth.exp$fit", "growth.exp$lwr", and "growth.exp$upr"  

#	To use the function, enter "CountPVA(x,y)
#	x The column with the years of the study census
#	y The column with the annual count (years can be missed, the counts are for the matching years in the x year column) 

CountPVAsimple <- function(Years,Count){

	Yrs <- length(unique(Years))
	rsq <- c()
	xvar <- c()
	ySpecies <- c()
	PVA.lm <- list()	
	mu_sp <- c()
	year.int <- c()

		count1 <- Years
		yrs <- length(count1)
		
		lapply(1:length(Count), function(row){
		  x <- sqrt(Years[row+1] - Years[row])
		  y <- log10(Count[row+1]/Count[row])/x
		  
		})
		
		
			for(i in 1:(yrs-1)){
				yr <- sqrt((Years[i+1]) - (Years[i]))
				xvar <- rbind(xvar, yr)
				y1 <- ( log10(Count[i+1]/Count[i]) )/yr
				ySpecies <- rbind(ySpecies, y1)
				PVA.table <<- data.frame(cbind(xvar = as.numeric(xvar),
					ySpecies = as.numeric(ySpecies)))
				year.int1 <- data.frame(cbind(RangeStart = min(Years), 
					RangeEnd = Years[Yrs]))
				PVA.lm <- lm(PVA.table[,2] ~ -1 + PVA.table[,1])

				mu_sp1 <- predict(PVA.lm, level= 0.95, 
					interval = "confidence",
					se.fit = T)
			}
	rsq <- data.frame(rbind(rsq, summary(PVA.lm)$adj.r.squared))	# pull R squared values from
												# each year interval 1995-1999...
	Sitemu <<- ySpecies
		rm(ySpecies); rm(xvar); rm(PVA.table)
		ySpecies <- c(); xvar <- c(); PVA.table <- list()
	
		mu_sp <- data.frame(rbind(mu_sp, mu_sp1$fit[1,]))
		year.int <- data.frame(rbind(year.int, year.int1))
	

	R.sq <<- rsq
	colnames(R.sq) <<- "R.squared"
	mu.s <<- mu_sp
	colnames(mu.s) <<- c("fit", "lwr", "upr")
	growth <<- mu_sp
	years <<- year.int
	growth.exp <<- exp(growth)
	lambda <<- data.frame(cbind((min(years[,2])-3):(max(years[,2])-1),Sitemu))
	
}




