## 	CountPVA Simple function

# 	Extract variable:
# Each lambda for the duration of the study as "mu"	
# r squared	as "r2"
# mu with lower and upper 95% CI as "mu.s" with three columns of "mu.s$fit", "mu.s$lwr", and "mu.s$upr" 
# the set of year intervals of the start year of study and each consecutive addition of years as "years"
#		"years$RangeStart" and "years$RangeEnd"
# Lambda and the lower and upper 95% CIs as "growth.exp" with "growth.exp$fit", "growth.exp$lwr", and "growth.exp$upr"  

#	To use the function, (years can be missed, the counts are for the matching years in the x year column) 
#	Years The column name with the years of the study census
#	Count The column name with the annual count 

CountPVA <- function(df,Years,Count){

Yrs <- df[,Years]
Cnt <- df[,Count]
				
PVA.table <- lapply(1:(length(Cnt)-1), function(row){
                x <- sqrt(Yrs[row+1] - Yrs[row])
                y <- log10(Cnt[row+1]/Cnt[row])/x
                cbind(x,y,year = Yrs[row])
                })
PVA.table <- do.call(rbind,PVA.table)

# LM for each consecutive additional year 3 years on...
pva.lm <- lapply(3:nrow(PVA.table), function(x){
  lm(y ~ -1 + x, data = data.frame(PVA.table[1:x,]))
})

lapply(pva.lm, function(lm){
  summary(lm)
})

mu_sp.l <- lapply(pva.lm, function(lm){
  conf <- confint(lm, "x", level=0.95)
  cbind(summary(lm)$coefficients[1], conf, summary(lm)$adj.r.squared)
})
mu_sp <- data.frame(matrix(unlist(mu_sp.l), nrow=nrow(df)-3, byrow=TRUE))
names(mu_sp) <- c("mu","l95","u95","r2")
mu_sp$StartYear <- Yrs[1]
mu_sp$EndYear <- Yrs[4:nrow(df)]	
mu_sp
}




