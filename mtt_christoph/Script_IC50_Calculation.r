library(drc)

#plots the model
drawplot <- function(my.df, my.model, my.valid.points, my.problem, ...) {

#if we have a dose response model
	if (!is.null(my.model) && my.model$fit$convergence==TRUE) {
	#create the initial plot using the drm functionality, but only for the valid points
		plot (my.model, type = c("all"), subset = my.valid.points, ...)
		
		#derive the 95% confidence interval from the least squares model
		confidence.interval <- confint(my.model, parm= c("e"), level = 0.95)

		#get the activity at IC50 for drawing the confidence interval
		ic50.value = my.model$coefficients[4]
		ic50.y <- predict(my.model, as.data.frame(ic50.value))
		
		#isolate outlier points
		outliers <- my.df[!my.valid.points,]
			
		#add outlier points to the plot
		points(outliers$concentration,outliers$activity, col="magenta")
			
		#draw the confidence interval into the plot
		arrows(confidence.interval[1],ic50.y,confidence.interval[2],ic50.y, code = 3, angle=90, lwd=2)
	}
	
	
	#if we do not have a model at all, we just plot the points using a standard scatter plot
	else {
		plot (my.df$activity ~ my.df$concentration, log = "x", ...)	
	}
#now we add an informative text to either of the plots

	text(median(my.df$concentration), mean(my.df$activity), paste (" ", my.problem), col="red")
}



#calculates the robust 68th percentile of the residuals
#adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
P68 <- function (residuals) {
  
  #take the absolute value of the residuals
  res <- abs(residuals)
  
  #sort ascending
  res.sorted <- sort(res)
  
  #calculate the corresponding percentiles
  res.percentiles <- (seq(1:length(res.sorted))/length(res.sorted))*100
  
  #find index of first percentile larger than 68.25
  index <- min(which(res.percentiles > 68.25))
  
  #isolate points around 68.25 percentile for linear intrapolation
  x <- c(res.percentiles[index-1],res.percentiles[index])
  y <- c(res.sorted[index-1],res.sorted[index])
  
  #create linear model
  m <- lm(y~x)
  
  #predict value of 68.25 percentile
  x <- c(68.25)
  y <- predict(m,as.data.frame(x))
  
  return(y)
}

#calculates the robust standard deviation of the residuals (RSDR) with correction for degrees of freedom
#adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
RSDR <- function(residuals, number.of.coefficients.fitted) {
  
  my.residuals <- as.numeric(residuals)
  my.residuals <- na.omit(residuals)
  
  N <- length(my.residuals)  #the number of data points fitted
  K <- number.of.coefficients.fitted #for ic50, 4 coefficients are fitted
  
  result <- P68(residuals) * N/(N-K)
  
  return (result)
}


#false discovery rate (FDR) approach, returns a T/F vector for selection of valid data points
#adapted from Motulsky HJ, Brown RE, BMC Bioinformatics 2006, 7:123
FDR <- function(res) {
  
  N <- length(res) #the number of data points fitted
  Q <- 0.01 #Q=1%
  K <- 4    #number of coefficients in the fitted LL.4 model
  R <- RSDR(res,K) #the robust standard deviation of the residuals
  
  #the id column facilitates identification of samples
  id <- seq(1:length(res))
  
  #creation of the startin data table containg id and residuals
  df <- data.frame(id,res)
  
  #calculate absolute residual, attach to df
  df$res.abs <- abs(df$res)
  
  #sort data frame ascending to absolute residuals
  df <- df[order(df$res.abs),] 
  
  #enumarate the residuals in the sorted table, attach to df
  df$i <- seq(1:N)
  
  #enumarate the residuals in the sorted table, attach to df
  df$i.fraction <- df$i / N
  
  #alpha, attach to df
  df$alpha <- Q*(N-(df$i-1))/N
  
  #t value, attach to df
  df$t <- df$res.abs / R
  
  #probability/density from the t-distribution, attach result to df
  df$P <- dt(df$t,N-K)
  
  #test whether derived probability is smialler than alpha, attach result to df
  df$include <- ifelse(df$P < df$alpha & df$i.fraction >= 0.7,F,T)
  
  #sort the data frame to obtain the original order of the residuals
  df2 <- df[order(df$id),]
  
  return (df2$include)
}

#function to round number and turn infinite, Na, NaN values to NULL for easier insertion into database tables
shapenumber <- function (my.number) {
  if (is.finite(my.number)) {
    my.result <- signif(my.number,3)    
  } else {  
    my.result <- NA
  } 
  return (my.result)
}

get_drc <- function (ID, activity, concentration, create.plot) {
  key <- unique(ID)

  #the drc plotting function can plot multiple lines, we only plot a single line with ID 1
  ID = 1

  my.df <- cbind(activity, concentration, ID)
  my.df <- as.data.frame(my.df)
  my.df <- na.omit(my.df)

  ic50.m1 <- NULL
  ic50.m2 <- NULL
  valid.points <- NULL

#create collapsed strings to attach it as raw data to the dataset
Dose <- paste(concentration, sep="," , collapse=",")
Response <- paste(activity, sep="," , collapse=",")

#here is the guts: make curve fit model, 4-parameter logistics 
try(ic50.m1 <- drm(activity ~ concentration, ID, data = my.df , fct = LL.4(), robust="median"))

#FDR method returns a vector true/false for which points to include
try(valid.points <- FDR(residuals(ic50.m1)))

#perform a least squares curve fit, exclude outliers, use coefficients of robust curve fit as starting values
try(ic50.m2 <- drm(activity ~ concentration, ID, data = my.df ,subset=valid.points, start = ic50.m1$coefficients, fct = LL.4(), robust="mean"))

if(!is.null(ic50.m2)){
  
  #check whether fit has converged and assign results
  if(ic50.m2$fit$convergence==TRUE) {
    
    #extract coefficients
    b <- coefficients(ic50.m2)[1]   #Hill coefficient
    c <- coefficients(ic50.m2)[2]   #asymptote 1
    d <- coefficients(ic50.m2)[3]   #asymptote 2
    e <- coefficients(ic50.m2)[4]   #IC50
    
    RSE <- summary(ic50.m2)$rseMat[1] #residual standard error estimated
    
    #predict at different doses
    Response.lowestdose.predicted <- predict(ic50.m2, data.frame(concentration=min(my.df$concentration), ID=ID), se.fit=FALSE)[1]
    Response.highestdose.predicted <- predict(ic50.m2, data.frame(concentration=max(my.df$concentration), ID=ID), se.fit=FALSE)[1]
    
    #create properties with common names according to BAO
    HillCoefficient <- b
    IC50.relative <- e 
    Response.difference <- abs(Response.lowestdose.predicted  - Response.highestdose.predicted)
    pIC50 <- -log10(e/1000000)
    
    #determine whether the fit is valid
    FitIsValid <- TRUE
    Activity.call <- "active"
    Problem <- ""
	#a valid curve gets a blue color, until it does not fullfill one of the conditions below
	my.col = "blue"
    
    #not valid if the response difference is too small
    if (Response.difference < 25) {
      FitIsValid <- FALSE
      Problem <- "Response Difference lower than 25%"
      Activity.call <- "inactive"
	  my.col = "red"
      }
    
    #not valid if IC50 is outside the measured range
    if (IC50.relative > max(concentration)) {
      FitIsValid <- FALSE
      Problem <- "IC50 larger than highest measured concentration"
      Activity.call <- "inactive"
	  my.col = "red"
    }
    
    if (IC50.relative < min(concentration)) {
      FitIsValid <- FALSE
      Problem <- "IC50 lower than lowest measured concentration"
      Activity.call <- "inconclusive"
	  my.col = "red"
    }
    
    #derive the 95% confidence interval from the least squares model
    confidence.interval <- confint(ic50.m2, parm= c("e"), level = 0.95)
    
    IC50.relative.lower <- confidence.interval[1] 
    IC50.relative.higher <- confidence.interval[2]
	
	#test if there is any dose-response relationship at all
	p.value <- noEffect(ic50.m2)[3]
    
    #round numbers
    Response.lowestdose.predicted <- shapenumber(Response.lowestdose.predicted)
    Response.highestdose.predicted <- shapenumber(Response.highestdose.predicted)
    HillCoefficient <- shapenumber(HillCoefficient)         
    Response.difference <- shapenumber(Response.difference)
    IC50.relative <- shapenumber(IC50.relative)
    IC50.relative.lower <- shapenumber(IC50.relative.lower)
    IC50.relative.higher <- shapenumber(IC50.relative.higher)
    pIC50 <- shapenumber( -log10(IC50.relative/1000000))
	p.value <- shapenumber(p.value)

	#create subtitle with formatted results
	subtitle <-  paste("IC50 ", IC50.relative , " uM, RespDiff " , Response.difference , " %, HillCoeff " , HillCoefficient)

  }



}

  #if fit did not converge
  else{
    Response.lowestdose.predicted <- NA
    Response.highestdose.predicted <- NA
    Response.difference <- NA
    HillCoefficient <- NA
    IC50.relative <- NA 
    FitIsValid <- FALSE
    Problem <- "Fit did not converge"
    Activity.call <- "inconclusive"
    IC50.relative.lower <- NA 
    IC50.relative.higher <- NA 
    RSE <- NA
    b <- NA
    c <- NA
    d <- NA
    e <- NA
    pIC50 <- NA
	p.value <- NA
	my.col="violet"

	subtitle = "fit did not converge"
    }

#prepare plotting

#these are the standard axis limits
ylim.low = 0
ylim.high = 125

#if the points to plot are outside, the limits are adapted
if (min(activity) < ylim.low) {ylim.low <- min(activity)}
if (max(activity) > ylim.high) {ylim.high <- max(activity)}

#the title and subtitle of the plot	
main.text <- key

	#draw the plot	if create.plot variable is set to true
	if (create.plot) {
		drawplot (my.df, ic50.m2, valid.points, Problem, main= main.text , ylim=c(ylim.low,ylim.high), sub=subtitle, xlab="concentration (uM)", ylab="Response (%)", col=my.col)
	}

  # Ergebnis zurückgeben
	outvar <- data.frame(key, Response.lowestdose.predicted, Response.highestdose.predicted, Response.difference, HillCoefficient, IC50.relative, IC50.relative.lower, IC50.relative.higher, pIC50, FitIsValid, Activity.call, Problem, RSE,  b, c, d, e, p.value, Dose, Response)
  
  return (outvar)
}

#main

#in the case of plotting, we set create.plot variable to true, but do not assign the result to knime.out
result <- get_drc(knime.in$"ID", knime.in$"activity", knime.in$"concentration", FALSE)
knime.out <- result