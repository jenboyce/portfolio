###############################################################################
#  A quick R cheat sheet for multiple regression
###############################################################################


###############################################################################
#  Setup
###############################################################################


# Load libraries
	library(psych)
	library(car)
	library(QuantPsyc)
	library(leaps)
	library(Hmisc)

# Load data
	setwd('c:/Documents/working_directory_name')
	dataset <- read.table("file.txt", na.strings=("-"), header=TRUE)
	attach(dataset)  # This is a very bad idea. But sometimes it's useful.

# Create dummy variables if needed
	# Automagically
		library(dummies)
		dataset_dummies = dummy.data.frame(dataset)	
	# Manually
		dummy1 <- dataset$factor_name ==1)*1
		dummy2 <- dataset$factor_name ==2)*1

# Label and order factors/categorical variables
	dataset$var1 <- factor(dataset$var1, levels = c(1, 2, 3), labels = c("apple", "banana", "currant"))  # Unordered factor values labels
	dataset$var1 <- ordered(dataset$var1,levels = c(1, 2, 3), labels = c("Low", "Medium", "High"))  # Ordered factor values


###############################################################################
#  Explore the data
###############################################################################


# Visualize the data. Do relationships look linear? Any obvious outliers? Does it look like a transformation is needed?

	# Scatter plots
		plot(xvar, yvar)
		abline(lm(yvar~xvar), col='red')
		pairs(~xvar1+xvar2+..., data=dataset)  # Scatter plot matrix of all variable combinations

	# Histograms
		hist(var)
		
	# Boxplots
		boxplot(yvar~ xvar1...)

# Summary statistics
	summary(var)
	describe(var)
	describeBy(yvar, xvar)  # Summary statistics for yvar calculated for each possible value/level of a xvar

# Correlations
	cor(dataset, use='complete', method='pearson')  # Pearson correlation of quantitative vars only using complete observations. Alt: 'pairwise' 'everything'. NOT for dummy variables.
	cor(dataset, use='pairwise.complete.obs')  # Only use rows that don't have missing values for the two variables being compared


###############################################################################
#  Create and explore the model
###############################################################################


# Fit an initial model
	fit <- lm(yvar ~ xvar1 + xvar2 + ...)  # Quantitative variables
	fit <- lm(yvar ~ as.factor(xvar1) + as.factor(xvar2) + ...)  # Categorical variables- can also do this in data setup step
	fit <- lm(yvar~., dataset)  # Use all xvars to fit the model

# Explore the fitted model
	summary(fit)
	coefficients(fit)  # Just the intercept and variable coefficients
	lm.beta(fit)  # Standardized regression coefficients
	confint(fit, level = 0.95)  # Confidence intervals for model intercept and variables


###############################################################################
#  Evaluate the model
###############################################################################


# Evaluate goodness of fit and check that assumptions for regression are satisfied. Look at adj-R2, F-statistic, etc.

# Overview plots
	plot(fit)

# Plot residuals
	plot(fitted(fit), residuals(fit))  # Residual plot vs fitted values plot (difference between predicted and actual y values) - look for linearity and homoscedasticity
	plot(xvar, rstandard(fit))  # Residuals for each xvar- look for linearity and homoscedasticity
	abline(a=0, b=0, col='red')
	qqnorm(residuals(fit))  # Check normality with a QQ plot
	qqline(residuals(fit), col='red')  # Add a QQ line to the QQ plot

# Check for influential points
	# Generally, influential points are hii hat values > 0.5, Cook's distance D > 1, abs(studentized deleted residuals) > 2 or 3.
		summary(influence.measures(fit))
		plot(rstudent(fit)~hatvalues(fit))
		dataset[-13,]  # Remove the observation that is an outlier.

# Check for multicollinearity
	vif(fit)  # Multicollinearity problem if > 10-ish
	vif(fit) > 10  # Return True or False for each variable if VIF > 10

# If needed, re-fit the model, try transformations, consider interaction models, etc, and start the process over again.
# Improved models will generally show lower F-statistic p-values, increased adj-R2.
 

###############################################################################
#  Transformations- if needed
############################################################################### 


# Order to try transformations: log, sqrt, y^2, y^3, inverseY - Box Cox
# If 0's in data and need to log transform, log(var+1)

# BC Power Transform Y
	bc <- powerTransform(yvar~x1+x2+..., data=dataset)
	summary(bc)

# Apply transformation to y and evaluate
	dataset$ynew <- bcPower(dataset$y, bc$lambda)
	pairs(ynew~xvar1+xvar2+..., data=dataset)
	m_bcy <- lm(ynew~xvar1+xvar2..., data=dataset)
	plot(fitted(m_bcy), rstandard(m_bcy))
	plot(dataset$xvar1, rstandard(m_bcy))

# Multivariate Box Cox Transformation
	dataset_complete <- dataset[complete.cases(dataset),]  # Remove NAs- needed for BC transformation
	summary(tranxy<-powerTransform(cbind(xvar1+xvar2...)~1, data=dataset_complete))


###############################################################################
#  Automated Variable Selection Methods
###############################################################################


# Setup for automatic variable selection
	# Remove NAs
		dataset <- na.omit(dataset)
	
	# Create list of xvars
		xvarlist <- c('var1', 'var2', ...)
	
	# Create x matrix
		xvars <- dataset[xvarlist,]
	
	# Create yvar
		yvar <- dataset['var']

# adj-R2  Look for highest adj-R2 value
	leaps(x=dataset[,1:15], y=dataset[,16], names=names(dataset)[1:15], method = 'adjr2')
	cbind(leapmodels$size,leapmodels$which, leapmodels$adjr2)
	head(mat[order(mat[,dim(mat)[2]], decreasing=T),])

# Best subset by Mallows' CP  Best model should have CP = num of parameters in the model
	leapFit <- leaps(x=dataset[,1:4], y=dataset[,5], names=names(dataset)[1:4], method = 'Cp')
	mat <- cbind(leapmodels$size,leapmodels$which, leapmodels$Cp)
	head(mat[order(mat[,dim(mat)[2]]),])

# Backward Selection
	step(fit, direction = "backward")

# Forward selection
	base <- lm(lngrowth~1, data=dataset)
	step(base, scope = list(upper=fit, lower=~1), direction = "forward")

# Stepwise with AIC criteria
# Base is a lm with a single xvar- one that you're pretty sure should be in the final model
	step(base, scope = list( upper=fit, lower=~1 ), direction = "forward", trace=FALSE)