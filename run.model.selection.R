#
# Perform best predictive model selection by bestglm, lasso, forward & backward selection methods
# Make some graphics also
#
# Required packages: corrgram, bestglm, glmnet, ROCR, ggplot2
#
# Usage:
#
# 1) ensure required packages are installed
# 2) setwd
# 3) put datafiles "course_info.csv" & "HMXPC13_DI_v2_5-14-14.csv" into working directory
# 4) run "make.data.R" to produce the dataset for prediction models ("prediction.RData")
# 5) run this script to see best prediction models (printed), their AUC, ACC metrics (printed) and produce graphics (saved as PNG files)
#
#

#init
fname<-"./data/prediction.RData"
train.set.size<-0.6


#load model data & utils
load(file = fname)
source(file = "./func.R")


#show summary on all model vars
print(summary(model.data))


#separate data on test/training
set.seed(12345)
train<-sample(seq(to=nrow(model.data)),as.integer(train.set.size*nrow(model.data)),replace=FALSE)
#debug<-sample(seq(to=nrow(model.data)),600,replace=FALSE)


#analyse variables correlations to prevent potential multicollinearity
factor.vars<-c("gender","level","same_subject", "prerequisites","LoE_DI","subject")
print(cor(model.data[,!(colnames(model.data) %in% factor.vars)], method="pearson"))
drop.model.vars<-c("prev_nevents","prev_nchapters_ratio","prev_grade","viewed","prerequisites")


#prepare model.data columns for model selection functions
#also make NOT_viewed=1 the outcome of interest for conventional interpretation of perf measures
y<-ifelse(model.data[,c("viewed")]==1,0,1)	
X<-model.data[,!(colnames(model.data) %in% drop.model.vars)]


#baseline model accuracy on the test set 
prior<-mean(y[-train])


#bestglm, glmnet(LASSO), forward & backward stepwise selection best models
b.glm.best<-bestglm_model(X, y, train)
lasso.best<-lasso_model(X, y, train, nlambda=500)
fwd.best<-step_model(X, y, train, direction="forward")
bwd.best<-step_model(X, y, train, direction="backward")


#print summary of best models
best.models<-rbind(data.frame(model="bestglm",b.glm.best[1:3]),
	data.frame(model="lasso",lasso.best[1:3]),
	data.frame(model="forward",fwd.best[1:3]),
	data.frame(model="backward",bwd.best[1:3]))
print(best.models)


#make publication graphics
plot_roc_curve(b.glm.best$ROCcurve, "ROC_curve_bestglm.png")
#plot_roc_curve(lasso.best$ROCcurve, "ROC_curve_lasso.png")
plot_correlation_matrix(cor(model.data[,!(colnames(model.data) %in% c(drop.model.vars,factor.vars))],method="pearson"))	#correlation matrix of numeric model variables
plot_viewed_prob_prev_viewed(df.prev_viewed, include.by_course=FALSE)	#%viewed vs #prev_viewed on df.prev_viewed (with overlapping) dataset
plot_registration_stripchart(df, course_id="HarvardX/PH278x/2013_Spring")	#registration stripchart (timeline) for sample course HarvardX/PH278x/2013_Spring
plot_nchapters_ratio_grade(df, course="HarvardX/PH278x/2013_Spring")	#scatterplot of grade vs nchapters ratio



