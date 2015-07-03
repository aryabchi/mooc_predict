
# Functions for model selection & plotting graphics
library(corrgram)
library(bestglm)
library(glmnet)
library(ROCR)
require(ggplot2)


############################################################################################################################
plot_nchapters_ratio_grade <- function(dat = NULL, file="scatterplot_grade_vs_nchapters_ratio.png", course) {

#remove unnecessary data
dat<-with(dat, dat[course_id==course & viewed==1 & is.na(incomplete_flag) & !is.na(nchapters) & !is.na(grade),])
#set cert_grade threshold
intercept<-dat[1,c("cert_grade")]*100

png(file, width =720, height = 720, units = "px", bg = "white")
print(
	qplot(
		x=nchapters/course_chapters*100,
		y=grade*100,
		data=dat,
		color=I("black"),
		geom=c("point"),
		span=1,
		size=I(2),
		se=F,
		xlab="освоено разделов курса, %",
		ylab="контрольный балл",
		main=paste("—лушатели курса", course, "\nв пространстве показателей результативности обучени€")

	)
	+ geom_jitter(position=position_jitter(w=1.0,h=0.5), alpha=0.4)
	+ theme_minimal()
	+ geom_abline(intercept=intercept, slope=0, colour="black", size=I(2)) 
)

dev.off()
#browseURL(file)
}


############################################################################################################################
plot_viewed_prob_prev_viewed <- function(dat = NULL, file="viewed_by_prev_viewed.png", include.by_course=FALSE) {
  # plot no. of prev interactions vs viewed probability
  png(file, width =720, height = 720, units = "px", bg = "white")
  
  by_viewed_no<-with(dat,aggregate(dat[,c("viewed")],by=list(dat[,c("prev_viewed")]),FUN=mean))
  names(by_viewed_no)<-c("prev_viewed","viewed_share")

  plot(
    by_viewed_no$prev_viewed,
    by_viewed_no$viewed_share*100,
    type="b",
    pch=16,
    lwd=3,
    ylim=c(0,100),
    xlim=c(1,max(by_viewed_no$prev_viewed)),
    ylab="дол€ регистрантов, начавших взаимодействие с курсом",
    xlab="общее количество курсов, где регистрант ранее был активен",
    main="ƒол€ активных регистраций относительно количества\nпредшествующих взаимодействий с MOOC\n(по всем курсам платформы EdX)"
  )

  if(include.by_course){
	by_viewed_no_course<-with(dat,aggregate(dat[,c("viewed")],by=list(dat$prev_viewed,dat$course_id),FUN=mean))
	names(by_viewed_no_course)<-c("prev_viewed","course_id","viewed_share")
	by.course<-split(by_viewed_no_course,by_viewed_no_course[,2])
	for (i in 1:length(by.course)){
	  #group<-with(by_viewed_no_course,by_viewed_no_course[course_id==i,])
	  points(x = by.course[[i]][,c("prev_viewed")], y = by.course[[i]][,c("viewed_share")]*100, type="o", col=i)
	}
  }
  
  dev.off()
# browseURL(file)
}


############################################################################################################################
plot_registration_stripchart <- function(dat = NULL, file="registration_stripchart.png", course_id) {

	#subset by course_id
	df.course<-dat[dat$course_id==course_id,]

	df.course<-transform(
		df.course,
		registration_moment=as.numeric(df.course$start_time_DI-df.course$launch_date),
		registration_open_moment=as.numeric(df.course$registration_open-df.course$launch_date),
		wrap_moment=as.numeric(df.course$wrap_date-df.course$launch_date),
		launch_moment=as.numeric(0)
	)

	df.course<-df.course[,c("userid_DI","registration_moment","registration_open_moment","launch_moment","wrap_moment")]

	png(file, width =720, height = 320, units = "px", bg = "white")
	stripchart(
		df.course$registration_moment,
		method="stack",
#		jitter=.1,
		offset = 1/100,
		col=grey(1/3,alpha=0.5),
		pch=20,
		main="ѕлотность регистрации слушателей курса PH278x по дн€м",
		xlab="номер дн€ относительно даты начала курса",
#		ylim = c(1,2)
	)
	abline(v=df.course$registration_open_moment[1],lty=3)
	abline(v=df.course$launch_moment[1],lty=3)
	abline(v=df.course$wrap_moment[1],lty=3)
	text(x=c(df.course$registration_open_moment[1],df.course$launch_moment[1],df.course$wrap_moment[1]),
     		y=1, 
     		labels =c("начало\nрегистрации","начало\nкурса","завершение\nкурса"), 
     		col = 2, 
     		adj = c(0, 1)
     	)
	dev.off()
#	browseURL(file)
}


############################################################################################################################
plot_correlation_matrix <- function(dat = NULL, file="correlation_matrix.png") {

 	png(file, width =720, height = 720, units = "px", bg = "white")
 	corrgram(dat, 
		order=NULL, 
		lower.panel=panel.shade,
           	upper.panel=panel.pts, 
		text.panel=panel.txt,
           	main=""
	)
  	dev.off()
#	browseURL(file)
}


############################################################################################################################
plot_roc_curve <- function(dat = NULL, file="ROC_curve.png") {
  png(file, width =720, height = 720, units = "px", bg = "white")
  plot(dat, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
  abline(0,1)	#add rnd classifier ROC
  dev.off()
#	browseURL(file)
}


############################################################################################################################
bestglm_model <- function(X, y, train) {

Xy<-cbind(X,y=y)

#run model selection (this should take time)
b.glm<-bestglm(Xy[train,], family=binomial, IC="AIC")

#debug: save results & data
#save(list = c("b.glm","Xy","train"), file = "./data/b.glm.RData")

#extract formulas from best models of size 1:ncol(X)
l<-apply(b.glm$Subsets[2:nrow(b.glm$Subsets),(names(b.glm$Subsets) %in% colnames(Xy))], 1 , FUN=function(x){names(x)[x==T]})
lfrm<-lapply(l,FUN = function(x){as.formula(paste("y ~ ", paste(x, collapse= "+")))})
val.errors<-matrix(NA,ncol(X),6)
colnames(val.errors)<-c("ACC","FPR","FNR","specificity","sensitivity","AUC")
val.roc<-list()
for(i in 1:ncol(X)){
	#don't know how to extract coeffs => refit on same data and predict
	print(lfrm[[i]])
	glm.fit<-glm(lfrm[[i]],data=Xy[train,],family=binomial)
	#debug: bestglm BestModel and glm.fit's best model estimates should be equal
	if(i==b.glm$ModelReport$Bestk){
		print(summary(glm.fit))	#our model estimates
		print(summary(b.glm$BestModel))	#bestglm estimates
	}
	glm.probs<-predict(glm.fit,Xy[-train,],type="response")
	glm.pred<-ifelse(glm.probs>0.5,1,0)
	rocr.pred<-prediction(glm.probs,y[-train])
	conf.matrix<-table(pred=glm.pred,orig=y[-train])
	val.errors[i,"ACC"]<-mean(glm.pred==y[-train])
	val.errors[i,"FPR"]<-sum(glm.pred!=y[-train] & y[-train]==0)/sum(conf.matrix[,1]) #conf.matrix[2,1]/sum(conf.matrix[,1])
	val.errors[i,"FNR"]<-sum(glm.pred!=y[-train] & y[-train]==1)/sum(conf.matrix[,2]) #conf.matrix[1,2]/sum(conf.matrix[,2])
	val.errors[i,"specificity"]<-sum(glm.pred==y[-train] & y[-train]==0)/sum(conf.matrix[,1])  #TNR conf.matrix[1,1]/sum(conf.matrix[,1])
	val.errors[i,"sensitivity"]<-sum(glm.pred==y[-train] & y[-train]==1)/sum(conf.matrix[,2]) #TPR conf.matrix[2,2]/sum(conf.matrix[,2])
	val.errors[i,"AUC"]<-performance(rocr.pred, measure = "auc")@y.values[[1]]
	val.roc[i]<-performance(rocr.pred, measure = "tpr", x.measure = "fpr")
}
#best 'validation set' model No.(max AUC criterion)
bm<-which.max(val.errors[,"AUC"])

#return best model formula & performance metrics
return(c(
	val.errors[bm,"ACC"],
#	val.errors[bm,"FPR"],
#	val.errors[bm,"FNR"],
#	val.errors[bm,"specificity"],
#	val.errors[bm,"sensitivity"],
	val.errors[bm,"AUC"],
	formula=as.character(lfrm[bm]),
	ROCcurve=val.roc[bm]
))
}


############################################################################################################################
lasso_model <- function(X, y, train, nlambda=100) {

	y<-as.factor(y)
	Xy<-cbind(X,y=y)
	X<-model.matrix(y ~ . -1, data=Xy)
	lasso<-glmnet(X[train,], y[train], family="binomial", nlambda=nlambda)
	glm.probs<-predict(lasso, newx=X[-train,], type="response")
	glm.pred<-ifelse(glm.probs>0.5,1,0)

	AUCPerf<-apply(glm.probs, 2, FUN=function(x){performance(prediction(x,y[-train]), measure = "auc")@y.values[[1]]})
	ACCPerf<-apply(glm.pred==y[-train], 2, FUN=mean)
	
	lambda.best<-lasso$lambda[which.max(AUCPerf)]
	#best.model<-coef(lasso,s=lambda.best)
	best.model.coefs<-predict(lasso, s=lambda.best, type="coefficients")
	best.model.inds<-predict(lasso, s=lambda.best, type="nonzero")
	vars<-rownames(best.model.coefs)[c(-1)][best.model.inds[,1]] #non-zero coef names to infer model

	return(c(
		ACC=as.numeric(ACCPerf[which.max(AUCPerf)]),
		AUC=max(AUCPerf),
		formula=paste("y ~ ", paste(vars, collapse= "+")),
		ROCcurve=performance(prediction(glm.probs[,which.max(AUCPerf)],y[-train]), measure = "tpr", x.measure = "fpr")
	))
}


############################################################################################################################
step_model <- function(X, y, train, direction) {
	y<-as.factor(y)
	Xy<-cbind(X,y=y)

	#define null & full models
	null.model<-glm(y ~ 1, data=Xy[train,], family=binomial)
	full.model<-glm(y ~ ., data=Xy[train,], family=binomial)

	#specify step arguments
	scp<-list(lower=null.model, upper=full.model) 
	if(direction=="forward"){
		obj<-null.model
	}else{
		obj<-full.model
	}
	keepfunc<-function(model,aic){
		glm.probs<-predict(model,Xy[-train,],type="response")
		glm.pred<-ifelse(glm.probs>0.5,1,0)
		#AUC,ACC on test set & max AUC model formula 
		return(c(
			performance(prediction(glm.probs,y[-train]), measure = "auc")@y.values[[1]],
			mean(glm.pred==y[-train]),
			as.character(model$formula))
		)
	}

	#run stepwise model selection & store metrics in step.glm$keep
	step.glm<-step(object=obj, scope=scp, direction=direction, trace=0, keep=keepfunc)

	#note: formula goes in the 5th row of step.glm$keep component
	vars<-step.glm$keep[5,which.max(step.glm$keep[1,])] 
	return(list(
		ACC=as.numeric(step.glm$keep[2,which.max(step.glm$keep[1,])]),
		AUC=max(step.glm$keep[1,]),
		formula=paste("y ~ ", vars),
		ROCcurve=NA
	))
}


############################################################################################################################
#predict method for `regsubsets`

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

