
# Reformatting base dataset and unloading datasets for plotting & classification experiments

#init section
rm(list=ls())

fname="./data/prediction.RData"
df<-read.csv("./data/HMXPC13_DI_v2_5-14-14.csv",header=T,na.strings=c("","NA"),stringsAsFactors=FALSE)
academic.year<-2012
df<-transform(
  df,
  start_time_DI=as.Date(start_time_DI),
  last_event_DI=as.Date(last_event_DI),
  userid_DI_course_id=as.character(paste(userid_DI,course_id,sep="")) #dataframe GUID
)
courses<-read.csv("./data/course_info.csv",header=T,na.strings="")
courses<-transform(
  courses,
  course_id=as.character(course_id),  
  registration_open=as.Date(registration_open),
  launch_date=as.Date(launch_date),
  wrap_date=as.Date(wrap_date),
  subject=as.character(subject),
  min_effort=as.numeric(as.character(min_effort)),
  max_effort=as.numeric(as.character(max_effort)),
  length=as.integer(as.character(length)),
  course_chapters=as.numeric(course_chapters)
)
#df<-with(df, df[is.na(incomplete_flag),])

#end init section
  
df<-merge(df,courses,by=c("course_id"))

df.full<-merge(df,df[df$viewed==1 & !is.na(df$nevents),], by=c("userid_DI"),all=FALSE) #viewed assumes nevents>0
df.full.filtered<-with(df.full, df.full[pmax(start_time_DI.y,launch_date.y)<pmax(start_time_DI.x,launch_date.x),])  #all potential prev course interactions (with overlapping times)
df.full.filtered.agg<-aggregate(df.full.filtered[,c("viewed.y")],by=list(df.full.filtered$userid_DI_course_id.x),FUN=sum) #potentially viewed prev
#df.full.filtered.agg=99,615
names(df.full.filtered.agg)<-c("userid_DI_course_id","prev_viewed")   

df.full.filtered.nol<-with(df.full, df.full[(pmax(start_time_DI.y,launch_date.y)<pmax(start_time_DI.x,launch_date.x)) & (last_event_DI.y<pmax(start_time_DI.x,launch_date.x)) & !is.na(last_event_DI.y),])#only completed prev interactions (NOT overlapping)
agg.list<-c("viewed.y","nevents.y","ndays_act.y","nplay_video.y","nforum_posts.y","subject.x","subject.y","nchapters.y","course_chapters.y","grade.y","length.y")
df.full.filtered.agg.nol<-by(
        df.full.filtered.nol[,agg.list],
        df.full.filtered.nol$userid_DI_course_id.x,
	  FUN=function(x){c(
        prev_viewed		=sum(x$viewed.y), 
        prev_nevents		=sum(x$nevents.y, na.rm=TRUE), #=weighted.mean(nevents.y,length.y), 
        prev_ndays_act		=sum(x$ndays_act.y, na.rm=TRUE), #=weighted.mean(ndays_act.y,length.y),
        prev_nplay_video	=sum(x$nplay_video.y, na.rm=TRUE), #=weighted.mean(nplay_video.y,course_chapters.y),
        prev_nforum_posts	=sum(x$nforum_posts.y, na.rm=TRUE),
        prev_nchapters_ratio	=sum(x$nchapters.y, na.rm=TRUE), #=mean(nchapters.y/course_chapters.y),
        prev_grade		=sum(x$grade.y, na.rm=TRUE), #=mean(grade.y),
        same_subject		=sum(is.element(x$subject.x,x$subject.y))>0
	  )}
)
df.full.filtered.agg.nol<-as.data.frame(do.call("rbind",as.list(df.full.filtered.agg.nol)))
df.full.filtered.agg.nol$userid_DI_course_id<-rownames(df.full.filtered.agg.nol)
#df.full.filtered.agg.nol=50,156

df.prev_viewed<-merge(df, df.full.filtered.agg, by=c("userid_DI_course_id"), all=FALSE)  
model.data<-merge(df.prev_viewed, df.full.filtered.agg.nol, by=c("userid_DI_course_id","prev_viewed"), all=FALSE)  #full/complete prior interactions among potential ones
#model.data=40,865

#final formatting & filtering (registered before launch date, !=CS50x)
model.data<-transform(
  model.data,
  age=academic.year-YoB,
  reg_week=as.integer(as.numeric(start_time_DI-launch_date)/7),
  effort=(min_effort+max_effort)/2,
  LoE_DI=as.factor(LoE_DI),
  gender=as.factor(gender),
  subject=as.factor(subject),
  prerequisites=as.logical(prerequisites),
  same_subject=as.logical(same_subject)
)
levels(model.data$LoE_DI)<-c(levels(model.data$LoE_DI)[1],"Master's","Secondary",levels(model.data$LoE_DI)[4:5])
model.data<-with(
  model.data,
  model.data[start_time_DI<launch_date & course_id!="HarvardX/CS50x/2012",]
)
#model.data=26,144

all.model.vars<-c("viewed",
	"age","LoE_DI","gender", 
	"length","course_chapters","subject","effort","level","prerequisites","cert_grade",
	"reg_week","same_subject",            
	"prev_viewed","prev_nevents","prev_ndays_act","prev_nplay_video","prev_nforum_posts","prev_nchapters_ratio","prev_grade"
)
rownames(model.data)<-model.data$userid_DI_course_id
model.data<-model.data[,(colnames(model.data) %in% all.model.vars)]
model.data<-na.omit(model.data)
#model.data=20,034

df.prev_viewed<-df.prev_viewed[,c("course_id","viewed","prev_viewed")]	#strip unnecessary fields

#unload objects for further classification & plotting
save(list = c("df","df.prev_viewed","model.data"), file = fname)



