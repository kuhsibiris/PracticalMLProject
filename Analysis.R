library(dplyr)
library(caret)
library(purrr)
library(tidyr)
library(doParallel)
library(corrplot)
library(Matrix)


rm(list=ls())
registerDoParallel(cores=3)
setwd("~/PracticalMLProject")

if(!file.exists("training.csv")){
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                      "training.csv")
}

if(!file.exists("testing.csv")){
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                      "testing.csv")
}

training<-read.csv("training.csv",stringsAsFactors = FALSE,na.strings = c("","NA","<NA>","#DIV/0!"))
testing<-read.csv("testing.csv")

colsnum<-colnames(training)[sapply(training,class)=="numeric"]
nonmissings<-map_df(training,~sum(!is.na(.x)))
nonmissings<-gather(nonmissings,variable,valor)
difvalues<-map_df(training,~length(unique(.x)))
difvalues<-gather(difvalues,variable,valor)

to_remove<-difvalues$variable[difvalues$valor<3]

training<-training %>% dplyr::select(-one_of(to_remove))

# incross<-createDataPartition(training$classe,p=0.3,list=FALSE) 
# crossv<-training[incross,] training<-training[-incross,]

trainingpre=preProcess(training,c("center","scale","knnImpute"))

training2<-predict(trainingpre,training)
training2$X<-NULL


# #the following variables [1] "roll_belt"        "total_accel_belt" "max_roll_belt"    "max_picth_belt"   "min_roll_belt"   
# #[6] "min_pitch_belt"   "avg_roll_belt"    "avg_yaw_belt"     "accel_belt_y"
# # are highly correlated with one another will remove all but avg_roll_belt that seems to be the most correlated
# # with the rest of variables, others are detected as full colineality
# 
# removecols<-c("min_yaw_belt","amplitude_pitch_arm" ,
#         "min_roll_dumbbell","max_yaw_forearm","amplitude_yaw_arm","min_yaw_dumbbell")
# 
# training2<-training2 %>% dplyr::select(-one_of(removecols))

rankfist<-function(i){
rankMatrix(training2[colnames(training2)%in% colsnum][1:i])
}

fin<-(training2[colnames(training2)%in% colsnum] %>% dim)[2]
milista<-map(1:fin,rankfist)
milista2<-map(milista,`[[`,1)
unlist(milista2)
colnames(training2[colnames(training2)%in% colsnum])[c(10,66,95)]
to_remove2<-colnames(training2[colnames(training2)%in% colsnum])[c(10,66,95)]
training2<-training2 %>% dplyr::select(-one_of(to_remove2))

train_control<-trainControl(method = "cv",number = 10)
training2$user_name<-training2$raw_timestamp_part_1<-training2$raw_timestamp_part_2<-training2$cvtd_timestamp
modelo1<-train(data=training2,method="lda",classe~.-1,trControl=train_control)

data1<-predict(trainingpre,testing)
final<-predict(modelo1,data1[,colnames(data1)%in% colnames(training2)])