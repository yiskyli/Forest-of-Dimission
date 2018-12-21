####Step 1:data load
library(data.table)
library(tidyverse)
setwd("C:\\Users\\AM\\Desktop\\HR\\Forest_revised")
hr<-as.tibble(fread("HR_comma_sep.txt")) ##读入数据，tibble是data.frame的一种，但结构更紧凑、简洁,其中fread（）相当于read.table()
glimpse(hr) ##功能类似于print(),用于了解数据的大致结构,也可用str()
sapply(hr,function(x){sum(is.na(x))}) ##查询是否有缺失值，并返回缺失值的数量

####Step 2:Data pre-processing (用于将数据量化&因子化，方便做差异分析)
hr$BusinessTravel<-recode(hr$BusinessTravel,"Non_Travel"="0","Travel_Rarely"="1","Travel_Frequently"="2") ##数据因子化，将水平值赋值
hr$Gender<-recode(hr$Gender,"Male"="0","Female"="1")
hr$MaritalStatus<-recode(hr$MaritalStatus,"Single"="0","Married"="1","Divorced"="2")
hr$OverTime<-recode(hr$OverTime,"Yes"="1","No"="0")
hr$Attrition<-recode(hr$Attrition,'Yes'="1",'No'="0")
hr$Department<-as.factor(hr$Department) ##部分无法量化的数据因子化
hr$EducationField<-as.factor(hr$EducationField)
hr$JobRole<-as.factor(hr$JobRole)
hr$Over18<-as.factor(hr$Over18)


####Step 3:Correlation between different Variable Value
library(corrplot)
cor.hr<-hr %>% select(-Department,-EducationField,-JobRole,-Over18,-EmployeeCount,-StandardHours) ##去除无法量化的数据以及没有标准差的数据
cor.hr$BusinessTravel<-as.numeric(cor.hr$BusinessTravel) ##将因子变量转换为数据型
cor.hr$Gender<-as.numeric(cor.hr$Gender)
cor.hr$MaritalStatus<-as.numeric(cor.hr$MaritalStatus)
cor.hr$OverTime<-as.numeric(cor.hr$OverTime)
cor.hr$Attrition<-as.numeric(cor.hr$Attrition)
pdf(file="correlation of all group.pdf",width = 50,height=50) ##type表示下三角的整体形状，method表示形状为方形,order表示特征向量的顺序
corrplot(corr = cor(cor.hr),bg = "white", mar=c(30,2,2,2),title = "correlations",order = "AOE",type="upper",tl.pos = "d")
corrplot(corr = cor(cor.hr),add=TRUE, type="lower", method="number",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n",cex=1,cex.axis=2)
dev.off()
##another way to Visual correlation of some group
library(PerformanceAnalytics)
cor.hr.new<-cor.hr[c("RelationshipSatisfaction","Age","MonthlyIncome","JobLevel","TotalWorkingYears","YearsInCurrentRole","YearsWithCurrManager","OverTime","Attrition")]
pdf(file="correlation of attrition and factors.pdf",width=35,height=30)
chart.Correlation(cor(cor.hr.new),method = "pearson")
dev.off()

####Step 4:EDA,Exploratory Data Analysis
library(ggplot2) ##使用ggplot2包绘制统计分析图
library(plotly)
pdf(file="Number of people in different occupations.pdf")
ggplot(group_by(hr,JobRole),aes(x=JobRole,fill=JobRole))+geom_bar(width = 1)+coord_polar(theta = "x")+ggtitle("Number of people in different occupations")
dev.off()
pdf(file="Total working years of people in different occupations.pdf")
ggplot(hr,aes(x=JobRole,y=TotalWorkingYears,fill=JobRole))+geom_boxplot()+ggtitle("Total working years of people in different occupations")+stat_summary(fun.y = mean,size=3,color='white',geom = "point")+
       theme(legend.position = "none")
dev.off()
pdf(file="The relationship between attrition and total working years.pdf")
ggplot(hr,aes(x=JobRole,y=TotalWorkingYears,fill=Attrition))+geom_boxplot()+ggtitle("The relationship between attrition and total working years")
dev.off()
pdf(file="Job level of people in different occupations.pdf")
ggplot(hr,aes(x=JobRole,y=JobLevel,fill=Attrition))+geom_boxplot()+ggtitle("Job level of people in different occupations")
dev.off()

####Step 5:The relationship between variant and attribution
ggplot(hr,aes(x=TotalWorkingYears,color=Attrition))+geom_line(stat = "density")+ggtitle("总工作年数和离职的关系")
ggplot(hr,aes(x=JobLevel,fill=Attrition))+geom_histogram(stat="count")+ggtitle("工作水平和离职的关系")
#ggplot(hr,aes(x=promotion_last_5years,fill=left))+geom_histogram(stat="count")+ggtitle("近5年升值和离职的关系")
#ggplot(hr,aes(x=last_evaluation,color=left))+geom_point(stat = "count")+ggtitle("最后一次评价和离职的关系")
#hr %>% group_by(sales) %>% ggplot(aes(x=sales,fill=Work_accident))+geom_bar()+coord_flip()+
#theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())+scale_fill_discrete(labels=c("no accident","at least once"))

####Step 6:Model Construction and evaluation
library(pROC)
library(caret)
library(randomForest)
cor.hr$Attrition<-as.factor(cor.hr$Attrition)#需要转换数据类型，否则容易出现报错
index<-sample(2,nrow(cor.hr),replace = T,prob = c(0.7,0.3))
train<-cor.hr[index==1,];test<-cor.hr[index==2,]
model<-randomForest(Attrition~.,data = train)
predict.cor.hr<-predict(model,test)
confusionMatrix(test$Attrition,predict.cor.hr)

prob.cor.hr<-predict(model,test,type="prob")
roc.cor.hr<-roc(test$Attrition,prob.cor.hr[,2],levels=levels(test$Attrition))
pdf(file="AUC curve.pdf")
plot(roc.cor.hr,type="S",col="red",main = paste("AUC=",roc.cor.hr$auc,sep = ""))
dev.off()
