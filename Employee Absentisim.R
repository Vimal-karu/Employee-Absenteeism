#Clean the R environment 
rm(list = ls(all = T))

#Setting working directory 
setwd("D:/Data Scientist/Project/Employee Absenteeism")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees', 'readxl')

#install_packages(x)
lapply(x,require,character_only = TRUE)
rm(x)

library(readxl)
library(DMwR)
library(ggplot2)
library(gridExtra)
library(corrgram)
library(DataCombine)
library(caret)
library(randomForest)
library(rpart)
library(randomForest)
library(inTrees)
library(usdm)

# Load the data 
emp_abs = read_excel("Absenteeism_at_work_Project.xls", sheet = 1)

#Explor the data 
str(emp_abs)
dim(emp_abs)
colnames(emp_abs)

#renaming the variables
names(emp_abs)[2]= "Reason_for_absence"
names(emp_abs)[3]= "Month_of_absence"
names(emp_abs)[4]= "Day_of_the_week"
names(emp_abs)[6]= "Transportation_expense"
names(emp_abs)[7]= "Distance_from_residence_to_work"
names(emp_abs)[8]= "Service_time" 
names(emp_abs)[10]= "Workload_average_perday"
names(emp_abs)[11]= "Hit_target"
names(emp_abs)[12]= "Disciplinary_failure"
names(emp_abs)[15]= "Social_drinker"
names(emp_abs)[16]= "Social_smoker" 
names(emp_abs)[20]= "Body_mass_index"
names(emp_abs)[21]= "emp_abseeism_time_in_hours"

# ##Missing value analysis 
# #Create a data frame of missing value percentage 
# 
# missing_val = data.frame(apply(emp_abs,2,function(x){sum(is.na(x))}))
# 
# #convert row names into column
# 
# missing_val$columns = row.names(missing_val)
# row.names(missing_val) = NULL
# 
# #Calculating percentage missing value
# names(missing_val)[1] =  "Missing_percentage"
# missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(emp_abs)) * 100
# 
# #Arrange in decensing order 
# missing_val = missing_val[order(-missing_val$Missing_percentage),]
# 
# #rearanging the columns 
# missing_val = missing_val[,c(2,1)]

# #Save the output for further analysis
# write.csv(missing_val, "missing_percentage_R.csv", row.names = F )
# 
# # Missing value plots
# ggplot(data = missing_val[1:8,], aes(x=reorder(columns, -Missing_percentage),y = Missing_percentage))+
#   geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
#   ggtitle("Missing data percentage") + theme_bw()
# 
# ggplot(data = missing_val[9:15,], aes(x=reorder(columns, -Missing_percentage),y = Missing_percentage))+
#   geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
#   ggtitle("Missing data percentage") + theme_bw()
# 
# ggplot(data = missing_val[16:21,], aes(x=reorder(columns, -Missing_percentage),y = Missing_percentage))+
#   geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
#   ggtitle("Missing data percentage") + theme_bw()
# 
# ggplot(data = missing_val[1:18,], aes(x=reorder(columns, -Missing_percentage),y = Missing_percentage))+
#  geom_bar(stat = "identity",fill = "grey")+xlab("Variables")+
#  ggtitle("Missing data percentage") + theme_bw()
  
#Actual value = 29
#Mean = 26.68079
#Meadian = 25
#KNN = 29

#Mean method 
# emp_abs$Body_mass_index[is.na(emp_abs$Body_mass_index)] = mean(emp_abs$Body_mass_index, na.rm = T)
# 
# #Meadian method
# emp_abs$Body_mass_index[is.na(emp_abs$Body_mass_index)] = median(emp_abs$Body_mass_index, na.rm = T)
emp_abs = as.data.frame(emp_abs)
#KNN method
emp_abs = knnImputation(emp_abs , k = 3)

#To check Missing value in data
sum(is.na(emp_abs))

#changing numeric to categorical of categorical variable 

emp_abs$ID = as.factor(emp_abs$ID)
emp_abs$Reason_for_absence = as.factor(emp_abs$Reason_for_absence)
emp_abs$Month_of_absence = as.factor(emp_abs$Month_of_absence)
emp_abs$Day_of_the_week = as.factor(emp_abs$Day_of_the_week)
emp_abs$Seasons= as.factor(emp_abs$Seasons)
emp_abs$Disciplinary_failure = as.factor(emp_abs$Disciplinary_failure)
emp_abs$Education = as.factor(emp_abs$Education)
emp_abs$Social_drinker= as.factor(emp_abs$Social_drinker)
emp_abs$Social_smoker = as.factor(emp_abs$Social_smoker)


#outlier analysis
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(emp_abs,is.numeric) #selecting only numeric

numeric_data = emp_abs[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
  {
    assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "emp_abseeism_time_in_hours"), data = subset(emp_abs))+
             stat_boxplot(geom = "errorbar", width = 0.5) +
             geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                          outlier.size=1, notch=FALSE) +
             theme(legend.position="bottom")+
             labs(y=cnames[i],x="emp_abseeism_time_in_hours")+
             ggtitle(paste("Box plot of responded for",cnames[i])))
  }

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)
gridExtra::grid.arrange(gn11,gn12,ncol=2)

#Replace all outliers with NA and impute
#create NA 
for(i in cnames){
  val = emp_abs[,i][emp_abs[,i] %in% boxplot.stats(emp_abs[,i])$out]
  #print(length(val))
  emp_abs[,i][emp_abs[,i] %in% val] = NA
}
#Impute with KNN 
emp_abs = knnImputation(emp_abs, k = 5)

# correlation for continuous variables
corrgram(emp_abs[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## ANOVA test for Categprical variable
summary(aov(formula = emp_abseeism_time_in_hours~ID,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Reason_for_absence,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Month_of_absence,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Day_of_the_week,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Seasons,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Disciplinary_failure,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Education,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Social_drinker,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Social_smoker,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Son,data = emp_abs))
summary(aov(formula = emp_abseeism_time_in_hours~Pet,data = emp_abs))

#Dimention reduction of unwanted variables 
emp_final = subset(emp_abs,select=-c(Weight,Day_of_the_week,Education,Pet))
str(emp_final)
dim(emp_final)

#Feature Scaling
#Normality check 
hist(emp_final$Age)
hist(emp_final$emp_abseeism_time_in_hours)

# Updating the continuous and catagorical variable
continuous_vars = c('Distance_from_residence_to_work', 'Service_time', 'Age',
                    'Workload_average_perday', 'Transportation_expense',
                    'Hit_target', 'Height', 
                    'Body_mass_index')

catagorical_vars = c('ID','Reason_for_absence','Disciplinary_failure', 
                     'Social_drinker', 'Son', 'Month_of_absence', 'Seasons', 'Social_smoker')

# Normalization
for(i in continuous_vars)
{
  print(i)
  emp_final[,i] = (emp_final[,i] - min(emp_final[,i]))/(max(emp_final[,i])-min(emp_final[,i]))
}

# Save Data after per processing 
#write.csv(emp_final,"empfs.csv", row.names = T)

#Model Developmen
#Cleaning the environment
rmExcept("emp_final")

#Devide data into test and train using stratified sampling method 

set.seed(1234)
train.index = createDataPartition(emp_final$emp_abseeism_time_in_hours, p = .80, list = FALSE)
train = emp_final[ train.index,]
test  = emp_final[-train.index,]

# Decision tree  
#rmse = 2.96
#acurracy = 97.04
#Develop Model on training data
fit_dt = rpart(emp_abseeism_time_in_hours ~., data = train, method = "anova")
# summary(fit_dt)

#perdict new test cases 
prediction_dt = predict(fit_dt, test[,-17])

RMSE(prediction_dt, test[,17], na.rm = FALSE)

#random forest
#For n = 100
#RMSE = 4.3199
#acurracy = 95.69
#Develop Model on training data
model_rf = randomForest(emp_abseeism_time_in_hours ~., train, importance = TRUE, ntree = 100)

#Presdict test data using random forest model
RF_Predictions = predict(model_rf, test[,-17])

RMSE(RF_Predictions, test[,17])
#For n = 200
#RMSE = 2.872
#acurracy = 97.128
#Develop Model on training data
model_rf = randomForest(emp_abseeism_time_in_hours ~., train, importance = TRUE, ntree = 200)

#Presdict test data using random forest model
RF_Predictions = predict(model_rf, test[,-17])

RMSE(RF_Predictions, test[,17])

#For n = 300
#RMSE = 2.8343
#acurracy = 97.1657
#Develop Model on training data
model_rf = randomForest(emp_abseeism_time_in_hours ~., train, importance = TRUE, ntree = 300)

#Presdict test data using random forest model
RF_Predictions = predict(model_rf, test[,-17])

RMSE(RF_Predictions, test[,17])

#Linear Regression 
#RMSE = 2.9240
#acuraccy = 97.076

#check multicollearity
library(usdm)
vif(emp_final[,-17])

levels(droplevels(emp_final$Reason_for_absence))
train.index1 = sample(1:nrow(emp_final),0.8*nrow(emp_final))
train1 = emp_final[train.index1,]
test1 = emp_final[-train.index1,]

#run regression model 
LM_model = lm(emp_abseeism_time_in_hours ~. , data = train1)
#summary of the model 
summary(LM_model)
#Predict the target variable 
Prediction_LR = predict(LM_model, test1[,1:16])

RMSE(Prediction_LR, test1[,17])

##Till here we have implemented Decision Tree, Random Forest and Linear Regression. Among all of these,
##________ Random Forest and linear Regression  is having highest accuracy.

# Now Predict loss of each month 
# creating subset

loss = subset(emp_final,select = c(Month_of_absence,Service_time,emp_abseeism_time_in_hours,Workload_average_perday))  

# Workloss/month = (absent time * workload)/service time    mathematical formula
loss["month_loss"]=with(loss,((loss[,"Workload_average_perday"]*loss[,"emp_abseeism_time_in_hours"])/loss[,"Service_time"]))

for (i in 1) {
  print('JANUARY')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 2) {
  print('FEBRUARY')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 3) {
  print('MARCH')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 4) {
  print('APRIL')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 5) {
  print('MAY')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 6) {
  print('JUNE')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 7) {
  print('JULY')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 8) {
  print('AUGUST')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 9) {
  print('SEPTEMBER')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 10) {
  print('OCTOBER')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 11) {
  print('NOVEMBER')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}
for (i in 12) {
  print('DECEMBER')
  emp = loss[which(loss["Month_of_absence"]==i),]
  print(sum(emp$month_loss))
}

