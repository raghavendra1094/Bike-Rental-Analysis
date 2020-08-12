rm(list=ls(all=T))
#get Working directory
setwd("F:/Rittal Docs/edwisor.com/02.Portfolio/01.Project-1/Actual/Received")

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','fastDummies')
)
lapply(x, require, character.only = TRUE)

install.packages(c("dplyr","plyr","reshape","ggplot2","data.table","DMwR","caret",'unbalanced',"C50","dummies","e1071","Information","gbm","ROSE","sampling","inTrees"))
install.packages("GGally")

# Install  Require libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")
for ( i in x ) {
  print(i)
  library("ggplot2")
  
}
#load Bike rental data in R
bike = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))
bike_train=bike


#It  shows  variables like 'mnth',holiday','weekday','weathersit' are 
#catogical variabless  and already encoded

#Numeric  variables like 'temp','atem','hum','windspeed' are 
#standardized form

# data  contains  no  missing  values 
# Outliers might be present in variables 'actual','registered','cnt'

#structure of  data
str(bike_train)

# for  four variables data type 'mnth',holiday','weekday','weathersit' have to convert to factor
# Analyze variables  by visualize
# function to create univariate distribution of numeric  variables
univariate_numeric <- function(num_x) {

  ggplot(bike_train)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
  
}
# analyze the distribution of  target variable 'cnt'
univariate_numeric(bike_train$cnt)

# analyse the distrubution of  independence variable 'temp'
univariate_numeric(bike_train$temp)

# analyse the distrubution of  independence variable 'atemp'
univariate_numeric(bike_train$atemp)

# analyse the distrubution of  independence variable 'hum'
univariate_numeric(bike_train$hum)

# analyse the distrubution of  independence variable 'windspeed'
univariate_numeric(bike_train$windspeed)

# analyse the distrubution of  independence variable 'casual'
univariate_numeric(bike$casual)

# analyse the distrubution of  independence variable 'casual'
univariate_numeric(bike$registered)

# the above graph is showing   'cnt' data is normally   distributed
# Visualize categorical Variable 'mnth' with target variable 'cnt'

ggplot(bike_train, aes(x=as.factor(mnth), y=cnt),fill="grey") + 
  stat_summary(fun.y="mean", geom="bar")

ggplot(bike_train)+
  geom_histogram(aes(x=cnt,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=cnt,y=..density..))

# Visualize categorical Variable 'holiday' 
ggplot(bike_train) +
  geom_bar(aes(x=holiday),fill="grey")

# it is showing that almost all the  cycle rentals are happening  on holidays
# Visualize categorical Variable 'weekday' 
ggplot(bike_train) +
  geom_bar(aes(x=weekday),fill="grey") 

# it is showing  counts are same on all weekdays
# Visualize categorical Variable 'weathersit' 
ggplot(bike_train) +
  geom_bar(aes(x=weathersit),fill="grey") 

# count  is more when  whether is " Clear, Few clouds, Partly cloudy, Partly cloudy"

# *****************bivariate  relationship between numeric variables****************************

#check the relationship between 'temp' and 'atemp' variable
ggplot(bike_train, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()

#This  graph is saying that very strong relationship  between 'temp' and 'atemp'
#check the relationship between 'temp' and 'hum' variable

ggplot(bike_train, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()

# here  it is showing  Humidity is increses  till temparature is 0.7 and it is decreasing  gradually
#check the relationship between 'temp' and 'windspeed' variable

ggplot(bike_train, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()

# it is showing that very less nagative   correlation between  temp and windspeed
#check the relationship between all numeric variable using pair plot

ggpairs(bike_train[,c('atemp','temp','hum','windspeed','cnt')])

# that above plot stating that less  nagative relationship between
# 'cnt'-'hum'  and cnt-windspeed
# and there is strong positive relationship between 
# temp- cnt and  atemp-cnt

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = bike_train, aes(x =temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25)
hist2 = ggplot(data = bike_train, aes(x =hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25)
hist3 = ggplot(data = bike_train, aes(x =atemp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 25)
hist4 = ggplot(data = bike_train, aes(x =windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25)
gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = bike_train, aes(x =temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = bike_train, aes(x =hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = bike_train, aes(x =atemp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = bike_train, aes(x =windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

# *************visualize the relationship between categorical variable***************

#check relationship between  season and holiday
rel_mnth_holi= table(bike_train$season,bike_train$holiday)
rel_mnth_holi
barplot(rel_mnth_holi)

# here contgency table showing  holiday=0  is same for almost all the seasons
#check relationship between  season and weekday
rels_cats_2 <- table(bike_train$season,bike_train$weekday)
barplot(rels_cats_2)

#check relationship between  season and weathersit
rels_cats_3 <- table(bike_train$weathersit,bike_train$season)
rels_cats_3
prop.table(rels_cats_3,2)
barplot(rels_cats_3)

#It is stating that in all the season  whether 1 type is large numbers
##check relationship between  holiday and weathersit
rels_cats_4 <- table(bike_train$weathersit,bike_train$holiday)
rels_cats_4
barplot(bike_train$weathersit,bike_train$holiday)

#to check in proportion
prop.table(rels_cats_4,2)
barplot(rels_cats_4)
# it it staing that holiday type '0' and  weathersit type '1' almost covered 0.63%

#Check the distribution of categorical Data using bar graph
bar1 = ggplot(data = bike_train, aes(x = season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = bike_train, aes(x = weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = bike_train, aes(x = holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data = bike_train, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# ## Plotting plots together
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)

######Exploratory Data Analysis##################

bike_train$season=as.factor(bike_train$season)
bike_train$mnth=as.factor(bike_train$mnth)
bike_train$yr=as.factor(bike_train$yr)
bike_train$holiday=as.factor(bike_train$holiday)
bike_train$weekday=as.factor(bike_train$weekday)
bike_train$workingday=as.factor(bike_train$workingday)
bike_train$weathersit=as.factor(bike_train$weathersit)

d1=unique(bike_train$dteday)
df=data.frame(d1)
bike_train$dteday=as.Date(df$d1,format="%Y-%m-%d")
df$d1=as.Date(df$d1,format="%Y-%m-%d")
bike_train$dteday=format(as.Date(df$d1,format="%Y-%m-%d"), "%d")
bike_train$dteday=as.factor(bike_train$dteday)

str(bike_train)

###Missing Values Analysis###############################################

# 1. checking for missing value
missing_val = data.frame(apply(bike_train,2,function(x){sum(is.na(x))}))

##############Outlier Analysis##########

# 1.BoxPlots - Distribution and Outlier Check
install.packages(c('evaluate','gridExtra'))
numeric_index = sapply(bike_train,is.numeric) #selecting only numeric
numeric_data = bike_train[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
  {
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_train))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="cnt")+
              ggtitle(paste("Box plot of count for",cnames[i])))
   }

gridExtra::grid.arrange(gn1,gn2,ncol=3)
gridExtra::grid.arrange(gn3,gn4,ncol=2)

# detect outliers in  'actual' , 'registered' and 'cnt' variables
ggplot(data = bike, aes(x = "", y = casual)) + 
  geom_boxplot()
#coord_cartesian(ylim = c(0, 10000))
# it is showing there are few outliers in  casual variables

# boxplot for  Registered  variable
ggplot(data = bike, aes(x = "", y = registered)) + 
  geom_boxplot() 
#coord_cartesian(ylim = c(0, 10000))
# there  is no outliers  in registered variables

# boxplot for  cnt variable
ggplot(data = bike_train, aes(x = "", y = cnt)) + 
  geom_boxplot()
#coord_cartesian(ylim = c(0, 10000))

# it is  showing that there is no outliers in  cnt variable

# #################  Treat Outliers ##############

# analyse relationship between causal and cnt variables before  outlier treatment
ggplot(bike, aes(x= casual,y=cnt)) +
  geom_point()+
  geom_smooth()
bike_train_out <-  bike

# #Remove outliers using boxplot method
val = bike_train_out$casual[bike_train_out$casual %in% boxplot.stats(bike_train_out$casual)$out]
bike_train_out = bike_train_out[which(!bike_train_out$casual %in% val),]

# Boxplot after removing  outliers
# boxplot for  casual variable
ggplot(data = bike_train_out, aes(x = "", y = casual)) + 
  geom_boxplot()

# verify the relationship after  outliers
ggplot(bike_train_out, aes(x= casual,y=cnt)) +
  geom_point()+
  geom_smooth()

cor(bike$casual,bike$cnt)
cor(bike_train_out$casual,bike_train_out$cnt)

# there is difference  in correleation between  casual and  cnt before and after outlier detection
# and also loosing  number of observations 

#####################Feature Selection#################
library("corrgram")

## Correlation Plot 
corrgram(bike_train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Dimension Reduction####
bike_train = subset(bike_train,select = -c(atemp))

########################  Normality  check #################################
#Normalisation
cnames = c("casual","registered")

for(i in cnames){
  print(i)
  bike[,i] = (bike[,i] - min(bike[,i]))/
    (max(bike[,i] - min(bike[,i])))
}

bike$casual
#bike_train_features

#######Model Development####################################################
bike_train=subset(bike_train,select = -c(instant,casual,registered,dteday))

###########################################
install.packages('DataCombine')
library('DataCombine')

rmExcept("bike_train")
train_index = sample(1:nrow(bike_train), 0.8 * nrow(bike_train))
train = bike_train[train_index,]
test = bike_train[-train_index,]

###########Decision tree regression  #################
fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-11])
print(fit)

#  plotting decision tree
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
par(cex= 0.8)
plot(fit)
text(fit)
rpart.plot(fit)


#############Random Forest Model##########################
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
predictions_RF = predict(RF_model, test[,-11])
plot(RF_model)


################Linear Regression#################

# develop Linear Regression  model
#run regression model
lm_model = lm(cnt ~., data = train)

#Summary of the model
summary(lm_model)

# observe the  residuals and   coefficients  of the linear regression model
# Predict  the Test data 

#Predict
predictions_LR = predict(lm_model, test[,-11])
 

#converting multilevel categorical variable into binary dummy variable
cnames= c("season", "yr", "mnth", "weekday", "workingday", "weathersit","holiday")
data_lr=bike_train[,cnames]
cnt=data.frame(bike_train$cnt)
names(cnt)[1]="cnt"
data_lr <- fastDummies::dummy_cols(data_lr)
data_lr= subset(data_lr,select = -c(dteday,season,mnth,weekday,weathersit))
d3 = cbind(data_lr,bike_train)
d3= subset(d3,select = -c(season,mnth,weekday,weathersit,cnt))
data_lr=cbind(d3,cnt)

#dividind data into test and train
train_index = sample(1:nrow(data_lr), 0.8 * nrow(data_lr))
train_lr = data_lr[train_index,]
test_lr = data_lr[-train_index,]

#Linear regression model making
lm_model = lm(cnt ~., data = train_lr)
predictions_LR_1 = predict(lm_model,test_lr[,-46])

#summary(lm_model)


########################KNN Implementation############################
library(class)
#Predict test data
KNN_Predictions = knn(train[, 1:10], test[, 1:10], train$cnt, k = 5)
#convert the values into numeric
KNN_Predictions=as.numeric(as.character((KNN_Predictions)))

#################evaluating MApe value###############
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}
MAPE(test[,11], predictions_DT)

MAPE(test[,11], predictions_RF)

MAPE(test_lr[,46], predictions_LR_1)

MAPE(test[,11], KNN_Predictions)
#################Evaluate  Model using RMSE################

RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}
RMSE(test[,11], predictions_DT)

RMSE(test[,11], predictions_RF)

RMSE(test_lr[,46], predictions_LR_1)

RMSE(test[,11], KNN_Predictions)

##########extacting predicted values output from Random forest model######################
results <- data.frame(test, pred_cnt = predictions_RF)

write.csv(results, file = 'RF output R .csv', row.names = FALSE, quote=FALSE)


##########################################data visualization##############################################3

#add act_season , act_holiday, act_weathersit columns for better visualisation
bike_train$act_season = factor(x = bike_train$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
bike_train$act_holiday = factor(x = bike_train$holiday, levels = c(0,1), labels = c("Working day","Holiday"))

# 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
#2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
#3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, 
#Light Rain + Scattered clouds 
#4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
# we will take 1=Clear, 2=Cloudy/Mist ,3=Light Rain/snow/Scattered clouds, 4=Heavy Rain/Snow/Fog
bike_train$act_weathersit = factor(x = bike_train$weathersit, levels = c(1,2,3,4), 
                                    labels = c("Clear","Cloudy/Mist","Light Rain/snow/Scattered clouds","Heavy Rain/Snow/Fog"))

#For Actual Counts
ggplot(bike_train,aes(x=season,y=cnt,fill=act_season))+theme_bw()+geom_col()+
  labs(x='Season',y='Total_Count',title='Season wise distribution of counts')

ggplot(bike_train,aes(x=holiday,y=cnt,fill=act_holiday))+theme_bw()+geom_col()+
  labs(x='Holiday',y='Total_Count',title='Holiday wise distribution of counts')

ggplot(bike_train,aes(x=weathersit,y=cnt,fill=act_weathersit))+theme_bw()+geom_col()+
  labs(x='Weather Condition',y='Total_Count',title='Weather Suitation distribution of counts')

#For Predicted Counts
#adding Predicted_season , Predicted_holiday, Predicted_weathersit columns
results$Predicted_season = factor(x = results$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
results$Predicted_holiday = factor(x = results$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
results$Predicted_weathersit = factor(x = results$weathersit, levels = c(1,2,3,4), 
                                   labels = c("Clear","Cloudy/Mist","Light Rain/snow/Scattered clouds","Heavy Rain/Snow/Fog"))

ggplot(results,aes(x=season,y=pred_cnt,fill=Predicted_season))+theme_bw()+geom_col()+
  labs(x='Season',y='Predicted_Count',title='Season wise distribution of Predicted counts')
ggplot(results,aes(x=holiday,y=pred_cnt,fill=Predicted_holiday))+theme_bw()+geom_col()+
  labs(x='Holiday',y='Predicted_Count',title='Holiday wise distribution of Predicted counts')
ggplot(results,aes(x=weathersit,y=pred_cnt,fill=Predicted_weathersit))+theme_bw()+geom_col()+
  labs(x='Weather Suitation',y='Predicted_Count',title='Weather Suitation distribution of Predicted counts')

############################################################################################