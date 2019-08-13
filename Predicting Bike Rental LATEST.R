library(ggplot2) # Visualisation
library(dplyr) # Data Wrangling
library(e1071) # Prediction: SVR
library(randomForest) # Prediction: Random Forest
df = read.csv('F:/Downloads/day.csv')
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
# install.packages("caTools")
install.packages("ie2misc")
library(ie2misc)

library(caTools)
# install.packages(x)
lapply(x, require, character.only = TRUE)
head(df,6)
rm(x)
df$dteday = as.POSIXct(df$dteday, format="%Y-%m-%d")
str(df)
summary(df)
# Checking  for Missing Data
sapply(df, function(x) sum(is.na(x)))

# Exploratory Data Analysis
# Plotting the Count of Rental along with the Temprature
ggplot(df, aes(temp, cnt)) +
  geom_point(aes(color=temp),alpha=0.2) + theme_bw()

# Plotting count against Day
ggplot(df, aes(dteday, cnt)) +
  geom_point(aes(color=season),alpha=0.5) + theme_bw()

# Plotting count against Season
ggplot(df, aes(season, cnt)) +
  geom_boxplot(aes(color=season),alpha=0.5) + theme_bw()

# Outlier Analysis
numeric_index = sapply(df,is.numeric)
numeric_index
numeric_data = df[,numeric_index]
cnames = colnames(numeric_data)
for(i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Count")+
           ggtitle(paste("Box plot of",cnames[i])))
}
# Plot the Graph
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)

# We will now be applying the Multiple Linear Regression

# For Fitting our regression models first we have to split the data into Training and Testing
sample = sample.split(df,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 = subset(df,sample == TRUE)
test1 = subset(df, sample == FALSE)
train1
test1
# Check for Missing Value once,
sapply(train1, function(x) sum(is.na(x)))

# Now fitting Multiple Linear Regression to the Training Set.
input <- train1[,c("weathersit", "temp", "windspeed", "weekday", "hum", "mnth", "season", "casual", "registered", "cnt")]
print(head(input))
# Creating a relationship Model and Getting the coefficiants
model <- lm(cnt~weathersit+temp+windspeed+weekday+season+casual+registered, data = input)
print(model)
summary(model)

# Cor weathersit and Temp
cor(train1$weathersit, train1$windspeed, method = "pearson")
# Ask the confidence intervals for the model coefficients
confint(model, conf.level=0.95)
# Lets check the regression diagontic plots for this model
plot(model)

regressor = lm (formula= cnt~ . , data = train1)
confint(regressor, conf.level=0.95)

plot(regressor)
# Choosing the best model in AIC in a stepwise Algorithm
# The Step( function iteratively removes insignificant features from the model
regressor = step(regressor)
y_pred = predict(regressor, test1)
y_pred
results <- data.frame(datetime = test1$dteday, count = y_pred)
write.csv(results, file = 'BikeSharingDemand_MLR.csv', row.names = FALSE, quote=FALSE)


# Support Vector Regression
regressor = svm(formula = cnt ~ .,
                data = train1,
                type = 'eps-regression',
                kernel = 'radial')
summary(regressor)
y_pred = predict(regressor, test1)
results <- data.frame(datetime = test1$dteday, count = y_pred)
write.csv(results, file = 'BikeSharingDemand_SVR.csv', row.names = FALSE, quote=FALSE)


# Random Forest Regression
regressor = randomForest(x = train1[,-which(names(train1)=="cnt")],
                         y = train1$cnt)
# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, test1)
results <- data.frame(datetime = test1$dteday, count = y_pred)
# Write the results to a csv file
write.csv(results, file = 'BikeSharingDemand_RandomForest.csv', row.names = FALSE, quote=FALSE)

# Mean Absolute error
mae(y_pred, observed, na.rm = FALSE)
