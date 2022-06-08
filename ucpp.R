#Importing required libraries
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(forecast)
library(zoo)

#Loading Data set
data = read.csv("E:\\VIT\\2ND YEAR\\Semester 4\\Data Science\\Project\\USP\\bmw.csv")

#Performing Data Visualization
View(data)
dim(data)
summary(data)
str(data)
head(data)

#Histograms

#-----------------------------------------------
hist(data$mileage)
hist(data$year)

#Scatter Plots
#-----------------------------------------------
print(ggscatter(data, x = "price", y = "mileage", 
                conf.int = TRUE,  cor.method = "pearson",
                xlab = "Price ($)", ylab = "mileage Travelled"))

#--------------------------------------
print(ggscatter(data, x = "price", y = "mpg", 
                conf.int = TRUE,  cor.method = "pearson",
                xlab = "Price ($)", ylab = "mileage per gallen"))

#--------------------------------------
print(ggscatter(data, x = "price", y = "year", 
                conf.int = TRUE,  cor.method = "pearson",
                xlab = "Price ($)", ylab = "Year"))

#--------------------------------------
print(ggscatter(data, x = "price", y = "engineSize", 
                conf.int = TRUE,  cor.method = "pearson",
                xlab = "Price ($)", ylab = "Engine Size"))

#--------------------------------------
print(ggscatter(data, x = "price", y = "tax", 
                conf.int = TRUE,  cor.method = "pearson",
                xlab = "Price ($)", ylab = "Tax"))

#Data Cleaning

#Checking for duplicate values
anyDuplicated(data)



any(is.na(data))
sum(is.na(data))

#Removing NA entries in the data set
row = which(is.na(data)==TRUE)
print(sum(row))
if(sum(row) > 0){
  data = data[-c(row), ]
}


#Box plots
boxplot(data$mileage)

#Partition of data set
set.seed(122)  
index  = sample(c(1:dim(data)[1]), dim(data)[1]*0.6)  
train = data[index, ]
valid = data[-index, ]

#Accuracy of the model
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2}

#Creating model and predicting prices
model = rlm(price~year+transmission+mileage+fuelType+tax+mpg+engineSize , data = train)
summary(model)
RSQUARE(valid$price,predict(model, valid))
new_d = data.frame(model='5 series',
                   year = 2017,
                   transmission='Manual',
                   mileage=38556,
                   fuelType = 'Petrol',
                   tax = 126,
                   mpg = 59.7,
                   engineSize = 1.5)
pri = predict(model,new_d)

cat("\n Pridicted price for given values is :",pri)
#-----------------------------------------------------------
#Data Visualizations ~ Om

#Line plot- Price and Year
#ggplot(data = data,aes(x=price,y=engineSize))+geom_smooth()

#Bar plot- Price and Model
ggplot(data = data,aes(x=price,fill=factor(model)))+geom_bar(position = "fill")

#ggplot(data = data,aes(x=price,fill=factor(mileage)))+geom_bar(position = "fill")

#Bar plot- Year, MPG, Engine Size and Price
ggplot(data = data,aes(x=year,fill=price))+geom_bar()

ggplot(data = data,aes(x=mpg,fill=price))+geom_bar()

ggplot(data = data,aes(x=engineSize,fill=price))+geom_bar()

#Bar plot-Dodge Comparison
ggplot(data = data,aes(x=transmission,fill=factor(price)))+geom_bar(position = "dodge")

#Jitter plot
ggplot(data = Birth_weight,aes(x=price,y=tax,col=price))+geom_jitter()

#Pie Chart- For Transmission and Fuel type
ggplot(data, aes(x = "", y = price, fill = transmission)) +
  geom_bar(stat = "identity") +
  coord_polar("y")

ggplot(data, aes(x = "", y = price, fill = fuelType)) +
  geom_bar(stat = "identity") +
  coord_polar("y")

#Time Series Plot- Engine Size
ggplot(data, aes(x=engineSize)) +
  geom_line(aes(y=price))

#Histogram- Tax
ggplot(data, aes(x=tax)) +
  geom_histogram(binwidth = 2, colour="black", fill="#00AFBB")

#Area Chart- Year, Model and Price
data(data)
ggplot(data, aes(x = year,
                     y = model, 
                     fill = price)) +
  geom_area() +
  labs(title = "Price affected by year and model",
       x = "Year",
       y = "Car Models")

#Correlogram
corr_matrix <- cor(data)

# with circles
corrplot(corr_matrix)

# with numbers and lower
corrplot(corr_matrix,
         method = 'number',
         type = "lower")

#Mosaic Plot- Complete Data set
data(data)
mosaicplot(data)

#Heat map- Complete Data Set
heatmap(as.matrix(year))
image(as.matrix(b[2:7]))

#Line graph
plot(data, type="1")

#Bar chart
barplot(price, legend = TRUE)
