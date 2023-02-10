
#install packages
install.packages("tidyr")
install.packages("visdat")
install.packages("dplyr")
install.packages("corrplot")
install.packages("psych")
install.packages('caTools')
install.packages('caret')
install.packages('e1071')
install.package("ElemStatLearn")



library(visdat)
library(dplyr)
library(naniar)
library(Amelia)
library(corrplot)
library(psych)
library(caTools)
library(caret)
library(e1071)
library(ElemStatLearn)



#Set the Working Directory and Import the PIMA Diabetes dataset
setwd("~/Desktop/University/DMA/CW2")
dataset <- read.csv("diabetes.csv") 


#Or manually select the dataset using the code below
# dataset <- read.csv(file.choose())   

####################---Find the Missing Rows----####################

#Checking for incomplete Rows
#Doesn't work because incomplete rows are labelled with 0 instead of NAs
dataset[!complete.cases(dataset),]



dataset[is.na(dataset$Pregnancies)]
dataset[is.na(dataset$Glucose)]

#Count how many rows are incomplete
#In this case 0 would be considered incomplete

# nrow(dataset[dataset$Pregnancies == 0,]) 
nrow(dataset[dataset$Glucose == 0,]) 
nrow(dataset[dataset$BloodPressure == 0,])
nrow(dataset[dataset$SkinThickness == 0,])
nrow(dataset[dataset$Insulin == 0,])
nrow(dataset[dataset$BMI== 0,])
nrow(dataset[dataset$DiabetesPedigreeFunction == 0,])
nrow(dataset[dataset$Age == 0,])


#NA for dataset1
#install.packages("tidyr")
library(tidyr)
library(dplyr)
library(naniar)

#Creating a new dataset and replacing all the 0s with NAs
#Required to plot a missing data graph
dataset1 <- dataset %>% replace_with_na(replace = list(Glucose = 0,BloodPressure = 0, SkinThickness = 0, Insulin = 0, BMI = 0))
dataset2 <- dataset1



####################---Visualise the missing data----####################
# install.packages("visdat")
library(visdat)
#Shows what type of value is the whole dataset
vis_dat(dataset)

#Displays the missing Values
vis_miss(dataset1)

#Display the missing value graphs using Amelia Package

library(Amelia)
missmap(dataset1, main="Diabetes Missing Data", 
        col=c("yellow", "black"), legend=TRUE)


#Change the 0s with NA for the missing values
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(naniar)

# Split the dataset into the Train and Test set
# install.packages('caTools')
library(caTools)
#Set the seed the same as my partner inorder to get the same random split
set.seed(1)
split = sample.split(dataset1$Outcome, SplitRatio = 0.80)
training_set = subset(dataset1, split == TRUE)
test_set = subset(dataset1, split == FALSE)


####################---Replacing the Missing data with Mean----####################

#Mean imputation method
mean_Glucose <- mean(training_set[,"Glucose"],na.rm= TRUE) 
mean_BloodPressure <- mean(training_set[,"BloodPressure"],na.rm= TRUE) 
mean_SkinThickness <- mean(training_set[,"SkinThickness"],na.rm= TRUE) 
mean_Insulin <- mean(training_set[,"Insulin"],na.rm= TRUE)
mean_BMI <- mean(training_set[,"BMI"],na.rm= TRUE) 

#Which columns contain missing data
dataset1[is.na(dataset1$Glucose),]
dataset1[is.na(dataset1$BloodPressure),]
dataset1[is.na(dataset1$SkinThickness),]
dataset1[is.na(dataset1$Insulin),]
dataset1[is.na(dataset1$BMI),]

#Replace the missing row with the mean value
test_set[is.na(test_set$Glucose),"Glucose"] <- mean_Glucose
test_set[is.na(test_set$BloodPressure),"BloodPressure"] <- mean_BloodPressure
test_set[is.na(test_set$SkinThickness),"SkinThickness"] <- mean_SkinThickness
test_set[is.na(test_set$Insulin),"Insulin"] <- mean_Insulin
test_set[is.na(test_set$BMI),"BMI"] <- mean_BMI

training_set[is.na(training_set$Glucose),"Glucose"] <- mean_Glucose
training_set[is.na(training_set$BloodPressure),"BloodPressure"] <- mean_BloodPressure
training_set[is.na(training_set$SkinThickness),"SkinThickness"] <- mean_SkinThickness
training_set[is.na(training_set$Insulin),"Insulin"] <- mean_Insulin
training_set[is.na(training_set$BMI),"BMI"] <- mean_BMI

#Created dataset2 just to plot the correlation graph
dataset2[is.na(dataset2$Glucose),"Glucose"] <- mean_Glucose
dataset2[is.na(dataset2$BloodPressure),"BloodPressure"] <- mean_BloodPressure
dataset2[is.na(dataset2$SkinThickness),"SkinThickness"] <- mean_SkinThickness
dataset2[is.na(dataset2$Insulin),"Insulin"] <- mean_Insulin
dataset2[is.na(dataset2$BMI),"BMI"] <- mean_BMI


#Check for any NA
any(is.na(test_set))
any(is.na(training_set))


####################---Correlation Graphs---####################
# install.packages("corrplot")
# install.packages("psych")

library(corrplot)
library(psych)


#Before Imputation
# Plot showing how the attributes are correlated
corrplot (corr = cor(dataset),
          method = "circle",
          type = "full",
          t1.pos ="tl",
          order = "original")
GGally::ggcorr(dataset,
               method = c("everything","pearson"),
               label = TRUE,
               label_alpha = TRUE)

corPlot(dataset,
        cex= 0.9,
        numbers = TRUE,
        n = 21,
        zlim = c(-0.5,1),
        stars = TRUE,
        diag = FALSE,
        gr = gr,
        main = "Correlation between the attributes"
)


#After Imputation
#Note to me: Peform this before mean imputation
corrplot (corr = cor(dataset2),
          method = "circle",
          type = "full",
          t1.pos ="tl",
          order = "original")
GGally::ggcorr(dataset2,
               method = c("everything","pearson"),
               label = TRUE,
               label_alpha = TRUE)

corPlot(dataset2,
        cex= 0.9,
        numbers = TRUE,
        n = 21,
        zlim = c(-0.5,1),
        stars = TRUE,
        diag = FALSE,
        gr = gr,
        main = "Correlation between the attributes"
)


####################---Feature Scaling, Extraction & Logistic Regression ---####################


# Feature Scaling
    #ALl columns selected except the dependent variable; last column ("Outcome")
training_set[-9] = scale(training_set[-9])
test_set[-9] = scale(test_set[-9])


# Applying Principal Component Analysis (PCA)
#install.packages('caret')
library(caret)
#install.packages('e1071')
library(e1071)

#New variable which we will use to transform our original dataset into this new dataset with the new Extracted feature
  # Our dataset will now only have 2 new Extracted features derived from all the previous independent variables
pca = preProcess(x = training_set[-9], method = 'pca', pcaComp = 2)
  #Training set transformed to compose of the Extracted features
training_set = predict(pca, training_set)
  #Re arranging the columns to put the Dependent Variable at the end; Just to make it easier for me
training_set = training_set[c(2, 3, 1)]
  #Same Step repeated for test_set
test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]

#  Logistic Regression for the Training set
dataset1$Outcome <- factor(dataset1$Outcome)
classifier = glm(formula = Outcome ~ ., #We want to predict the Outcome results based on all the independent variables
                 family = binomial,
                 data = training_set)
summary(classifier)

# Predicting the Test set results using the classifier as shown above
# We are interested in having 0 and 1 predictions rather than the probabilities
#Predicting the Test set observations using our Logistic Regression classifier
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3]) #Removing last column which is the dependent variable
#Get the results in just two classes 0 & 1
#If prob_pred is >0.5 return 1 else 0
y_pred = ifelse(prob_pred > 0.5, 1, 0)


################################################## Finding the Accuracy############
misclas <- mean(y_pred != test_set$Survived)
print(paste('Accuracy', 1-misclas))

#  Confusion Matrix
  #Note for me: New test_set only has 3 columns
conf = table(test_set[,3], y_pred)
conf




