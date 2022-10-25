pacman::p_load(tidyverse, mice, DMwR, car, caret, ROCR,naniar)
library(caTools)

#Importing the dataset from URL
data <- read.csv(url('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data'))

#understanding the data
str(data)                   
summary(data)
#read the name of the data
data_name <- read.csv(url('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names'), sep = c(":", ",","..",','))

str(data_name)
summary(data_name)
data_name

head(data)
dataname <- t(data_name)
str(dataname)
head(data_name)

#Setting up the data frame
data2 <- set_names(data, c('hypothyroid',"age", 'sex', 'on_thyroxine', 'query_on_thyroxine',
                           'on_antithyroid_medication', 'thyroid_surgery',
                           'query_hypothyroid', 'query_hyperthyroid',
                           'pregnant', 'sick', 'tumor', 'lithium', 'goitre',
                           'TSH_measured', 'TSH', 'T3_measured', 'T3', 
                           'T4_measured', 'TT4', 'T4U_measured', 'T4U', 'FTI_measured'
                           ,'FTI', 'TBH_measured', 'TBG'))
head(data2)
summary(data2)
str(data2)

cols <- c('hypothyroid',"age", 'sex', 'on_thyroxine', 'query_on_thyroxine',
          'on_antithyroid_medication', 'thyroid_surgery',
          'query_hypothyroid', 'query_hyperthyroid',
          'pregnant', 'sick', 'tumor', 'lithium', 'goitre',
          'TSH_measured', 'T3_measured', 
          'T4_measured', 'T4U_measured','FTI_measured'
          ,'TBH_measured')
          
data2[,cols] <- lapply(data2[, cols], factor)
summary(data2)
str(data2)
colMeans(is.na(data2))

cols2 <- c('age','TSH', 'T3', 'TT4', 'T4U', 'FTI'
           , 'TBG')
data2[,cols2] <- lapply(data2[, cols2], as.numeric)
str(data2)
head(data2)
colMeans(is.na(data2))

head(data2)

corrplot::corrplot(cor(data2[, sapply(data2, is.numeric)],
                       use="complete.obs"), method = "number", type='lower')

data2_input <- mice(data2[, !names(data2) %in% 'TBG'])

hypothyroid = complete(data2_input)
head(hypothyroid)

hypothyroid$Target = data2$hypothyroid
summary(hypothyroid)
colMeans(is.na(hypothyroid))

prop.table(table(hypothyroid$Target))
# 4% hypothyroid
hypothyroid$Target = with(data=hypothyroid,
                    ifelse(Target == "hypothyroid",1,0))
head(hypothyroid)
#set initial seed
set.seed(123)

#create a boolean flag to split data
splitData = sample.split(hypothyroid$Target, SplitRatio = 0.85)
head(hypothyroid)
#split_data
# create train and test datasets
train_set = hypothyroid[splitData,]

nrow(train_set)/nrow(hypothyroid)

test_set = hypothyroid[!splitData,]
nrow(test_set)/nrow(hypothyroid)

dim(train_set)
table(train_set$Target)
prop.table(table(train_set$Target))

#use train to create our 1st model
# use all independent variables 
str(train_set)
model = glm(Target ~ ., data = train_set, family = binomial)
summary(model)
vif(model)

model3 = glm(Target ~ . -T4U_measured, data = train_set, family = binomial)
summary(model3)
vif(model3)

model4 = glm(Target ~ . -T4U_measured -FTI_measured, data = train_set, family = binomial)
summary(model4)
vif(model4)

model5 = glm(Target ~ . -T4U_measured -FTI_measured -T4_measured, data = train_set, family = binomial)
summary(model5)
vif(model5)

model6 = glm(Target ~ . -T4U_measured -FTI_measured -T4_measured -TT4, data = train_set, family = binomial)
summary(model6)
vif(model6)

model2 <- step(model, trace = F)
summary(model2)

#exp(cbind(coef(model), confint(model)))

# test it on the train set
trainPredict = predict(model2, newdata = train_set, 
                       type = 'response')

# assign 0s or 1s for the values
p_class = ifelse(trainPredict > 0.2, 1,0)

matrix_table = table(train_set$Target, p_class)
matrix_table

confusionMatrix(table(p_class,train_set$Target), positive='1')

# over sample data using SMOTE
# -SMOTE(form, data, perc.over = 100, k = 5, perc.under = 200, ...)

# -form: A formula describing the prediction problem
# -data: A data frame containing the original (unbalanced) data set
# -perc.over: A number that drives the decision of 
# how many extra cases from the minority class are generated (known as over-sampling).
# -k: A number indicating the number of nearest neighbours that are used 
# to generate the new examples of the minority class.
# -perc.under:A number that drives the decision of how many extra cases 
# from the majority classes are selected for each case generated from the minority class (known as under-sampling)


train_set$Target = as.factor(train_set$Target)
table(train_set$Target)
#set initial seed
set.seed(123)
train_sm = SMOTE(Target ~ . ,  train_set, 
                 perc.over = 300, 
                 perc.under = 100)

# try perc.over = 300, perc.under = 100
table(train_sm$Target)
dim(train_sm)

prop.table(table(train_sm$Target))

str(train_sm)

# use all independent variables 
model11 = glm(Target ~ ., data = train_sm, family = binomial)


# check for multicollinearity
vif(model11)
summary(model2)

model2 = step(model2, trace = F)

#exp(cbind(coef(model2), confint(model2)))

# test it on the train set
trainPredict = predict(model2, newdata = train_sm, 
                       type = 'response')

# assign 0s or 1s for the values
p_class = ifelse(trainPredict > 0.5, 1,0)

matrix_table = table(train_sm$Target, p_class)
matrix_table

confusionMatrix(table(p_class,train_sm$Target), positive='1')


table(test_set$Target)

# test it on the test set
testPredict = predict(model2, newdata = test_set, 

summary(model2)


#####End#######
