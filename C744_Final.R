### Setup ###

setwd('D:/Documents/WGU') # Sets working directory.

#Loads needed libraries.
library(car)
library(caret)
library(cluster)
library(corrplot)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(FactoInvestigate)
library(gapminder)
library(geoR)
library(ggplot2)
library(ggthemes)
library(gmodels)
library(gridExtra)
library(Hmisc)
library(MASS)
library(missForest)
library(party)
library(plyr)
library(pROC)
library(rcompanion)
library(ROCR)
library(Rtsne)
library(tidyr)
library(tidyverse)
library(openxlsx)

talcum = read.csv("D:/Documents/WGU/C744/Final_Project/Resources/Telco-Customer-Churn_Copy.csv") #Imports data.

factor_cols = c(
  "gender",
  "SeniorCitizen",
  "Partner",
  "Dependents",
  "PhoneService",
  "MultipleLines",
  "InternetService",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod",
  "Churn"
) #Variables imported as character columns, will convert to factor columns.

talcum[, factor_cols] = lapply(talcum[, factor_cols], as.factor) #Converts only vars on list into factor columns.

str(talcum) #Returns basic description of each variable.
summary(talcum) #Returns more detailed description of each variable.

### Data cleansing ###

anyDuplicated(talcum) #Checks if any obs. are duplicates ... No duplicates.

## Finding and handling missing values ##

talcum[!complete.cases(talcum),] #Lists which obs, if any, have missing values ... 11 obs in TotalCharges have missing obs.
talc_comp = talcum[,c(1:19,21)] # Removes the TotalCharge variable.
talcum_comp = talc_comp # Will be used to keep tenure a continuous variable. Won't be used in any analysis.

## Changing values ##

talc_comp$gender = revalue(talc_comp$gender, c('Female'= 0, 'Male' = 1))
talc_comp$MultipleLines = mapvalues(talc_comp$MultipleLines, from=c("No phone service"), to=c("No"))
talc_comp$InternetService = mapvalues(talc_comp$InternetService, from=c("No", "DSL", "Fiber optic"), to=c(0,1,2))
talc_comp$Contract = mapvalues(talc_comp$Contract, from=c("Month-to-month", "One year", "Two year"), to=c(0,1,2))
talc_comp$PaymentMethod = mapvalues(talc_comp$PaymentMethod, from = c("Mailed check","Electronic check","Credit card (automatic)","Bank transfer (automatic)"), to=c(0,1,2,3))

for(i in c(4,5,7,8,17,20)) { # For Partner, Dependents, PhoneService, MultipleLines, PaperlessBilling, Churn.
  talc_comp[,i] = mapvalues(talc_comp[,i], from = c("Yes", "No"), to = c(1,0))
}

for(i in 10:15) { #For OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV and StreamingMovies.
  talc_comp[,i] = mapvalues(talc_comp[,i], from = c("No internet service"), to = c("No"))
  talc_comp[,i] = mapvalues(talc_comp[,i], from = c("Yes", "No"), to = c(1,0))
}

str(talc_comp)
summary(talc_comp)

### Univariate and Bivariate Statistics ###

## Univariate analysis (and some more cleaning). ##

hist(talc_comp$tenure, main = "Histogram of Tenure", xlab = "Tenure in months", col=c("red", "blue"),
     ann=T, labels=T, xpd=NA, xlim=c(0,80), ylim=c(0,1500)
     )
hist(talc_comp$MonthlyCharges, main = "Histogram of MonthlyCharges", xlab = "Charge per month in dollars", col = c("Purple", "Yellow"), 
     ann=T, labels=T, xpd=NA, xlim = c(5,130), ylim=c(0,1200)
)

DescTools::Freq(talcum_comp$tenure) # Shows bins.
DescTools::Freq(talc_comp$MonthlyCharges)
bwplot(talc_comp$MonthlyCharges, main="B&W Plot of MonthlyCharges", xlab="Monthly Charges", col="violet") #BWPlot checks for outliers.

## Reformatting Tenure into bins. ##

talc_comp$ten_bin = with(talc_comp,
                         cut(
                           talc_comp$tenure,
                           breaks = quantile(talc_comp$tenure, probs=seq(0, 1, 0.2)),
                           include.lowest = T,
                           labels = c(0:4)
                         ))
talc_comp$tenure = NULL # Removes tenure variable.

# * ten_bin's ranges:
## 0 = 0-6 months.
## 1 = 7-20 months.
## 2 = 21-40 months.
## 3 = 41-60 months.
## 4 = 61 + months.

writexl::write_xlsx(talc_comp, path="D:/Documents/WGU/C744/Final_Project/E_Light_Clean_Telco_Data.xlsx") # Outputs the cleaned data set as Excel spreadsheet.

## Getting freq. graph of each factor variable. ##

talc_comp %>% keep(is.factor) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + 
  geom_bar(color = "black", fill = "cornflowerblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1.5, fontface = "bold") +
  labs(y = "Count") + theme(axis.title = element_text(size = 12, face = "bold"))

## Bivariate Statistics ##

tenure_by_monthcharge = ggplot(data = talcum_comp, aes(x = tenure, y = MonthlyCharges)) +
  geom_area(aes(x = tenure, y = MonthlyCharges), fill = "red", color = "olivedrab3") +
  labs(title = "Monthly Charges over Customer Tenure", x =
         "Customer Tenure in Months", y = "Montly Charge in $") +
  theme(axis.title = element_text(size = 12, face = "bold"))

tenure_by_monthcharge

churn_by_ten = ggplot(data = talc_comp, aes(x=ten_bin, y=Churn)) +
  geom_count(aes(size=..n.., group=Churn, color=ten_bin), shape="square", show.legend = F) +
  scale_size_binned(range = c(0,14)) +
  theme_minimal() + labs(title="Churn Paterns Over Customer Tenure.", y="Churn (1=Yes)") +
  theme(axis.title = element_text(size=12, face="bold"))

churn_by_ten = churn_by_ten + geom_text(data=ggplot_build(churn_by_ten)$data[[1]], aes(x,y,label=n), color="black",vjust=-2.5, size=5)

churn_by_ten

churn_by_gen = ggplot(data = talc_comp, aes(x=gender, y=Churn)) + 
  geom_count(aes(size=..n.., group=gender, color=gender), shape="square", show.legend = F) +
  scale_size_binned(range = c(0,14)) +
  theme_minimal() + labs(title="Churn Patterns related to gender.", x="Gender (1 = Male)", y="Churn (1=Yes)") + theme(axis.title = element_text(size=12, face="bold"))
churn_by_gen = churn_by_gen +  geom_text(data=ggplot_build(churn_by_gen)$data[[1]], aes(x,y,label=n), color="black",vjust=-2.5, size=5)

churn_by_gen

churn_by_con =ggplot(data = talc_comp, aes(x=Contract, y=Churn)) + 
  geom_count(aes(size=..n.., group=Contract, color=Contract), shape="square", show.legend = F) +
  scale_size_binned(range = c(0,14)) + theme_minimal() +
  labs(title="Churn Patterns related to Contract Type.", x="Contract(0=Month-to-Month, 1= 1 Year, 2 = 2 Year)", y="Churn (1=Yes") +
  theme(axis.title = element_text(size=12, face="bold"))

churn_by_con = churn_by_con + geom_text(data=ggplot_build(churn_by_con)$data[[1]], aes(x,y,label=n), color="black",vjust=-2.5, size=5)

churn_by_con

churn_over_Mon_Charges =ggplot(data = talc_comp, aes(x=MonthlyCharges, y=Churn)) + 
  geom_boxplot(color="blue", fill="#FFD0D0", size=1.5 )+
  theme_minimal() + labs(title="Churn Patterns related to MonthlyCharges.", x="Monthly Charges in $", y="Churn (1=Yes)") + theme(axis.title = element_text(size=12, face="bold"))
churn_over_Mon_Charges

## Correlation detection ##
talcum_continuous = drop_na(talcum) # Keeps TotalCharges in for comparison use only.
num.var <- sapply(talcum_continuous, is.numeric)
corr.matrix <- cor(talcum_continuous[,num.var])
corrplot(corr.matrix, main="Correlation Plot of Tenure, MonthlyCharges and TotalCharges", method="number")

## Contingency tables, analysis of variable's level of discrimination. ##

Churn_chi = lapply(talc_comp, function(x) chisq.test(table(x,talc_comp$Churn), simulate.p.value = T)$p.value) # Only Gender and PhoneService not stat. significant.

Cramming_Churn = lapply(talc_comp, function(x) cramerV(x, talc_comp$Churn)) # Finds Cramer's V statistic.

Chargekal_Wallis = kruskal.test(Churn~MonthlyCharges, data=talc_comp) # Performs Kruskal-Wallis test on MonthlyCharges with Churn as dependent variable.

### Descriptive Data Analysis ###

## Factor Analysis of Mixed Data (FAMD). ##
FAMD_talc = FAMD(talc_comp[, c(2:18,20)]) #Runs FAMD on all of the ind. vars.

FAMD_scree = plot(FAMD_talc$eig[1:5], type="o", main="Scree Plot", xlab="Factor", ylab="E-value",
                  col="blue", pch=21, bg="red")
dimdesc(FAMD_talc)
round(FAMD_talc[["var"]][["contrib"]],2)

#Top 4 variable contributors to the factors.
## Dim.1 - MonthlyCharges (17.36%), InternetService (11.33%), StreamingMovies(10.71%), StreamingTV(10.62%)
## Dim.2 - Contract (20.38%), InternetService(12.99%), PaymentMethod(12.52%), ten_bin(11.45%)
## Dim.3 - InternetService(32.30%), PhoneService(26.34%), MultipleLines(11.65%), Partner(5.51%)
## Dim.4 - ten_bin(47.53%), Contract(42.59%), Dependents(4.00%), Partner(3.37%)
## Dim.5 - Dependents(34.09%), Partner(25.93%), Contract(11.04%), PaymentMethod(9.33%)

# * Variables up for possibly removing: gender,SeniorCitizen, Partner, Dependents, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, PaperlessBilling.

get_eig(FAMD_talc) #Shows the e-values by PC.

corrplot(FAMD_talc$var$contrib, is.corr=F, 
         col=colorRampPalette(c("#ffffff","green","red"))(200),
         tl.col = 'black', tl.cex = 1.1
         ) # Gives nice graph of variable's contribution to each PC.

## Cluster Analysis ##

claire = clara(talc_comp[,2:20], k=3, rngR=F)
claire$silinfo # Gets goodness-of-fit info.
clusplot(claire, color=T, col.p=claire$clustering)


# * k= 2, avg. width of silhouette, 0.624
# * k= 3, 0.70
# * k=4, 0.688
# * k=5, 0.617

### Running Logistic Regression. ###

## Splitting data into training/test(validate) data. ##

set.seed(1234)
talc_comp$customerID = NULL
talc_index = createDataPartition(talc_comp$Churn, p=0.70, list = F)

talc_train = talc_comp[talc_index,]
talc_test = talc_comp[-talc_index,]
talc_ind.vars = talc_train[,-18]

talc_test2 = talc_test[, c(-1,-5,-10)]

## Running GLM and ANOVA commands ##

model = glm(Churn ~., family=binomial(link="logit"), data=talc_train) #builds initial model
summary.glm(model)

anova_test = anova(model, test=c("Chisq")) #Gender, #PhoneService, StreamingTV, StreamingMovies, and MonthlyCharges not significant.
vif(model) #MonthlyCharges, StreamingMovies/TV, InternetService, PhoneService have VIF > 10.

Step_Select1 = stepAIC(model, ~.,  direction = "both") #Variable selection process without interaction terms.
Step_Select2 = stepAIC(model, ~.^2,  direction = "both") #Variable selection process taking interaction terms into account.

# * Step_Select1 removed Gender, Partner, OnlineBackup, DeviceProtection. Final AIC = 4159.44
# * Step_Select2 removed Gender, DeviceProtection, PhoneService. Final AIC = 4124.28

model2 = glm(Churn ~ SeniorCitizen + Partner + Dependents + MultipleLines + 
               InternetService + OnlineSecurity + OnlineBackup + TechSupport + 
               StreamingTV + StreamingMovies + Contract + PaperlessBilling + 
               PaymentMethod + MonthlyCharges + ten_bin + TechSupport:Contract + 
               SeniorCitizen:PaymentMethod + MultipleLines:MonthlyCharges + 
               Partner:PaperlessBilling + MultipleLines:OnlineBackup + SeniorCitizen:StreamingTV + 
               StreamingMovies:Contract + TechSupport:StreamingMovies + 
               Dependents:OnlineBackup,
             family=binomial(link="logit"),
             data=talc_train
             )

summary.glm(model2)

anova_test2 = anova(model2, test="Chisq") # Nothing more can really be removed, either because it's significant or part of a significant interaction term.
vif(model2)

## Misclassification Error and Accuracy. ##

fitted.results = predict.glm(model, newdata=talc_test, type = "response")
fitted.results = ifelse(fitted.results > 0.5, 1, 0)
misClasificError = mean(fitted.results !=talc_test$Churn)
print(paste('accuracy = ', 1 - misClasificError)) # 0.81

# * 0.913 sensitivity, 0.530 specificity.

fitted.results2 = predict.glm(model2, newdata=talc_test2, type = "response")
fitted.results2 = ifelse(fitted.results2 > 0.5, 1, 0)
misClasificError2 = mean(fitted.results2 !=talc_test2$Churn)
print(paste('accuracy = ', 1 - misClasificError2)) # 0.81

# * 0.910 sensitivity, 0.529 specificity.

## AUC/ROC ##

Pr = prediction(fitted.results, talc_test$Churn)
Prf = performance(Pr, measure='tpr', x.measure = 'fpr') # Setting up ROC.
plot(Prf, main="ROC curve of Log Reg", col="red")

Auc = performance(Pr, measure = 'auc')
Auc = Auc@y.values[[1]]
Auc # 0.72

Pr2 = prediction(fitted.results2, talc_test$Churn)
Prf2 = performance(Pr2, measure='tpr', x.measure = 'fpr') # Setting up ROC.
plot(Prf2, main="ROC curve of Log Reg 2", col="blue")

Auc2 = performance(Pr2, measure = 'auc')
Auc2 = Auc2@y.values[[1]]
Auc2 # 0.72

## Confusion Matrix. ##

table(fitted.results)
log_con_mat=table(Predicted = fitted.results, Actual = talc_test$Churn)
print(log_con_mat)

table(fitted.results2)
log_con_mat2=table(fitted.results2, talc_test$Churn)
print(log_con_mat2)

## Odds Ratio ##
OddsR = exp(coef(model))
OddsR2 = exp(coef(model2))

### Decision Tree ###
talc_tree = ctree(Churn~., data = talc_train)
plot(talc_tree, type="simple") #Contract, InternetService, tenure (binned as ten_bin), highest 3 vars.
pred_tree = predict(talc_tree, newdata = talc_test, type="response")
confusionMatrix(pred_tree, talc_test$Churn) # 0.79 accuracy.


### Random Forest ###

Rand_For = randomForest(Churn~. , data = talc_train) # Runs the Random Forest algorithm from RandomForest library.
print(Rand_For)
pred_ran_for = predict(Rand_For, talc_test)
confusionMatrix(pred_ran_for, talc_test$Churn) # 0.80 accuracy.

plot(Rand_For) #Suggested cutoff [100,300]

Tunes = tuneRF(talc_ind.vars, talc_train$Churn, stepFactor = 0.5, plot=T, ntreeTry = 200, 
               trace=T, improve=0.05) # mtry=2, want low OOB error.

Rand_For_Hills = randomForest(Churn~., data=talc_train, ntree=200, mtry=2, importance=T, proximity=T )
plot(Rand_For_Hills)

pred_rf_hills = predict(Rand_For_Hills, talc_test)
confusionMatrix(pred_rf_hills, talc_test$Churn) # 0.80 accuracy, but 0.92 sensitivity versus 0.90 sensitivity prior to tuning.
getTree(Rand_For_Hills)
varImpPlot(Rand_For_Hills, sort=T, main="Important Variables as determined by RF")

Rand_For_Hills2 = randomForest(Churn~.^2, data=talc_train, ntree=200, mtry=2, importance=T, proximity=T )
pred_rf_hills2 = predict(Rand_For_Hills2, talc_test)
confusionMatrix(pred_rf_hills2, talc_test$Churn) 
varImpPlot(Rand_For_Hills2, sort=T, main="Important Variables as determined by RF (w/ interactions)")