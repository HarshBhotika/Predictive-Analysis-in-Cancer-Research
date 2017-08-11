# Code written by Harsh Bhotika

tdata <- read.csv(file.choose(), header = TRUE)
attach(tdata)
View(tdata)

model <- step(lm(CRUDE_RATE~1, tdata), direction='both', scope=~ AGE_ADJUSTED_CI_LOWER+AGE_ADJUSTED_CI_UPPER+	AGE_ADJUSTED_RATE+	YEAR+	CRUDE_CI_LOWER+	CRUDE_CI_UPPER+	Alabama+	Alaska+	Arizona+	Arkansas+	California+	Colorado+	Connecticut+	Delaware+	Florida+	Georgia+	Hawaii+	Idaho+	Illinois+	Indiana+	Iowa+	Kansas+	Kentucky+	Louisiana+	Maine+	Maryland+	Massachusetts+	Michigan+	Minnesota+	Mississippi+	Missouri+	Montana+	Nebraska+	Nevada+	New_Hampshire+	New_Jersey+	New_Mexico+	New_York+	North_Carolina+	North_Dakota+	Ohio+	Oklahoma+	Oregon+	Pennsylvania+	Rhode_Island+	South_Carolina+	South_Dakota+	Tennessee+	Texas+	Utah+	Vermont+	Virginia+	Washington+	West_Virginia+	Wisconsin+	Wyoming+	Incidence+	Mortality+	Asian_Pacific_Islander+	Black+	Hispanic+ White1+ American_Indian_Alaska_Native+	Female_Breast1+	Prostate+	Brain_and_Other_NerV_System+	Cervix+	Colon_and_Rectum+	Corpus_and_Uterus_NOS+	Esophagus+	Female_Breast+	Hodgkin_Lymphoma+	Kidney_and_Renal_Pelvis+	Larynx+	Leukemias+	Liver_Intrahepatic_Bile_Duct+	Lung_and_Bronchus+	Myeloma+	Non_Hodgkin_Lymphoma+	Oral_Cavity_and_Pharynx+	Ovary+	Pancreas+	Stomach+	Thyroid+	Urinary_Bladder+	Melanomas_of_the_Skin+	Mesothelioma+	Testis+	Kaposi_Sarcoma+	Female+	Male)
summary(model)

regression <- lm(CRUDE_RATE~ CRUDE_CI_UPPER + Mesothelioma + Colon_and_Rectum +
                   Mortality + Lung_and_Bronchus + Hodgkin_Lymphoma + Larynx +
                   Kaposi_Sarcoma + Hispanic + Esophagus + Female + Female_Breast1 +
                   Female_Breast + Pancreas + Prostate + Corpus_and_Uterus_NOS +
                   Non_Hodgkin_Lymphoma + Ovary + White1 + Leukemias + Kidney_and_Renal_Pelvis +
                   Urinary_Bladder + Black + Melanomas_of_the_Skin + Testis +
                   Utah + California + Texas + Liver_Intrahepatic_Bile_Duct +
                   Georgia + Hawaii + Cervix + Arizona + Colorado + YEAR + Thyroid +
                   Nevada + Illinois + AGE_ADJUSTED_CI_UPPER + CRUDE_CI_LOWER +
                   AGE_ADJUSTED_RATE + AGE_ADJUSTED_CI_LOWER + Stomach + Oral_Cavity_and_Pharynx +
                   Alaska + Pennsylvania + Florida + Idaho + Asian_Pacific_Islander +
                   North_Carolina + Kansas + Connecticut + Nebraska + Minnesota +
                   Virginia + Wyoming + Washington + Maryland + Oregon + Maine +
                   Rhode_Island + Indiana + New_Hampshire + Ohio + New_York +
                   Alabama + Delaware + Vermont + Mississippi + West_Virginia +
                   South_Dakota + Michigan + Missouri)
summary(regression)

regression$coefficients
coef(regression)

# Prediction
vdata <- read.csv(file.choose(), header = TRUE)
attach(vdata)
View(vdata)
prediction <- predict(regression,vdata)
head(prediction)
head(vdata)

### BOOTSTRAP
install.packages("caret")
library(caret)
cancer <- read.csv(file.choose(), header = TRUE)
data(cancer)
train_control <- trainControl(method="boot", number = 100)
model <- train(CRUDE_RATE~., data=cancer, trControl=train_control, method="nb")
##

write.csv(prediction, file = "C:/Users/Harsh/Documents/prediction.csv")

cormatrix <- cor(tdata, use = "complete.obs")
View(cormatrix)

install.packages("heuristica")
library(heuristica)
install.packages("caret")
library(caret)

# CART Algo
cancer <- read.csv(file.choose(), header = TRUE)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
tree.model <- rpart(CRUDE_RATE~ CRUDE_CI_UPPER + Mesothelioma + Colon_and_Rectum +
                      Mortality + Lung_and_Bronchus + Hodgkin_Lymphoma + Larynx +
                      Kaposi_Sarcoma + Hispanic + Esophagus + Female + Female_Breast1 +
                      Female_Breast + Pancreas + Prostate + Corpus_and_Uterus_NOS +
                      Non_Hodgkin_Lymphoma + Ovary + White1 + Leukemias + Kidney_and_Renal_Pelvis +
                      Urinary_Bladder + Black + Melanomas_of_the_Skin + Testis +
                      Utah + California + Texas + Liver_Intrahepatic_Bile_Duct +
                      Georgia + Hawaii + Cervix + Arizona + Colorado + YEAR + Thyroid +
                      Nevada + Illinois + AGE_ADJUSTED_CI_UPPER + CRUDE_CI_LOWER +
                      AGE_ADJUSTED_RATE + AGE_ADJUSTED_CI_LOWER + Stomach + Oral_Cavity_and_Pharynx +
                      Alaska + Pennsylvania + Florida + Idaho + Asian_Pacific_Islander +
                      North_Carolina + Kansas + Connecticut + Nebraska + Minnesota +
                      Virginia + Wyoming + Washington + Maryland + Oregon + Maine +
                      Rhode_Island + Indiana + New_Hampshire + Ohio + New_York +
                      Alabama + Delaware + Vermont + Mississippi + West_Virginia +
                      South_Dakota + Michigan + Missouri, data = tdata)

vdata <- read.csv(file.choose(), header = TRUE)
attach(vdata)
View(vdata)
prediction <- predict(tree.model,vdata)
head(prediction)
head(vdata)

print(prediction)
View(prediction)
plotcp(prediction)
printcp(prediction)
summary(prediction)

plot(prediction, uniform=TRUE,
     main="Classification Tree for Cancer")
text(prediction, use.n=TRUE, all=TRUE, cex=.8)

post(tree.model, file = "", title = "cancer")

names(tree.model)

#mean absolute error
install.packages("Metrics")
library(Metrics)
meanabsoluterror <- mae(vdata$CRUDE_RATE,prediction)
meapercent <- meanabsoluterror*100
Error <- c(meapercent,"%")
Error