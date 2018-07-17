rm(list=ls()); gc()
setwd('/Users/shwetasaloni/Downloads/')
dat=read.csv('diabetic_data.csv', head=T, stringsAsFactors = F)
#View(dat)
dim(dat)
#colnames(dat)
dat[dat == '?'] <- NA
dat1=na.omit(dat)

dat1=dat1[,setdiff(colnames(dat),c('examide', 'citoglipton', 'glimepiride.pioglitazone',	"metformin.rosiglitazone", 'acetohexamide', 'repaglinide',	'nateglinide',	'chlorpropamide', 'tolbutamide',
                                  'rosiglitazone',	'acarbose',	'miglitol', 'troglitazone',	'tolazamide', 'glyburide.metformin',	'glipizide.metformin',	'metformin.pioglitazone',
                                  'metformin'))]

dim(dat1)
#Normalization of variables 
#dat1<-transform(dat1, encounter_id = log10(dat1$encounter_id))
#dat1<-transform(dat1, patient_nbr = log10(dat1$patient_nbr))
                
              #  (dat1$encounter_id - min(dat1$encounter_id)) / (max(dat1$encounter_id) - min(dat1$encounter_id)))

#dat2=as.data.frame( scale(dat1[,1] ))
#data.Normalization (dat1$encounter_id,type="n1",normalization="column")
unique(dat1$gender)

dat2=dat1[!(dat1$gender=='Unknown/Invalid'), ]
dim(dat2)


unique(dat2$race)
dat2$race= as.factor(ifelse(dat2$race == 'Asian' | dat2$race == 'Hispanic', 'Other',dat2$race))

table(dat2$age)
dat2$age=as.factor(ifelse(dat2$age=="[0-10)" | dat2$age== '[10-20)' | dat2$age== '[20-30)' ,'age_under30',
                          ifelse(dat2$age == '[30-40)' | dat2$age == '[40-50)' | dat2$age == '[50-60)','age_30To60', 
                                 ifelse(dat2$age == '[60-70)' | dat2$age == '[70-80)' | dat2$age == '[80-90)' | dat2$age == '[90-100)','Above60', dat2$age))))

#To count the number of each age type in when readmitted = 1
sum(dat2$age == 'Above60' & dat2$readmitted ==1) # not selected in the model
sum(dat2$age == 'age_30To60' & dat2$readmitted ==1) # more significant
sum(dat2$age == 'age_under30' & dat2$readmitted ==1) # less significant

table(dat2$admission_type_id)
dat2$admission_type_id=as.factor(ifelse(dat2$admission_type_id == '4' | 
                                          dat2$admission_type_id == '5' | 
                                          dat2$admission_type_id == '6' | 
                                          dat2$admission_type_id == '8', '4', dat2$admission_type_id))

table(dat2$discharge_disposition_id)
#Method1
#dat2$discharge_disposition_id=as.factor(ifelse(dat2$discharge_disposition_id %in% c('2', '4', '5', as.character(7:28)), 'others', dat2$discharge_disposition_id ))
#method2
dat2$discharge_disposition_id=as.factor(ifelse(dat2$discharge_disposition_id ==  setdiff( unique(dat2$discharge_disposition_id), c('2', '4', '5', as.character(7:28))  ), dat2$discharge_disposition_id, '0') )                                                

table(dat2$admission_source_id)
dat2$admission_source_id=as.factor(ifelse(dat2$admission_source_id %in% c(as.character(2:6), as.character(8:10), '13','14', '17','22'), '0',dat2$admission_source_id))

table(dat2$medical_specialty)
dat2$medical_specialty=as.factor(ifelse(dat2$medical_specialty %in% setdiff(unique(dat2$medical_specialty), c('Cardiology', 'InternalMedicine', 'Family/GeneralPractice', 'Emergency/Trauma', 'Surgery-General')),
                                        'Others', dat2$medical_specialty))
#dat2$medical_specialty = ifelse(dat2$medical_specialty %in% setdiff(unique(dat2$medical_specialty), c('Cardiology', 'InternalMedicine', 'Family/GeneralPractice', 'Emergency/Trauma', 'Surgery-General')),'Others', dat2$medical_specialty)

table(dat2$diag_1)
#dat2$diag_1= as.factor(ifelse(dat2$diag_1 %in% setdiff(unique(dat2$diag_1), c('410', '414', '427', '428', '486', '715', '786')), '0', '1'))


dat2$diag_1= as.factor(ifelse((dat2$diag_1 >=390 & dat2$diag_1 <= 459) | dat2$diag_1 == 785, 'Circulatory',
                              ifelse((dat2$diag_1 >=460 & dat2$diag_1 <=519) | dat2$diag_1 == 786, 'Respiratory',
                                     ifelse((dat2$diag_1 >= 520 & dat2$diag_1 <= 579) | dat2$diag_1 == 787, 'Digestive',
                                            ifelse(dat2$diag_1 >= 250 & dat2$diag_1 < 251, 'Diabetes',
                                                   ifelse(dat2$diag_1 >= 800 & dat2$diag_1<=999, 'Injury',
                                                          ifelse(dat2$diag_1>=710 & dat2$diag_1<= 739, 'Musculoskeletal',
                                                                 ifelse((dat2$diag_1>= 580 & dat2$diag_1 <= 629) | dat2$diag_1 == 788, 'Genitourinary',
                                                                        ifelse((dat2$diag_1>= 140 & dat2$diag_1 <= 239) | dat2$diag_1 == 780 | dat2$diag_1 == 781 | dat2$diag_1 == 784 |
                                                                                 (dat2$diag_1>= 790 & dat2$diag_1 <= 799) | (dat2$diag_1>= 240 & dat2$diag_1 <= 279 & dat2$diag_1!= 250) | 
                                                                                 (dat2$diag_1>= 680 & dat2$diag_1 <= 709 | dat2$diag_1 == 782) | (dat2$diag_1>= 001 & dat2$diag_1<= 139),'Neoplasms', 'Other')))))))))

table(dat2$diag_2)
#freq_diag_2=count(dat2, 'diag_2')
#write.csv(freq_diag_2,'fre.csv')
#dat2$diag_2= as.factor(ifelse(dat2$diag_2 %in% setdiff(unique(dat2$diag_2), c('250',	'276',	'401',	'403',	'427',	'428')), '0', '1'))

dat2$diag_2= as.factor(ifelse((dat2$diag_2 >=390 & dat2$diag_2 <= 459) | dat2$diag_2 == 785, 'Circulatory',
                              ifelse((dat2$diag_2 >=460 & dat2$diag_2 <= 519) | dat2$diag_2 == 786, 'Respiratory',
                                     ifelse((dat2$diag_2 >= 520 & dat2$diag_2 <= 579) | dat2$diag_2 == 787, 'Digestive',
                                            ifelse(dat2$diag_2 >= 250 & dat2$diag_2 < 251, 'Diabetes',
                                                   ifelse(dat2$diag_2 >= 800 & dat2$diag_2 <=999, 'Injury',
                                                          ifelse(dat2$diag_2>=710 & dat2$diag_2 <= 739, 'Musculoskeletal',
                                                                 ifelse((dat2$diag_2>= 580 & dat2$diag_2 <= 629) | dat2$diag_2 == 788, 'Genitourinary',
                                                                        ifelse((dat2$diag_2>= 140 & dat2$diag_2 <= 239) | dat2$diag_2 == 780 | dat2$diag_2 == 781 | dat2$diag_2 == 784 |
                                                                                 (dat2$diag_2>= 790 & dat2$diag_2 <= 799) | (dat2$diag_2 >= 240 & dat2$diag_2 <= 279 & dat2$diag_2!= 250) | 
                                                                                 (dat2$diag_2>= 680 & dat2$diag_2 <= 709 | dat2$diag_2 == 782) | (dat2$diag_2>= 001 & dat2$diag_2<= 139),'Neoplasms', 'Other')))))))))



table(dat2$diag_3)
#freq_diag_3=count(dat2, 'diag_3')
#write.csv(freq_diag_3,'fre.csv')
#dat2$diag_3= as.factor(ifelse(dat2$diag_3 %in% setdiff(unique(dat2$diag_3), c('250',	'276',	'401',	'428')), '0', '1'))


dat2$diag_3= as.factor(ifelse((dat2$diag_3 >=390 & dat2$diag_3 <= 459) | dat2$diag_3 == 785, 'Circulatory',
                              ifelse((dat2$diag_3 >=460 & dat2$diag_3 <= 519) | dat2$diag_3 == 786, 'Respiratory',
                                     ifelse((dat2$diag_3 >= 520 & dat2$diag_3 <= 579) | dat2$diag_3 == 787, 'Digestive',
                                            ifelse(dat2$diag_3 >= 250 & dat2$diag_3 < 251, 'Diabetes',
                                                   ifelse(dat2$diag_3 >= 800 & dat2$diag_3 <=999, 'Injury',
                                                          ifelse(dat2$diag_3 >=710 & dat2$diag_3 <= 739, 'Musculoskeletal',
                                                                 ifelse((dat2$diag_3 >= 580 & dat2$diag_3 <= 629) | dat2$diag_3 == 788, 'Genitourinary',
                                                                        ifelse((dat2$diag_3 >= 140 & dat2$diag_3 <= 239) | dat2$diag_3 == 780 | dat2$diag_3 == 781 | dat2$diag_3 == 784 |
                                                                                 (dat2$diag_3 >= 790 & dat2$diag_3 <= 799) | (dat2$diag_3 >= 240 & dat2$diag_3 <= 279 & dat2$diag_3 != 250) | 
                                                                                 (dat2$diag_3 >= 680 & dat2$diag_3 <= 709 | dat2$diag_3 == 782) | (dat2$diag_3 >= 001 & dat2$diag_3 <= 139),'Neoplasms', 'Other')))))))))


table(dat2$diag_3)
hist(dat2$time_in_hospital)
dat2$time_in_hospital=(ifelse(dat2$time_in_hospital>= 8,'GreaterThan8',
                                       ifelse(dat2$time_in_hospital>=3 & dat2$time_in_hospital < 8, 'between4to8Days',
                                              ifelse(dat2$time_in_hospital>0 & dat2$time_in_hospital<=2, 'lessthan2days',dat2$time_in_hospital))))


sum(dat2$time_in_hospital == 'GreaterThan8' & dat2$readmitted == 1)
sum(dat2$time_in_hospital == 'between4to8Days' & dat2$readmitted == 1)
sum(dat2$time_in_hospital == 'lessthan2days' & dat2$readmitted == 1)
sum(dat2$time_in_hospital != 'lessthan2days' & dat2$time_in_hospital != 'between4to8Days' & dat2$time_in_hospital != 'GreaterThan8' & dat2$readmitted == 1)

  
table(dat2$time_in_hospital)

hist(dat2$num_lab_procedures)
dat2$num_lab_procedures=(ifelse(dat2$num_lab_procedures>= 70,'Long',
                                ifelse(dat2$num_lab_procedures>=30 & dat2$num_lab_procedures < 70, 'Medium',
                                       ifelse(dat2$num_lab_procedures>=0 & dat2$num_lab_procedures<30, 'less',dat2$num_lab_procedures))))
table(dat2$num_lab_procedures)

hist(dat2$num_procedures)
dat2$num_procedures=(ifelse(dat2$num_procedures>= 4,'Long',
                                ifelse(dat2$num_procedures>=2 & dat2$num_procedures < 4, 'Medium',
                                       ifelse(dat2$num_procedures>=0 & dat2$num_procedures<2, 'less',dat2$num_procedures))))
table(dat2$num_procedures)

hist(dat2$num_medications)
dat2$num_medications=(ifelse(dat2$num_medications>= 40,'High',
                                ifelse(dat2$num_medications>20 & dat2$num_medications < 40, 'Medium',
                                       ifelse(dat2$num_medications>=0 & dat2$num_medications<=20, 'less',dat2$num_medications))))
table(dat2$num_medications)

hist(dat2$number_inpatient)
dat2$number_inpatient=(ifelse(dat2$number_inpatient>4,'High',
                              ifelse(dat2$number_inpatient>=2 & dat2$number_inpatient <= 4, 'Medium',
                                     ifelse(dat2$number_inpatient>=1 & dat2$number_inpatient<=2, 'less',
                                         'None'))))
table(dat2$number_inpatient)

hist(dat2$number_outpatient)
dat2$number_outpatient=(ifelse(dat2$number_outpatient>= 4,'High',
                               ifelse(dat2$number_outpatient>=3 & dat2$number_outpatient <= 4, 'Medium',
                                      ifelse(dat2$number_outpatient>=1 & dat2$number_outpatient<=2, 'less',
                                             ifelse(dat2$number_outpatient==0, 'veryLow',dat2$number_outpatient)))))
table(dat2$number_outpatient)

hist(dat2$number_emergency)
dat2$number_emergency=(ifelse(dat2$number_emergency== 0,'Highly_Emergency',
                                ifelse(dat2$number_emergency>=1 & dat2$number_emergency <= 5, 'Medium_Emergency','Emergency')))
table(dat2$number_emergency)

hist(dat2$number_diagnoses)
dat2$number_diagnoses=ifelse(dat2$number_diagnoses>=10 & dat2$number_diagnoses <= 16 ,'Less',
                               ifelse(dat2$number_diagnoses>=3 & dat2$number_diagnoses <= 8, 'Medium',
                                      ifelse(dat2$number_diagnoses==9, 'High', dat2$number_diagnoses)))
table(dat2$number_diagnoses)

dat2$readmitted=(ifelse(dat2$readmitted<=30, 1,0))
table(dat2$readmitted)


set.seed(1)
id.train = sample(1:nrow(dat2), nrow(dat2)*.6)
id.test=setdiff(1:nrow(dat2), id.train)
dat2.train = dat2[id.train,]
dat2.test = dat2[id.test,]
#write.csv(dat2,'Cleaned.csv')









min.model = glm(readmitted ~ 1, data = dat2.train, family = 'binomial')
library(pscl)
pR2(min.model)# r2 of the model

max.model = glm(readmitted ~ ., data = dat2.train, family = 'binomial')
max.formula = formula(max.model)
#summary(max.model)
obj = step(min.model, direction='forward', scope=max.formula) # it will print out models in each step
summary(obj) # it will give you the final model
#pR2(obj)
#Odd Ration
(exp(coef(obj)))
require(MASS)
exp(cbind((coef(obj))) )


yhat = predict(obj, newdata = dat2.test, type='response')
hist(yhat)

dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > .5] = 1
  out
}

yhat.class = dichotomize(yhat)
sum(yhat.class == dat2.test$readmitted)/length(id.test)



#To analyze the table of deviance
anova(obj, test='Chisq')
anova(min.model,obj,test='Chisq')
library(lmtest)
lrtest(min.model, obj)

# to check the fit of the model McFaddenR2 is used to
#check the fit of the model, it is same as linear regression R2 
library(pscl)
pR2(obj)

#Auc & ROC
library(ROCR)
p <- predict(obj, newdata=subset(dat2.test,select=setdiff(colnames(dat2.test),c('readmitted')), type="response"))
pr <- prediction(p, dat2.test$readmitted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

table(dat2.test$readmitted, p>0.5)
ER<-(6842+892)/(6842+892+10024+2136)
1-ER
plot(prf, col='red')
#As a rule of thumb, a model with good predictive ability 
#should have an AUC closer to 1 (1 is ideal) than to 0.5
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
dat2 %>% filter(dat2$race=="others")
