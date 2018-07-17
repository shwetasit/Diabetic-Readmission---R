# Diabetic-Readmission---R
classify readmission of diabetic patients to the hospital within 14 days of discharge. 

The dataset is about readmission of diabetic patients into hospitals. The data is collected from 130 hospitals over a span of 10 years (1999-2008). It has a total of 100K records and 50 variables among which 30 are categorical and 11 continuous. After data preprocessing and data cleaning, we have 49734 records over 30 different predictors to classify the readmission of a diabetic patient. 
The extensive Data preprocessing steps include omission of variables and records containing a large number of missing values. Data preprocessing also includes categorical reduction, outlier smoothing etc. The dataset includes 3 categorical variables that have 848 distinct values, which have been reduced to 18 different ICDS codes and then these variables have further been reduced to 8 distinct values to simplify the model. The output variable, “readmitted” has 3 categories that has been reduced to 2. The categorical reduction has been performed on the basis of the frequency of the values in the dataset. If the frequency of the value is low, then they have been reduced to further fewer categories. The dataset has been partitioned to build and validate the model accurately. 60% of the dataset, called training dataset, has been used to build the model and rest 40%, called validation dataset, has been used to validate the model. 
Out of the various classification algorithms, we have implemented CART and Logistic Regression on the dataset. We have performed 2 class classification where these two algorithm are implemented over the full dataset as well as the reduced dataset. In logistic regression, we have implemented the forward variable selection method to perform variable reduction, which will avoid model overfitting. We have drawn lift curve, confusion matrix, and AUC to determine the accuracy of the model. Another Algorithm is CART, which uses the full dataset to build the model. The CART algorithm is extremely simple and contains just one variable to classify the readmission of a patient. In real life, it is impossible to classify the readmission by considering just one predictor. We have implemented the Full tree and the Best-Prune tree, both of which have given similar results. 
Accuracy was used as a constant metric to evaluate our model. The Forward selection method for Logistic Regression gave us a better accuracy i.e. about 65%, whereas the trees formulated in CART is simple and show only one variable to classify the upcoming data as readmission or not, which is ideally not possible. Hence, it is discarded. The Logistic Regression model gave us the best results. Also, we discovered that the duration of being admitted and medication administered are the two variables that play an important role for a person likely to be readmitted to the hospital, including the race and age variables. 
