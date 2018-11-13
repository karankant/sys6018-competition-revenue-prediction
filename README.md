# sys6018-competition-revenue-prediction

# Team Mates -
* Luke Kang (sk5be) - Data Cleaning, and Random Forest implementation
* Kanika Dawar (kd2hr) - Data Exploration, and Spline implementation
* Karan Kant (sv2fr) -  Data Exploration, and OLS implementation- GitHub Manager

Collective discussion on selection of appropriate statistical methods and variables along with extracting additional features from the dataset

## We used the old dataset to make predictions

## final_file_submission.R

* It contains the data loading, cleaning (filling legit NAs), and imputation of missing data
* It also contains exploratory data analysis
* We extracted some seasonal variables from the date as well
* The statistical methods were selected based on Assignment guidelines (OLS, Random Forest, and Spline)
* We decided variables to be used in OLS based on forward selection techniques
* For spline, We decided knots based on two methods - n spaced equal intervals and x quartile separation
* Cross Validation (k-fold) for all to decide the version of best fit model

## OUTPUT
* Random forest had a significantly better score compared to the linear approaches on kaggle competition
* RF best score - 2.1084
* OLS best score - 14.0238
* Spline best score - 18.5417


## Reflection

## Who might care about this problem and why?
Companies looking to boost customer revenue would care about this problem. In order to provide value add to companies, it is useful to look at new predictors and variables that haven’t been used before. This data might also help in the online advertising world to deliver advertisements based on the propensity of  a user to make a purchase at a certain season, day, hour for a specific product.

## Why was this problem challenging?
It was hard to make a good prediction since there are lots of missing values in regressors and response. Especially, given 401589 observations, more than 98% of its response value is missing. Most of the predictors were categorical variables with heaps of text values (such as city, networkDomain, keywords, etc), of which some were not in the train dataset which made it that much harder to implement linear models like OLS and splines with good accuracy.

## What other problems resemble this problem?
Time series predictions such as opening/closing stock prices, retail data, weather/temperature at hourly level, economic data ( population of a region, employment rates, interest rates, etc.) resemble this problem. The data we had was within a time period. The dates were used as predictors as well.

## What might account for the differing performance levels of the mandatory models?
The varying flexibility of the models and differing variable selections may account for differing performance levels.  5 variables were used in OLS, while other more were used for the random forest and splines. In general, RF>Splines>OLS. We observed the best performance with RF, followed by OLS & Splines. The fact that there was huge amount of data and variables, most of which were categorical with heaps of text values made the decision trees (RF) a much better fit as compared to a linear model.
