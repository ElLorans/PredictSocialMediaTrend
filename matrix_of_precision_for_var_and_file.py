# Obtain matrix for each column and each file
# stating success & reliability of Baseline Model

# Baseline Model is a model that divides a dataset in time windows of 4 weeks (the parameter can be changed when calling the  function), 
# does linear regression on each time window's
# first 3 weeks and use the regression to understand if 4 week will see an increase or a decrease.

# So Baseline Model gets first 3 weeks to predict fourth, gets weeks 2-5 to predict the sixth ... and so on.

# Baseline Model forecasts only whether there will be growth in the last week of each period!

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from sklearn.model_selection import train_test_split             # splits data for training and testing
from sklearn.linear_model import LinearRegression

count_ig_csv = 'emerging_risks_doc_count_instagram.csv'
count_tw_csv = 'emerging_risks_doc_count_twitter.csv'

engagement_fb_csv = 'emerging_risks_local_engagement_facebook.csv'
engagement_ig_csv = 'emerging_risks_local_engagement_instagram.csv'
engagement_tw_csv = 'emerging_risks_local_engagement_twitter.csv'

files = [count_ig_csv, count_tw_csv, engagement_fb_csv, engagement_ig_csv, engagement_tw_csv]

dfs = {'count_ig_csv': pd.read_csv(count_ig_csv), 'count_tw_csv': pd.read_csv(count_tw_csv), 
       'engagement_fb_csv': pd.read_csv(engagement_fb_csv), 
       'engagement_ig_csv': pd.read_csv(engagement_ig_csv), 'engagement_tw_csv': pd.read_csv(engagement_tw_csv)}


def is_up_real_target(ser: pd.Series, mean_previous_week) -> bool:  
    """
    Evaluates whether the last value of a pandas Series is higher than the first one.
    Goal: Understand if during the test period we have a positive or negative trend.
      
    :param ser: column of dataset to predict (REAL value of the part of the dataset selected for test).
    : param mean_previous_week: UNUSED right now 
    TODO: IMPROVE. Maybe compare mean_previous_week to ser.mean() ??
    """ 
    if ser.values[0] < ser.values[-1]:
        return 1
    
    return 0

def is_trending_up_real_target(ser: pd.Series) -> bool:
    """
    Improvement of previous version: estimate sign of linear regression coefficient for real data in week 4.
    """
    model = LinearRegression()
    x = ser.index.values.reshape(-1,1)
    y = ser
    model.fit(x, y)
    if model.coef_ > 0:
        return 1
    return 0
    
def is_up_predicted_target(coefficients: np.array) -> bool:
    """
    Evaluates if slope of the linear regression is positive.
    Goal: Understand if during the trend period we have a positive or negative trend (calculated as slope/coefficient of a 
    regression)
    :param coefficients: coefficients of regression on column of dataset used for training.
    """
    if coefficients[0] > 0:
        return 1
    return 0


def update_eval_values(tp: int, tn: int, fp: int, fn: int, predicted_target:bool, real_target:bool):
    """
    Updates matrix of
     _________________________________
    | True Positive  | False Positive |
     ---------------------------------
    | False Negative | True Negative  |
     _________________________________
    
    depending on the difference 
    Goal: Considering one train/test, understand if the model is correctly predicting if the test period had a positive or negative trend.
    """
    if predicted_target == 1 and real_target == 1:
        tp += 1
    elif predicted_target == 0 and real_target == 0:
        tn += 1
    elif predicted_target == 1 and real_target == 0:
        fp += 1
    elif predicted_target == 0 and real_target == 1:
        fn += 1
    return (tp, tn, fp, fn)

def confusion_matrix_baseline_model(column: pd.Series, step_days=7, month_length=28, evaluate_trend=True):
    """
    Goal: apply a linear regression model to one variable in one file and return the confusion matrix
     _________________________________
    | True Positive  | False Positive |
     ---------------------------------
    | False Negative | True Negative  |
     _________________________________    
     
    The time series of the variable is split into moving time windows of a length of "month_length". To each time window, 
    a linear regression is applied on a "train" (3/4 of "month_length") period and then tested on a "test" 
    period (1/4 of "month length").
    
    calls is_up_real_target and is_up_predicted_target to check if regression corretcly predicts the following "test" period.
    :param evaluate_trend: if set to True, calls is_trending_up_real_target in place of is_up_real_target
    """
    #breakpoint()
    tp, tn, fp, fn = 0, 0, 0, 0

    for day in range(0, 364, step_days):
        month = column[day:(day + month_length)]
        train, test = train_test_split(month, test_size=0.25, shuffle=False)

        model = LinearRegression()
        X_train = train.index.values.reshape(-1,1)
        y_train = train
        model.fit(X_train, y_train)
        
        last_train_period = train_test_split(train, test_size=0.64, shuffle=False)[0] # get last wk of train
        
        if evaluate_trend is False:
            real_target = is_up_real_target(test, last_train_period.mean())
        elif evaluate_trend is True:
            real_target = is_trending_up_real_target(test)
            
        predicted_target = is_up_predicted_target(model.coef_)

        tp, tn, fp, fn = update_eval_values(tp, tn, fp, fn, predicted_target, real_target)

    return {"tp": tp, "tn": tn, "fp": fp, "fn": fn}


def get_df_matrix(data_table: pd.DataFrame, confusion=False, accuracy=False, threshold=10, evaluate_trend=True) -> dict:
    """
    Return the confusion matrix or the accuracy matrix for an entire df.
    Confusion matrix for entire df is a dict of dicts.
    Accuracy matrix for entire df is a dict of floats.
    
    :param threshold: min of # of values different from 0. 
    """
    if confusion == accuracy:
        raise TypeError('Set either confusion or accuracy to True.'
                        '\nUse either get_file_matrix(df, confusion=True) or get_file_matrix(df, accuracy=true)')
        
    matrix = dict()
    for colonna in data_table:
        # do regression only if at least threshold non-zero values, as column with few values get 100% prediction success!
        if colonna != 'date' and sum(data_table[colonna] != 0) >= threshold:
            conf = confusion_matrix_baseline_model(data_table[colonna], evaluate_trend=evaluate_trend)
                
            if confusion is True:
                matrix[colonna] = conf
            
            elif accuracy is True:
                matrix[colonna] = (conf['tp'] + conf['tn']) / sum(conf.values())
    return matrix

# confusion matrixes not suited to csv export: each cell is a dictionary with the confusion matrix!
confusion_matrixes = {df: get_df_matrix(dfs[df], confusion=True) for df in dfs} # takes 30-60 seconds

accuracy_matrix = {df: get_df_matrix(dfs[df], accuracy=True) for df in dfs }
    
acc_df = pd.DataFrame(accuracy_matrix)
# acc_df.to_csv("accuracy_matrix.csv")

source_accuracies = acc_df.mean(axis=0) # by column
topic_accuracies = acc_df.mean(axis=1)  # by row
print(source_accuracies)
source_accuracies.to_csv("BaselineModelAccuracyByDoc.csv", header=["Baseline Model Avg Accuracy"])

# Threshold of 10 non-0 values caused a loss in accuracy of 

# count_ig_csv         7 % points
# count_tw_csv         0 % points
# engagement_fb_csv    7 % points
# engagement_ig_csv    14 % points
# engagement_tw_csv    0 % points

print(topic_accuracies)
# topic_accuracies.to_csv("BaselineModelAccuracyByTopic.csv", header=["Baseline Model Avg Accuracy"])
