# fORged Contest 2019

The goal of this project is to predict the demand and develop an inventory model to minimize the total cost of an inventory system. The dataset for this contest provides monthly demand for a hardware device for a period of 10 years from 1996 to 2005. The details of the contest can be found at [link](https://higherlogicdownload.s3-external-1.amazonaws.com/INFORMS/fORgedbyMachine-25.pdf?AWSAccessKeyId=AKIAVRDO7IEREB57R7MT&Expires=1570944482&Signature=3Pepc2GyV3GsrGFJjkFX0TfC7ow%3D).

The dataset can be found at [link](https://sites.google.com/usc.edu/gomez/data)

# The Proposed Solution
We use the first 8-year data (month 1 – month 96) as the training data and the last 2-year data (month 97 – month 120) as the test data. For the inventory control part, Autoregressive Integrated Moving Average (ARIMA) has a smaller test error since it is automatically updated as new demand values are observed, while XGBoost has a smaller inventory cost due to the unbalanced penalty of holding cost and backorder cost. We combine ARIMA and XGBoost as a mixed forecast model due to ARIMA’s adjustment power and XGBoost’s cost-robust power. For the inventory control part, we use the insights from the traditional newsvendor problem and heuristically combine the prediction errors into the model. Parameters in the inventory control model are derived by minimizing the total inventory cost when applying demand forecast and inventory control models to the test data. 

# Contents of the Repository
This repository contains the following files:

(1) "RJ_fORged.R": this is the code for implementing the mixed demand forecast model and the inventory control model. This code will output two csv files to report the beginning inventory, order quantity, ending inventory, holding cost, and backorder cost for each month. As a summary, the program will also output total cost, as well as, total and average holding costs, and total and average backorder costs.
(2) "Ten-Year-Demand.csv": this is the 10-year historical demand data.
(3) "Two-Year-Demand.csv": this is the 2-year test data that we generated by using the mean of predictions from ARIMA and XGBoost models.
(4) "RJ_fORged_Report.pdf": this file contains the project report including the mathematical description of the proposed methodology, a description of how to run the inventory planning system for a period of two years, and the preliminary numerical results.  
