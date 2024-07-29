In this Repo, we have 2 scrips for you to check:

RiskRegressionIssues.R covers two errors we found within the riskRegression library
One error when using plotRisk()
It looks like the function refers to 'model' instead of 'models', could just be a typo
The other error occurs when using plotCalibration() and the event are very few
This error could be fixed by using more decimal places in the round() function
However, we need to consider that when events are few bars are easier to evaluate, so even when this error is soled I am not sure desnity curves are better. In KDpredict we preferred bars for this reason -- we tried curves but they were wild 

nestedCVsl.R
nested cross-validation could be useful in 2 situtions
1) Insufficient old data to train and temporally test the SL (Scotland could have this issue); Steps 1-3 needed
2) If we have a SL produced with old data but we want to assess if an updated (re-trained) version is needed; Steps 2 and 3 needed





