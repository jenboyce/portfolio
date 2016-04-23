## Modeling Chicago’s Violent Crime: Summary

Violent crime is a fact of life in Chicago. As law enforcement agencies develop strategies to combat crime, it’s essential to understand the underlying patterns of occurrences in order to predict future crimes. Time series models offer insight that can help further this understanding.

You can walk through the analysis process with Time_Series_Model_Crime_Rate_Walkthrough.md found above.

The data for this study include all incidences of violent crime reported by the Chicago Police Department from January 1, 2001 through February 8, 2016.  “Violent crimes” include assault, battery, homicide, robbery, and sexual assault. The number of violent crimes per day ranged from 80 to 547, with a median of 264 and mean of 270.  Crime rate showed a steady decrease from 2001 to 2016.  

Using a time series model called a SARIMA model, it is possible to describe and predict the rate of violent crime. The model uses a calculation that incorporates crime counts on preceding days, as well as the impact of days whose values lie outside of the typical range (‘shocks’).

The final model can be used to predict future incidences of crime. With each successive day that is predicted beyond the last known value, we become less confident about the predictions. Consequently, the first few values that are predicted are likely to be relatively close to the actual value that will occur, while observations farther into the future will be more of an educated guess. We can be 80% confident that the actual crime rate on a given day will fall within the range of the values given in the table at right.

Statistical tests on the prediction accuracy indicate that on average, the predictions will differ from the actual observed crime rate by 13.9%.

Although imperfect, the model can be used to predict violent crime in Chicago. The forecasted values are plausible, and well within the range that would be expected. In looking at the plotted forecast, however, the predictions show a more gradual change in values than occurs normally in the data. Furthermore, the range of predicted values is quite large, offering limited utility for applications like police staffing levels.

Overall, this is a decent model, but one that leaves much room for further research.

A technical summary of this project [is available.] (https://github.com/jenboyce/portfolio/Time_Series/Time_Series_Model_Crime_Rate_files/Crime_Rate_Modelling_Technical_Summary.pdf)




