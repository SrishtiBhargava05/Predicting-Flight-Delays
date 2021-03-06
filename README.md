
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Predicting Flight Delays

## Introduction

In a fast-moving world with optimization of time being essential a major
flight delay is a concerning issue. Thus, it would be beneficial to know
which airline to travel with at what time of the day depending on the
route one is taking. The models fitted below with the help of the US
Aircraft Data (Bureau of Transportation Statistics, 2oo8) aim to aid the
customer in flight selection.

The models considered include Decision Trees, Random Forests and
Gradient Boosting. Cross validation, accuracy and sensitivity statistics
were used to select the best fitted model and it was concluded that a
random forest was the best approach to model this problem. The final
model used the carrier, departure hour, the month of year, the origin of
the flight and the destination it was headed as the best predictors of
the possibility of the flight being delayed by more than 20 mins. Delays
were predicted correctly at the rate of 76%. Thus, giving the customer a
fairly certain idea for the best choice of flight.

## Data Set

The original dataset which was a subset of a larger dataset consisted of
the following variables.

DayOfWeek - Monday to Sunday DayofMonth - specifies the day of the month
of the flight Month – specifies the month of year (January-December)
Year – The year during which the flight data was collected FlightNum –
The unique number allocated to aircrafts Distance – The distance which
the flight travelled in miles UniqueCarrier – The code of the airline
that owns the aircraft Origin – the city from which the flight takes of
Dest – the city to which the aircraft is headed DepDelay - difference in
mins between scheduled and actual departure time DepTime – actual
departure time

## Data Exploration

The first step in the analysis was exploring the data to identify the
variables relevant in the model fitting process. The observations were
filtered to remove irrelevant variables. Then the possible predictors
were individually assessed. Flight Numbers were dropped due to the
complicated nature of assigning flight codes which differs from carrier
to carrier and often allows for different flights to have the same
number (Rizzo, 2018).

![Figure 1: Total number of flights attributed to the different
carriers](/Users/srishtibhargava/Documents/Predicting-Flight-Delays/Rplot01.jpeg)

Next, the data was summarized on the basis of the Unique Carriers and a
very low proportion of flights were identified to belong to the carriers
“AQ”, HA” and “F9”. Thus, the data associated with these three
attributes was combined together to create a new category called
“Other”. Giving the relatively smaller carriers a larger share in the
predictive process.

![Figure 2: Average Delay Time for different
carriers](/Users/srishtibhargava/Documents/Predicting-Flight-Delays/Rplot02.jpeg)

Flight origin and destinations were then filtered to include the top 40
most popular destinations, this was done by summarizing the number of
flights per location of origin.

![Figure 3: Average Flight Delay Times plotted against the Departure
hour](/Users/srishtibhargava/Documents/Predicting-Flight-Delays/Rplot.jpeg)

The variable Departure Time was converted into Departure Hour as hourly
patterns were observed to be more interesting. The figure 2 indicates
that on an average, late-night flights have a higher average delay time
than early morning flights which seem to depart earlier than the
scheduled departure time.

## Methodology and Model Assessment

The original data was now reduced to 3.1million observations. The
variables left in the dataset included Departure Hour, Month, Day of
Month, Day of Week, Origin, Destination, Distance, Unique Carrier and
Delay Time. Delay time was then replaced with a binary variable that
took on the value 1 if the delay was more than 20 mins and 0 otherwise.

Each variable except the distance was fitted as a categorical variable.
The data was then split into training and test samples. The training set
consisted of 60% of the observations and the test set contained 40% of
the observations.

The aim was to build easily interpretable accurate models which predict
the probability of a delay greater than 20 minutes, but this was tricky
as the data set was extremely unbalanced. The number of flights that
were delayed more than 20 min made only made 17% of the data while the
number of flights that were not delayed made 83% of the data.

To deal with this problem, down sampling was used to train the models.
This randomly sampled equal data from both available classes to the
number of observations for the class with lower number of samples. The
models were then trained on this dataset which consisted of 636k
observations.

The models considered were Trees, Random Forests and Gradient Boosting.

Cross validation using the test data was used to calculate the confusion
matrix. This matrix is a summary of the predicted values against the
observed values. It gives the number of times the model predicted delay
when there was a 20min delay and not delayed when there wasn’t. The
confusion matrix was then used to calculate the accuracy, specificity
and sensitivity of the model. These measures were used for model
selection. In this case the relevant result is indicted to be a
successful prediction of delay and thus the focus is majorly on the
sensitivity statistic. For a brief explanation of the statistic
calculation refer to the table below. (Altman, D.G and, Bland, J.M.,
1994)

![Table 1: Confusion
Matrix](/Users/srishtibhargava/Documents/Predicting-Flight-Delays/Table.png)

The formulas used here are:  Sensitivity = D/(D+B) Specificity = A/(C+A)
Thus, sensitivity is the rate of true positives. Positive here meaning
that the flight was delayed.

### Trees

Trees are simple classifiers which work with recursive partitioning by
splitting the covariates into relevant categories, these are selected on
the basis of the importance of their contribution in determining the
response variable. At the root of this tree is the most important
predictor and then the leaves are determined on the basis of the
relative importance of each covariate.

The classification trees were fitted using the “rpart” package
(Atkinson, 2018) in R-Studio and manipulated using the parameter
complexity parameter which is used to access the model fit. The initial
tree used all available covariates to predict the delays with a
complexity parameter as 0.001. The tree had 67% accuracy and a 68%
sensitivity but was too complicated and thus pruning was considered. The
different trees tried are shown in the appendix under trees.

Manual pruning began with removing the covariates that did not
contribute too much to the most complex tree. These included the day of
month and day of week. The first pruned model removed distance as a
possible predictor, it increased that reduced the accuracy to 69% but
the ability to correctly identify delays increased to approximately 70%.
This tree was then pruned automatically on the basis of the best
complexity parameter as indicated in the figure 4 below, to remove any
other irrelevant covariates which were not used in the tree. That did
not have an effect on the result but helped retain only the relevant
predictors. The final tree is plotted in figure 5 below and used the
unique carrier, origin, destination, departure hour and month to
determine the possibility of a delay and is plotted below.

![Figure 4: Model selection criteria: Complexity parameter plotted
against the error rates according to the size of the
tree.](/Users/srishtibhargava/Documents/Predicting-Flight-Delays/CP-tree.jpeg)

![Figure 5: Final Tree: Each leaf indicates a split in the selected
predictors.](/Users/srishtibhargava/Documents/Predicting-Flight-Delays/Rplot04.jpeg)

### Random Forests

Random Forests are an extension of trees. They work by fitting multiple
trees on the training data by resampling the covariates at each nodes to
find the best model. The prediction at the end is done by taking the
average of the results from each tree that is the majority vote of all
the trees indicate decide the class prediction.

The random forest was fitted using the randomForest (Andy Liaw and
Matthew Wiener, 2002) package in R. The covariates used were the same as
the one picked out from the best tree. Each of them were important
predictors as seen below.

![Figure 6: Variable Importance in Random Forests: Variable importance
calculated on the basis of their contribution in decreasing the gini
index.](/Users/srishtibhargava/Documents/Predicting-Flight-Delays/Rplot05.jpeg)

The parameters that can be adjusted include the number of trees and the
mtry or the number of variables randomly sampled at each node. The
number of trees considered were 400 and the number of variables
resampled were 3. The results did not indicate a need for further
adjusting the parameters. The accuracy of the model as tested on train
data was 78% and the sensitivity was 76% making it the best model used
for delay prediction.

### Gradient Boosting

The R package “Caret” (Max Kuhn. Contributions from Jed Wing, Steve
Weston, Andre Williams, Chris Keefer, Allan Engelhardt, Tony Cooper,
Zachary Mayer, Brenton Kenkel, the R Core Team, Michael Benesty, Reynald
Lescarbeau, Andrew Ziem, Luca Scrucca, Yuan Tang, Can Candan and Tyler
Hunt, 2018) was used to fit stochastic boosting models. This method uses
decision trees and the Bernoulli function to fit multiple trees to the
same data. They work with probabilities and classify observations
according to the probability of a certain observation belonging to a
prediction class. Each tree is built by learning from the previous one
using a constant learning rate, by reclassification of observations
giving inaccurate predictions higher importance or weights.

For model fitting, an initial cross validation function was set up which
split the data into 2- folds. A few different predictors were tested but
the accuracy of the model with the same covariates as the best tree gave
the most accurate results. The covariates used were Month, Unique
Carrier, Destination, Origin and Departure Hour.

The parameters available to be adjusted by the caret package are the
number of trees, the learning rate, the depth of the trees and the
number of observations in each node for it to considered. The learning
rate or shrinkage was kept at a constant of 0.1 to avoid increase in
computational time and the observations per node were kept at 10.

The number of trees tested were 50,100,150 and 500. The depth of trees
was 1,2 and 3. The best model automatically selected by the package was
the one with 150 trees and of depth 3. This indicated a scope of
improvement and thus a model with depth 8 and number of trees 500 was
tested. Which did not improve the model sensitivity by too much thus the
simpler model was selected.

The comparison of the first few trees is done in the figure below. The
accuracy of the tree is determined by running a two-fold

![Figure 7: Model Accuracy – Accuracy plotted against boosting
iterations according to the tree
depth](/Users/srishtibhargava/Documents/Predicting-Flight-Delays/Boosting%20Comparison.jpeg)

The final model had 64% accuracy with a sensitivity rate of 72%. Thus,
accurately achieving the goal of predicting delays. A further
improvement would require more computational time and thus was not
considered.

## Conclusion

Each model was fairly accurate in predictions as seen by cross
validation accuracy and sensitivity. While some were computationally
expensive and thus not the best choice. The gradient boosting method
does the job but a slower rate than the random forest. While trees are
easy to interpret their accuracy is dependent on the data. Random
Forests add a stochastic element to the model generation process and
work on the knowledge of many to make prediction.

The best model is the Random Forest as it has the highest accuracy in
predicting a delay more than 20 mins if there is one. It is also
computationally cost saving as it is easy to fit and doesn’t require too
much time. The model is also fairly simple to understand as it uses the
vote of a majority to make predictions. The variables selected do not
require any external information other than knowing basic travelling
details. Thus, proving the client a fast solution to the problem of
deciding which flight to take to avoid any delays.

## References

• Bureau of Transportation Statistics. (2oo8). Bureau of Transportation
Statistics. Retrieve from transstats:
<https://www.transtats.bts.gov/Fields.asp?Table_ID=236>

• Rizzo, C. (2018, March). There’s a secret code behind flight numbers —
here’s how to tell what it means. Retrieved from Business Insider:
<https://www.businessinsider.com/what-does-flight-number-mean-2018-3?r=US&IR=T>

• Atkinson, T. T. (2018). rpart: Recursive Partitioning and Regression
Trees. Retrieved from <https://CRAN.R-project.org/package=rpart>

• Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams,
Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton
Kenkel, the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew
Ziem, Luca Scrucca, Yuan Tang, Can Candan and Tyler Hunt. (2018). caret:
Classification and Regression Training. R package version 6.0-81.
<https://CRAN.R-project.org/package=caret>

• Brandon Greenwell, Bradley Boehmke, Jay Cunningham and GBM Developers
(2019). gbm: Generalized Boosted Regression Models. R package version
2.1.5. <https://CRAN.R-project.org/package=gbm>

• A. Liaw and M. Wiener (2002). Classification and Regression by
randomForest. R News 2(3), 18–22.

• Altman, D.G., Bland, J.M. (1994) “Diagnostic tests 1: sensitivity and
specificity,” British Medical Journal, vol 308, 1552.

• Pandas, T., Python, T., Topic 2. Part 2. Overview of Seaborn, M.,
Topic 3. Classification, D., Squares, T., & Regression, T. et
al. (2019). Open Machine Learning Course mlcourse.ai. Retrieved
<https://mlcourse.ai/>

• Stack Overflow - Where Developers Learn, Share, & Build Careers.
(2019). Retrieved from <https://stackoverflow.com>

• G. Ridgeway (1999). “The state of boosting,” Computing Science and
Statistics 31:172-181.

• Kuhn, M. (2019). 7 train Models By Tag \| The caret Package. Retrieved
from
<http://topepo.github.io/caret/train-models-by-tag.html#Two_Class_Only>

• R Core Team (2018). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria. URL
<https://www.R-project.org/>.
