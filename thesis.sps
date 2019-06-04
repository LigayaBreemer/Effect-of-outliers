* Encoding: UTF-8.
*Remember to adjust the file location in the syntax when opening the data.

PRESERVE.
SET DECIMAL DOT.

GET DATA  /TYPE=TXT
  /FILE="\\VUW\Personal$\Homes\17\s1782851\My Documents\results.txt"
  /ENCODING='UTF8'
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /DATATYPEMIN PERCENTAGE=95.0
  /VARIABLES=
  Condition AUTO
  Samp AUTO
  ES AUTO
  Outliers AUTO
  ErrorRMean AUTO
  ErrorRMedian AUTO
  ErrorWelch AUTO
  ErrorWilcoxon AUTO
  V1 AUTO
  V2 AUTO
  /MAP.
RESTORE.

CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.

*To create Figrue 1, which concerns Type I Error Rates, we use a filter to select the cases with an effect size of 0.
*We want to know the medians of the error rates for the description of our figures.

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(ES = 0).
VARIABLE LABELS filter_$ 'ES = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=ErrorRMean ErrorRMedian ErrorWelch ErrorWilcoxon
  /STATISTICS=MEDIAN
  /ORDER=ANALYSIS.

EXAMINE VARIABLES=ErrorRMean ErrorRMedian ErrorWelch ErrorWilcoxon 
  /COMPARE VARIABLE
  /PLOT=BOXPLOT
  /STATISTICS=NONE
  /NOTOTAL
  /MISSING=LISTWISE.

*To create Figure 2, which concerns Type II Error Rates, we use a filter to select the cases with an effect size of more than 0.

USE ALL.
COMPUTE filter_$=(ES  >  0).
VARIABLE LABELS filter_$ 'ES  >  0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=ErrorRMean ErrorRMedian ErrorWelch ErrorWilcoxon
  /STATISTICS=MEDIAN
  /ORDER=ANALYSIS.

EXAMINE VARIABLES=ErrorRMean ErrorRMedian ErrorWelch ErrorWilcoxon 
  /COMPARE VARIABLE
  /PLOT=BOXPLOT
  /STATISTICS=NONE
  /NOTOTAL
  /MISSING=LISTWISE.


