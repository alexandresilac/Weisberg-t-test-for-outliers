### Weisberg t-test for outliers ### Version 1

# Created by Alexandre Silva Lacerda
# https://orcid.org/0009-0000-2825-0288

# For citations, use:
# A. Lacerda, “Weisberg T-test for outliers,” Version 1, 2025. [Online]. Available at: https://github.com/alexandresilac/Weisberg-t-test-for-outliers

# based in the article:
# Demonstrating thr Consistency of Small Data Sets

# Seely, R.J. & Munyakazi, L. & Haury, J. & Simmerman, Heather & Rushing, W.H. & Curry, T.F.. (2003). 
# Demonstrating the consistency of small data sets: Application of the Weisberg t-test for outliers.16. 36-42+58. 

# Determining whether a data point is an "outlier" - a result that doesn't fit, that is too high or too low, 
# that is extreme or discordant - is difficult when using small data sets (such as the data from three, four, 
# or five conformance runs). The authors show that the Weisberg t-test is a powerful tool for detecting deviations 
# in small data sets.

###########################################################################################################

library(tcltk)

# Data insertion:

data <- data.frame(
  variable = c(1, 2, 5, 2, 10, 12, 25, 1, 10))                                  # Enter the data here.
                                                                                # The code works for up to 20 samples, 
                                                                                # but it is recommended to use up to 15.

# Ascending order

data_order <- data.frame(
  ascending_order = data[order(data$variable),])                                # ascending order


# Student's Critical values of T for one-tailed tests insertion 
# Degrees of freedom are n-2, the number of samples (for this case);

# Other cases can be observed in:
# https://www.scribbr.com/statistics/students-t-table/

critital_T_values <- data.frame(
  degrees_of_freedom = 3:18,
  `0.01` = c(4.541, 3.747, 3.365, 3.143, 2.998, 2.896, 2.821, 2.764,
             2.718, 2.681, 2.650, 2.624, 2.602, 2.583, 2.567, 2.552),
  `0.05` = c(2.353, 2.132, 2.015, 1.943, 1.895, 1.860, 1.833, 1.812,
             1.796, 1.782, 1.771, 1.761, 1.753, 1.746, 1.740, 1.734),
  `0.1`  = c(1.638, 1.533, 1.476, 1.440, 1.415, 1.397, 1.383, 1.372,
             1.363, 1.356, 1.350, 1.345, 1.341, 1.337, 1.333, 1.330))


###############################################################################
############################ Outliers test ####################################

highest_value <- data_order$ascending_order[nrow(data_order)]                   # selecting the highest value
lowest_value <- data_order$ascending_order[1]                                   # selecting the lowest value

mean_less_highest <- mean(data_order$ascending_order[-nrow(data_order)])        # mean -1 (highest)
mean_less_lowest <- mean(data_order$ascending_order[-1])                        # mean -1 (lowest)

stdev_less_highest <- sd(data_order$ascending_order[-nrow(data_order)])         # stdev -1 (highest)
stdev_less_lowest <- sd(data_order$ascending_order[-1])                         # stdev -1 (lowest)

tcalc_highest = abs((mean_less_highest-highest_value)/stdev_less_highest)       # Tcalc highest
tcalc_lowest = abs((mean_less_lowest-lowest_value)/stdev_less_lowest)           # Tcalc lowest

sqrroot = sqrt((nrow(data)-1)/nrow(data))                                       # sqr root data

weisberg_ttest_highest = sqrroot*tcalc_highest                                  # Weisberg ttest
weisberg_ttest_lowest = sqrroot*tcalc_lowest                                    # Weisberg ttest

tcrit <- critital_T_values$X0.05[nrow(data)-4]                                  # -4 because the critical t-value table starts at 3 degrees.
                                                                                # 95% confidence (0.05) was chosen, but 99% (0.01) and 90% (0.1) are available.

tkmessageBox(
  title = "Results",
  message = ifelse(weisberg_ttest_highest>tcrit,
                   "The highest value is an outlier",
                   "The highest value is not an outlier"),
                   
  icon = "info",
  type = "ok"
)

tkmessageBox(
  title = "Results",
  message = ifelse(weisberg_ttest_lowest>tcrit,
                   "The lowest value is an outlier",
                   "The lowest value is not an outlier"),
  
  icon = "info",
  type = "ok"
)





