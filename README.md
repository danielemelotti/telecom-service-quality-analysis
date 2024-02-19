# Objective
The primary goal of this project is to analyze the service quality of *Atlas Communications*, a major telecommunications provider which has a monopoly on phone services in many areas of the US. The company claims that they take 7.6 minutes to repair phone services for its customers on average. We act as the New York *Public Utilities Commission* (PUC), which regularly monitors repair times with customers in New York to verify the quality of Atlas' services. Utilizing hypothesis testing and bootstrap methods, the project aims to verify the company's claim about their repair efficiency and provide a comprehensive understanding of the service quality as experienced by customers in New York.

# Dataset
The dataset consists of a recent sample of repair times collected by the New York PUC. It records the time taken by Atlas Communications to repair phone services for each customer in the sample.

# Tasks

## 1. Data Visualization and Hypothesis Formulation
* Visualize the distribution of Atlas' repair times, marking the mean with a vertical line.
* PUC checks: Formulate the null and alternative hypotheses for testing Atlas' claim about their average repair time.

## 2. Hypothesis Testing
* Calculate the estimated population mean and construct a 99% confidence interval for it.
* Perform a t-test to find the t-statistic and p-value, assessing the validity of Atlas' repair time claim.
* Interpret the results.

## 3. Bootstrap Analysis
* Bootstrapped Percentile: Estimate the 99% confidence interval of the average repair time
* Bootstrapped Difference of Means: Calculate the 99% confidence interval of the difference between the population mean and the hypothesized mean.
* Bootstrapped t-interval: Determine the 99% confidence interval of the bootstrapped t-statistic.
* Plot the distribution for each of the bootstrapped analyses.

## 4. Comments and Implications
* Do the t-test and the bootstrapping methods agree with each other on the outcome?
* Implications


