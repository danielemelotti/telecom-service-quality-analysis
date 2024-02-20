Atlas Telecommunications Service Quality Analysis
================

**Author:** Daniele Melotti<br> **Date:** Feb 2024

*See README for info about data and list of tasks*.

# 1. Data Visualization and Hypothesis Formulation

We start off by loading the sample data with the repair times of Atlas
Telecommunications and summarizing it:

``` r
atlas <- read.csv(file = "../data/atlascom.csv")

str(atlas)
```

    ## 'data.frame':    1687 obs. of  2 variables:
    ##  $ Time : num  17.5 2.4 0 0.65 22.23 ...
    ##  $ Group: chr  "ILEC" "ILEC" "ILEC" "ILEC" ...

``` r
summary(atlas)
```

    ##       Time            Group          
    ##  Min.   :  0.000   Length:1687       
    ##  1st Qu.:  0.750   Class :character  
    ##  Median :  3.630   Mode  :character  
    ##  Mean   :  8.522                     
    ##  3rd Qu.:  7.350                     
    ##  Max.   :191.600

We know that we are dealing with a dataframe containing two variables:

- a *Time* variable, which indicates the repair times,
- a *Group* variable, which indicates the carrier type; it can be either
  CLEC or ILEC (see [this
  link](https://www.tutorialspoint.com/difference-between-ilec-and-clec)
  for differences between CLEC and ILEC).

There are 1664 data entries, the mean value is 8.522 minutes, while the
median time is 3.63 minutes.

## Visualize the distribution of Atlas’ repair times, marking the mean with a vertical line

Let’s see how the repair times of Atlas telecommunications are
distributed:

``` r
# Taking into account only the Time variable
time <- atlas$Time

plot(density(time), main = "Atlas Repair Times", lwd = 2, col = "tomato")
abline(v = mean(time), lty = "dashed")
```

<img src="telecom-analysis_files/figure-gfm/atlas density-1.png" style="display: block; margin: auto;" />

The plot shows that most of the repair times are contained within 10
minutes or so, however, there are some scenarios in which the repair
times increase even up to 3 hours.

## PUC checks: Formulate the null and alternative hypotheses for testing Atlas’ claim about their average repair time

Let’s suppose that Atlas claims that it takes an average 7.6 minutes to
repair phone services its customers. Given that the New York Public
Utilities Commission (PUC) would like to check for any deviations from
such claim, we suppose a *two-tailed scenario*, where the null
hypothesis states that the average repair time equals 7.6 minutes, and
the alternative hypothesis states that the average repair time is
different from 7.6 minutes. Hence:

<div align="center">

![](https://latex.codecogs.com/gif.latex?H_0%3A%20%5Cmu%3D7.6)

</div>

<div align="center">

![](https://latex.codecogs.com/gif.latex?H_a%3A%20%5Cmu%5Cneq%207.6)

</div>

<br>

# 2. Hypothesis Testing

## Calculate the estimated population mean and construct a 99% confidence interval for it

Earlier we saw that the population mean is 8.522 minutes. We can use
this estimate to build a confidence interval:

``` r
# Computing mean, standard deviation and standard error
mean_time <- mean(time)
sd_time <- sd(time)
se <- sd_time/sqrt(length(time))

# 99% CI
mean_time + c(-2.58, 2.58) * se
```

    ## [1] 7.593073 9.450946

The interval indicates that we can be 99% confident that the mean repair
time lays between 7.59 and 9.45 minutes. This interval barely includes
the mean repair time of our null hypothesis.

## Perform a t-test to find the t-statistic and p-value, assessing the validity of Atlas’ repair time claim

The t-test is a perfect instrument for evaluating whether there is a
significant difference between Atlas’ claimed repair time and the actual
computed mean. A t-statistic is calculated by subtracting the null
hypothesis’ mean from the actual computed mean, and then dividing the
result by the standard error:

<div align="center">

![](https://latex.codecogs.com/gif.latex?t_%7Bstat%7D%3D%5Cfrac%7B%5Cmu_A-%5Cmu_0%7D%7Bse%7D)

</div>

Let’s calculate it on our own:

``` r
# Null hypothesis' mean 
mu_0 <- 7.6

# Computing the t-statistic
t_stat <- (mean_time - mu_0)/se
t_stat
```

    ## [1] 2.560762

We found a t-statistic equal to 2.56. Let’s also compute the
t-statistic’s p-value:

``` r
# Computing the degrees of freedom
df <- length(time) - 1

# Computing the p-value
p_value <- 1 - pt(t_stat, df)
p_value
```

    ## [1] 0.005265342

## Interpret the result

The t-statistic is the ratio of departure of the estimated value of the
mean from its hypothesized value to its standard error. If the
t-statistic falls out of the 99% CI range (-2.58, 2.58; 3 standard
deviations to the left of the mean, and 3 standard deviations to the
right) there is a likelihood that the null distribution of t (which is
centered on 0) is not the proper distribution for our data. Then, we’d
have to consider the alternative distribution, which shares the same
shape with the null distribution. Our t-statistic (2.56) is basically on
the left boundary of the 3-standard deviations interval, (-2.58, 2.58).

As the number of degrees of freedom is large, we can approximate the
t-distribution with a normal distribution. The p-value helps us
determine whether or not the t-statistic is statistically significant.
In our scenario, we are conducting a two-tailed test, which means that
the critical p-value must be halved (it becomes 0.005 for a 99%
confidence case). We saw that our p-value was 0.0053, hence slightly
bigger than the critical p-value, which according to theory means that
**we cannot reject the null hypothesis**, leaving us with a borderline
case. That would mean that the claimed repair time of Atlas is true if
we consider a 99% confidence, but given the borderline scenario we
encountered, this conclusion is debatable and shall be taken with a
pinch of salt.

# 3. Bootstrap Analysis

We have performed a traditional t-test which left us with a
non-rejection. Now, we are going to employ bootstrapping on three
statistics (means, difference of means, t-interval) and see whether all
methods share the same outcome as the traditional t-test.

## Bootstrapped Means: Estimate the 99% confidence interval of the average repair time

We start off with the bootstrapped means. We create a function that
takes a sample of the original data with replacement and calculates the
mean of the sample. Then, we use `replicate()` to repeat this process
2000 times, which grants us with 2000 means and allows us to compute a
99% confidence interval of the mean:

``` r
# 2000 bootstraps
boots <- 2000

# Bootstrapped means function
compute_sample_mean <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace = TRUE)
  mean(resample)
}

# Setting seed
set.seed(150)

# Computing the 99% CI of the mean
boot_99 <- replicate(boots, compute_sample_mean(time))
perc_99_CI <- quantile(boot_99, probs = c(0.025, 0.995))
perc_99_CI
```

    ##     2.5%    99.5% 
    ## 7.847522 9.440592

The interval above **does not include the hypothesized average repair
time**, which means that we can reject the null hypothesis and point out
that the average repair time is different from 7.6 minutes. Let’s see if
the next methods lead to the same conclusion.

## Bootstrapped Difference of Means: Calculate the 99% confidence interval of the difference between the population mean and the hypothesized mean

For the difference of means, we create a function with a similar
structure to the one in the section above; however, instead of computing
the mean, we subtract the hypothesized mean of 7.6 minutes from the
bootstrapped mean. Then, we use `replicate()` to repeat the process 2000
times and compute a 99% confidence interval of the differences:

``` r
# Bootstrapped differences function
boot_mean_diffs <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace = TRUE)
  return(mean(resample) - mean_hyp)
}

# Setting seed
set.seed(350)

# Computing the 99% CI of the differences
mean_diffs <- replicate(boots, boot_mean_diffs(time, mu_0))
diff_99_CI <- quantile(mean_diffs, probs = c(0.005, 0.995))
diff_99_CI
```

    ##       0.5%      99.5% 
    ## 0.07479911 1.88919864

As **zero is not included in the interval**, we can reject the null
hypothesis and say that the average repair time is different from 7.6
minutes. Indeed, if the claimed repair time was actual, there would not
be a significant difference between that time and the boostrapped means
(hence the difference would have been much closer to zero, and the
interval would have likely contained zero).

## Bootstrapped t-interval: Determine the 99% confidence interval of the bootstrapped t-statistic

Our last bootstrap sees the computation of multiple t-statistics after
the data is resampled. Again, we bootstrap 2000 times. The
`boot_t_stat()` function takes care of resampling the data, computing
the difference between the computed mean from the resampled data and
hypothesized mean, computing standard error and finally the t-statistic:

``` r
# Bootstrapped t-stat function
boot_t_stat <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace = TRUE)
  diff <- mean(resample) - mean_hyp
  se <- sd(resample)/sqrt(length(resample))
  return(diff/se)
}

# Setting seed
set.seed(450)

# Computing the 99% CI of the t-statistics
t_boots <- replicate(boots, boot_t_stat(time, mu_0))
t_99_CI <- quantile(t_boots, probs = c(0.005, 0.995))
t_99_CI
```

    ##       0.5%      99.5% 
    ## 0.02478382 4.57617000

Even in this case, **zero is not included in the interval**, which means
that we can reject the null hypothesis and say that the average repair
time is different from 7.6 minutes.

## Plot the distribution for each of the bootstrapped analyses

Let’s build visualizations for the three bootstrapped analyses we just
ran:

``` r
# Bootstrapped means
plot(density(boot_99), main = "Bootstrapped Means", ylab = "", xlab = "Minutes")
abline(v = mean(boot_99), lwd = 1.5, col = "tomato") # mean of bootstrapped means
abline(v = perc_99_CI, lwd = 1.5, col = "cornflowerblue") # confidence interval boundaries
abline(v = mu_0, lwd = 1.5, lty = "dashed", col = "purple") # 7.6 minutes claim
```

<img src="telecom-analysis_files/figure-gfm/boot means-1.png" style="display: block; margin: auto;" />

The plot confirms how the claimed average repair time is out of the 99%
confidence interval built from the bootstrapped means.

``` r
# Bootstrapped difference of means
plot(density(mean_diffs), main = "Bootstrapped Difference of Means", ylab = "", xlab = "Minutes")
abline(v = mean(mean_diffs), lwd = 1.5, col = "tomato") # mean
abline(v = diff_99_CI, lwd = 1.5, col = "cornflowerblue") # confidence interval boundaries
abline(v = 0, lwd = 1.5, lty = "dashed", col = "purple") # zero
```

<img src="telecom-analysis_files/figure-gfm/boot diffs-1.png" style="display: block; margin: auto;" />

Similarly, zero is not included in the 99% confidence interval of the
bootstrapped difference of means.

``` r
# Bootstrapped t-statistics
plot(density(t_boots), main = "Bootstrapped t-statistics", ylab = "", xlab = "Minutes")
abline(v = mean(t_boots), lwd = 1.5, col = "tomato") # mean of bootstrapped t-statistics
abline(v = t_99_CI, lwd = 1.5, col = "cornflowerblue") # confidence interval boundaries
abline(v = 0, lwd = 1.5, lty = "dashed", col = "purple") # zero
```

<img src="telecom-analysis_files/figure-gfm/boot t stats-1.png" style="display: block; margin: auto;" />

And finally, the last visualization also shows that zero is (barely) not
included in the confidence interval generated from bootstrapped
t-statistics.

# 4. Comments and Implications

## Do the t-test and the bootstrapping methods agree with each other on the outcome?

The traditional t-test leads to the conclusion that we cannot reject the
null hypothesis. According to the three bootstrap analyses, we can
reject the null hypothesis and conclude that the mean repair time of
Atlas is not equal to 7.6 minutes, even though it’s worth noting that
the results might have been lightly affected by the seed choice.

## Implications

The comprehensive analysis of Atlas Telecommunications’ service repair
times, encompassing both traditional statistical tests and advanced
bootstrapping methodologies, provides a nuanced understanding of the
company’s performance against its claims. While the t-test and
bootstrapped t-statistic present borderline results, indicating a close
call in directly supporting or refuting Atlas’ claims, the bootstrapped
analyses of means and difference of means offer a more decisive stance
on rejecting the null hypothesis. This suggests that, statistically,
Atlas’ actual repair times may not align with its claimed performance.

Considering bootstrap, each analysis we ran showed that the critical
value of interest always stood somewhere on the left tail of the
distributions, which means that Atlas’ repair times are likely higher
than the claimed 7.6 minutes on average. The company should consider the
following:

- Evaluate and enhance its service protocols and operational efficiency
  to better meet its service standards and customer expectations.
- Attempt to identify bottlenecks or inefficiencies contributing to
  extended repair times.
- Research about the factors influencing repair time variability.
- Engage with stakeholders to discuss the findings of the actions
  mentioned above and propose improvements.
