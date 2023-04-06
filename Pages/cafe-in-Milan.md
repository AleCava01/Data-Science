t-Test for a cafe in Milan
================

Revenues from a well known cafe in Milan were collected ten days before
14/6/2021 (DD:MM:YYYY) and ten days after. 14/6/2021 was the day when
covid’s lockdown ended in Italy.

### Research output

|                      | Average measured revenue | Measured variance |
|----------------------|--------------------------|-------------------|
| **Before 14/6/2021** | 6616                     | 1881671           |
| **After 14/6/2021**  | 8384                     | 395448,9          |

``` r
#Data setup

n_1=10 # dimension of the 1st sample
n_2=10 # dimension of the 2nd sample

s_mean_1=6616 # sample mean 1
s_mean_2=8384 # sample mean 2

s_var_1=1881671 # sample variance 1
s_var_2=395448.9 # sample variance 2
```

### Question

Determine if there was a significative variation in the revenues after
the lockdown has been teared down.

Response format: \[statistic, p-value, decision\]

### Hypotesis

Populations:

- follow a Gaussian distribution

  $$
  Y_{11}....Y_{1n_1}\sim^{iid} N(\mu_1, \sigma_1^2)\\
  $$

  $$
  Y_{21}....Y_{2n_2}\sim^{iid} N(\mu_2, \sigma_2^2)
  $$

- have the same variance

  $$
  \sigma_1^2=\sigma_2^2=\sigma^2
  $$

- are independent to each other

## Setting up the test

From theory, the sample means follow a Gaussian distribution with
parameters given by the population mean and sample variance over sample
dimension.

$$
\bar Y_1\sim N\Big(\mu, \frac{s_1^2}{n_1}\Big)
$$

$$
\bar Y_2\sim N\Big(\mu, \frac{s_2^2}{n_2}\Big)
$$

In order to know if the means changed, we will consider the difference
between the sample means, that is distributed as follows

$$
\bar Y_1-\bar Y_2\sim N(\mu_1-\mu_2,\sigma^2\Big(\frac{1}{n_1}+\frac{1}{n_2}\Big))
$$

``` r
s_mean_diff = s_mean_1 - s_mean_2
s_mean_diff
```

    ## [1] -1768

To estimate the populations variance $\sigma^2$ we introduce the pooled
variance:

$$
s_{pooled}^2=\frac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}\approx\sigma^2
$$

``` r
s2_pooled = ((n_1-1)*s_var_1+(n_2-1)*s_var_2)/(n_1+n_2-2)
s2_pooled
```

    ## [1] 1138560

We now have everything to set up out test.

$$
T_0=\frac{\bar Y_1-\bar Y_2-0}{\sqrt{s_p^2\Big(\frac{1}{n_1}+\frac{1}{n_2}\Big)}}\sim t(n_1+n_2-2)
$$

``` r
t = abs(s_mean_diff)/sqrt(s2_pooled*(1/n_1 + 1/n_2))
t
```

    ## [1] 3.705009

We can calculate the p-value for this t-test as following
