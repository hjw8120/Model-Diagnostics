HW 04: Model diagnostics
================
Hannah Wang
10 October 2019

``` r
library(tidyverse)
library(broom)
library(knitr) 
library(rms)
```

``` r
airbnb_mod <- read_csv("data/airbnb_mod.csv")
```

### Question 1

``` r
airbnb_mod <- airbnb_mod %>%
  mutate(log_price_3 = log(price_3_nights))
```

``` r
logprice_model <- lm(log_price_3 ~ prop_type_simp + number_of_reviews + review_scores_rating, data = airbnb_mod)
kable(tidy(logprice_model, conf.int = TRUE, level = 0.95),digits=5)
```

| term                        |  estimate | std.error | statistic | p.value |  conf.low | conf.high |
| :-------------------------- | --------: | --------: | --------: | ------: | --------: | --------: |
| (Intercept)                 |   5.24423 |   0.35894 |  14.61018 | 0.00000 |   4.54020 |   5.94825 |
| prop\_type\_simpGuest suite | \-0.20559 |   0.05052 | \-4.06934 | 0.00005 | \-0.30469 | \-0.10650 |
| prop\_type\_simpHouse       | \-0.05715 |   0.04040 | \-1.41479 | 0.15732 | \-0.13639 |   0.02208 |
| prop\_type\_simpOther       | \-0.03232 |   0.04546 | \-0.71083 | 0.47729 | \-0.12149 |   0.05685 |
| number\_of\_reviews         | \-0.00138 |   0.00019 | \-7.28060 | 0.00000 | \-0.00175 | \-0.00101 |
| review\_scores\_rating      |   0.00809 |   0.00368 |   2.19688 | 0.02817 |   0.00087 |   0.01531 |

logprice-hat = 5.244 - 0.206 \* prop\_type\_simpGuestsuite - 0.057 \*
prop\_type\_simpHouse - 0.032 \* prop\_type\_simpOther - 0.001 \*
number\_of\_reviews + 0.008 \* review\_scores\_rating

### Question 2

For every 1 point increase in average review score, we expect the median
total cost for 3 nights to be multiplied by a factor of exp(0.008) =
$1.008.

### Question 3

``` r
logprice_model <- lm(log_price_3 ~ prop_type_simp + number_of_reviews + review_scores_rating + room_type, data = airbnb_mod)
kable(tidy(logprice_model, conf.int = TRUE, level = 0.95),digits=5)
```

| term                        |  estimate | std.error |  statistic | p.value |  conf.low | conf.high |
| :-------------------------- | --------: | --------: | ---------: | ------: | --------: | --------: |
| (Intercept)                 |   5.51289 |   0.29892 |   18.44273 | 0.00000 |   4.92659 |   6.09919 |
| prop\_type\_simpGuest suite | \-0.14503 |   0.04201 |  \-3.45215 | 0.00057 | \-0.22743 | \-0.06263 |
| prop\_type\_simpHouse       |   0.27884 |   0.03574 |    7.80083 | 0.00000 |   0.20873 |   0.34895 |
| prop\_type\_simpOther       |   0.16683 |   0.03850 |    4.33351 | 0.00002 |   0.09132 |   0.24234 |
| number\_of\_reviews         | \-0.00120 |   0.00016 |  \-7.63618 | 0.00000 | \-0.00151 | \-0.00089 |
| review\_scores\_rating      |   0.00558 |   0.00307 |    1.81945 | 0.06902 | \-0.00044 |   0.01159 |
| room\_typePrivate room      | \-0.74494 |   0.02829 | \-26.33320 | 0.00000 | \-0.80042 | \-0.68945 |
| room\_typeShared room       | \-1.82881 |   0.18891 |  \-9.68082 | 0.00000 | \-2.19933 | \-1.45828 |

logprice-hat = 5.513 - 0.145 \* prop\_type\_simpGuestsuite + 0.279 \*
prop\_type\_simpHouse + 0.167 \* prop\_type\_simpOther - 0.001 \*
number\_of\_reviews + 0.006 \* review\_scores\_rating - 0.745 \*
room\_typePrivateroom - 1.829 \*
room\_typeSharedroom

### Question 4

``` r
ggplot(data = airbnb_mod, mapping = aes(x = room_type, y = log_price_3)) + geom_boxplot() + labs(title = "Distribution of Logprice for 3 Nights by Room Type", x = "Room Type", y = "Logprice")
```

![](hw-04-diagnostics_files/figure-gfm/plot-roomtype-1.png)<!-- -->
ANOVA F Test

H0: βentirehome=βprivate=βshared=0

Ha: at least one βj is not equal to
0

``` r
reduced <- lm(log_price_3 ~ prop_type_simp + number_of_reviews + review_scores_rating, data = airbnb_mod)

full <- lm(log_price_3 ~ prop_type_simp + number_of_reviews + review_scores_rating + room_type, data = airbnb_mod)

kable(anova(reduced, full), format="markdown", digits = 3)
```

| Res.Df |     RSS | Df | Sum of Sq |       F | Pr(\>F) |
| -----: | ------: | -: | --------: | ------: | ------: |
|   1670 | 592.556 | NA |        NA |      NA |      NA |
|   1668 | 408.084 |  2 |   184.472 | 377.006 |       0 |

At least one coeffecient associated with `room_type` is not 0.
Therefore, `room_type` is a significant predictor of cost for 3 nights.

### Question 5

### Question 6

### Question 7

### Question 8

### Overall (do not delete\!)
