APT Pork
================
Mykola Dereva
1/24/2022

``` r
rm(list = ls())
```

``` r
d <- readRDS("Data/clean_data.RDS")
```

# Modeling

``` r
library(apt)
```

    ## Warning: package 'lmtest' was built under R version 4.1.2

``` r
library(urca)
library(dplyr)

library(xtable)
```

There is a conflict with the dplyr lag() so I have to change it to the
default function, so the apt package works properly

``` r
lag <- stats::lag
```

``` r
product <- "Pork"

prod <- x <- d %>% 
  filter(Type == "Producer",
         Product == product,
         Date >= "2013-01-01") %>% 
  select(Price) %>% 
  log() %>% 
  ts(start = c(2013,1), frequency = 12)
  
cons <- y <- d %>% 
  filter(Type == "Consumer",
         Product == product,
         Date >= "2013-01-01") %>% 
  select(Price) %>% 
  log() %>% 
  ts(start = c(2013, 1), frequency = 12)

x.name <- "Producer"
y.name <- "Consumer"

table_path <- paste("tables/", product, "/", sep = "")
```

``` r
par(mfrow=c(2,1))
plot(x, main = x.name)
plot(y, main = y.name)
```

![](Pork-analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ca.jo(cbind(y,x), type = "eigen", ecdet = "none") %>% summary()
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: maximal eigenvalue statistic (lambda max) , with linear trend 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 0.15788898 0.01446446
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 1 |  1.54  6.50  8.18 11.65
    ## r = 0  | 18.22 12.91 14.90 19.19
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##           y.l2       x.l2
    ## y.l2  1.000000  1.0000000
    ## x.l2 -1.115394 -0.8513682
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##          y.l2        x.l2
    ## y.d 0.1436097 -0.03318863
    ## x.d 0.6203902 -0.04296867

``` r
# Two step  cointegration test

LR <- lm(y ~ x)
# residuals
res <- ts(residuals(LR), start=start(x), end=end(x), frequency = 12)

ur.df(y = res, type = "none",  selectlags = "AIC") %>% summary()
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression none 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.097668 -0.018827  0.002949  0.017225  0.073246 
    ## 
    ## Coefficients:
    ##            Estimate Std. Error t value Pr(>|t|)    
    ## z.lag.1    -0.61313    0.11421  -5.369 4.85e-07 ***
    ## z.diff.lag -0.07193    0.09969  -0.721    0.472    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02882 on 104 degrees of freedom
    ## Multiple R-squared:  0.3345, Adjusted R-squared:  0.3217 
    ## F-statistic: 26.14 on 2 and 104 DF,  p-value: 6.368e-10
    ## 
    ## 
    ## Value of test-statistic is: -5.3686 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau1 -2.58 -1.95 -1.62

``` r
# Test for structural brakes in TS
br.x <- ur.za(x, model = "both", lag = 1)
summary(br.x)
```

    ## 
    ## ################################ 
    ## # Zivot-Andrews Unit Root Test # 
    ## ################################ 
    ## 
    ## 
    ## Call:
    ## lm(formula = testmat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.094631 -0.024708 -0.002361  0.019071  0.125981 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.0388054  0.2229619   4.659 9.81e-06 ***
    ## y.l1         0.6920342  0.0666874  10.377  < 2e-16 ***
    ## trend        0.0034771  0.0008296   4.191 5.99e-05 ***
    ## y.dl1       -0.0106426  0.0932017  -0.114 0.909317    
    ## du           0.0742540  0.0217140   3.420 0.000909 ***
    ## dt          -0.0035206  0.0008228  -4.279 4.31e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03993 on 100 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.9828, Adjusted R-squared:  0.982 
    ## F-statistic:  1146 on 5 and 100 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Teststatistic: -4.6181 
    ## Critical values: 0.01= -5.57 0.05= -5.08 0.1= -4.82 
    ## 
    ## Potential break point at position: 53

``` r
plot(br.x)
```

![](Pork-analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
br.y <- ur.za(y, model = "both", lag = 1)
summary(br.y)
```

    ## 
    ## ################################ 
    ## # Zivot-Andrews Unit Root Test # 
    ## ################################ 
    ## 
    ## 
    ## Call:
    ## lm(formula = testmat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.040244 -0.014278 -0.002436  0.009919  0.077189 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.5764317  0.1237161   4.659 9.80e-06 ***
    ## y.l1         0.8506443  0.0324580  26.208  < 2e-16 ***
    ## trend        0.0018176  0.0004617   3.937 0.000153 ***
    ## y.dl1        0.3759452  0.0833098   4.513 1.75e-05 ***
    ## du           0.0377660  0.0106049   3.561 0.000567 ***
    ## dt          -0.0016222  0.0004085  -3.971 0.000135 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02176 on 100 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.9958, Adjusted R-squared:  0.9956 
    ## F-statistic:  4764 on 5 and 100 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Teststatistic: -4.6015 
    ## Critical values: 0.01= -5.57 0.05= -5.08 0.1= -4.82 
    ## 
    ## Potential break point at position: 51

``` r
plot(br.y)
```

![](Pork-analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
## convenience function to make DF stationarity test tables quickly 

get_df_test_result <- function(ts.name, ts.data, 
                            test.type = c("none", "drift", "trace")) {
  
  t <- ur.df(y = ts.data, type = test.type,  selectlags = "AIC")
  
  table <- cbind(ts.name,
                  t@model,
                  t@teststat %>% t() %>% round(2),
                  t@cval,
                  t@lags) %>% as.data.frame() %>% 
  tibble::rownames_to_column()
  
  colnames(table) <- c("Parameter", "Item", "Test Type", "Test Statistics",
                     "1pct", "5pct", "10pct", "Lag")
  
  table <- table %>% select(Item, `Test Type`, Parameter, everything())
  
  return(table)
  
}
```

``` r
LR <- lm(y ~ x)
# residuals
res <- ts(residuals(LR), start=start(x), end=end(x), frequency = 12)
```

``` r
stationarity.table <- 
  rbind(get_df_test_result("Consumer", cons, test.type = "none"),
      get_df_test_result("Consumer", cons, test.type = "drift"),
      get_df_test_result("Consumer", cons, test.type = "trend"),
      get_df_test_result("Producer", prod, test.type = "none"),
      get_df_test_result("Producer", prod, test.type = "drift"),
      get_df_test_result("Producer", prod, test.type = "trend"),
      get_df_test_result("Coinegration", res, test.type = "none"),
      get_df_test_result("Coinegration", res, test.type = "drift"),
      get_df_test_result("Coinegration", res, test.type = "trend")) 

stationarity.table
```

    ##            Item Test Type Parameter Test Statistics  1pct  5pct 10pct Lag
    ## 1      Consumer      none      tau1            2.03 -2.58 -1.95 -1.62   1
    ## 2      Consumer     drift      tau2            -1.5 -3.46 -2.88 -2.57   1
    ## 3      Consumer     drift      phi1            3.49  6.52  4.63  3.81   1
    ## 4      Consumer     trend      tau3           -1.48 -3.99 -3.43 -3.13   1
    ## 5      Consumer     trend      phi2            2.69  6.22  4.75  4.07   1
    ## 6      Consumer     trend      phi3            1.67  8.43  6.49  5.47   1
    ## 7      Producer      none      tau1            1.88 -2.58 -1.95 -1.62   1
    ## 8      Producer     drift      tau2           -1.61 -3.46 -2.88 -2.57   1
    ## 9      Producer     drift      phi1            3.36  6.52  4.63  3.81   1
    ## 10     Producer     trend      tau3           -1.42 -3.99 -3.43 -3.13   1
    ## 11     Producer     trend      phi2             2.5  6.22  4.75  4.07   1
    ## 12     Producer     trend      phi3            1.69  8.43  6.49  5.47   1
    ## 13 Coinegration      none      tau1           -5.37 -2.58 -1.95 -1.62   1
    ## 14 Coinegration     drift      tau2           -5.34 -3.46 -2.88 -2.57   1
    ## 15 Coinegration     drift      phi1           14.28  6.52  4.63  3.81   1
    ## 16 Coinegration     trend      tau3           -5.85 -3.99 -3.43 -3.13   1
    ## 17 Coinegration     trend      phi2           11.55  6.22  4.75  4.07   1
    ## 18 Coinegration     trend      phi3           17.33  8.43  6.49  5.47   1

``` r
stationarity.table %>% 
    xtable() %>% 
      print(type = "html",
            file = paste(table_path,
                         product, "_Stationarity_no_diff.doc", sep = ""))
```

Both variables not stationary together, or not coinegrated So we need to
make a first difference to stationarize them

## First diff

``` r
prod.diff <- y.diff <- diff(cons, differences = 1)
cons.diff <- x.diff <- diff(prod, differences = 1)
```

``` r
stationarity.table_diff <- 
  rbind(get_df_test_result("Consumer", cons.diff, test.type = "none"),
      get_df_test_result("Consumer", cons.diff, test.type = "drift"),
      get_df_test_result("Consumer", cons.diff, test.type = "trend"),
      get_df_test_result("Producer", prod.diff, test.type = "none"),
      get_df_test_result("Producer", prod.diff, test.type = "drift"),
      get_df_test_result("Producer", prod.diff, test.type = "trend"))

stationarity.table_diff
```

    ##        Item Test Type Parameter Test Statistics  1pct  5pct 10pct Lag
    ## 1  Consumer      none      tau1           -7.42 -2.58 -1.95 -1.62   1
    ## 2  Consumer     drift      tau2            -7.8 -3.46 -2.88 -2.57   1
    ## 3  Consumer     drift      phi1           30.44  6.52  4.63  3.81   1
    ## 4  Consumer     trend      tau3           -7.91 -3.99 -3.43 -3.13   1
    ## 5  Consumer     trend      phi2           20.88  6.22  4.75  4.07   1
    ## 6  Consumer     trend      phi3           31.32  8.43  6.49  5.47   1
    ## 7  Producer      none      tau1           -5.35 -2.58 -1.95 -1.62   1
    ## 8  Producer     drift      tau2            -5.9 -3.46 -2.88 -2.57   1
    ## 9  Producer     drift      phi1           17.41  6.52  4.63  3.81   1
    ## 10 Producer     trend      tau3           -6.02 -3.99 -3.43 -3.13   1
    ## 11 Producer     trend      phi2           12.13  6.22  4.75  4.07   1
    ## 12 Producer     trend      phi3            18.2  8.43  6.49  5.47   1

``` r
stationarity.table_diff %>% 
    xtable() %>% 
      print(type = "html",
            file = paste(table_path,
                         product, "_Stationarity_first_diff.doc",
                       sep = ""))
```

## structural brake

Test stationarity with exogenous structural break

``` r
t <- ur.za(y = cons, model = "both",  lag = 1)
```

``` r
summary(t)
```

    ## 
    ## ################################ 
    ## # Zivot-Andrews Unit Root Test # 
    ## ################################ 
    ## 
    ## 
    ## Call:
    ## lm(formula = testmat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.040244 -0.014278 -0.002436  0.009919  0.077189 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.5764317  0.1237161   4.659 9.80e-06 ***
    ## y.l1         0.8506443  0.0324580  26.208  < 2e-16 ***
    ## trend        0.0018176  0.0004617   3.937 0.000153 ***
    ## y.dl1        0.3759452  0.0833098   4.513 1.75e-05 ***
    ## du           0.0377660  0.0106049   3.561 0.000567 ***
    ## dt          -0.0016222  0.0004085  -3.971 0.000135 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02176 on 100 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.9958, Adjusted R-squared:  0.9956 
    ## F-statistic:  4764 on 5 and 100 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Teststatistic: -4.6015 
    ## Critical values: 0.01= -5.57 0.05= -5.08 0.1= -4.82 
    ## 
    ## Potential break point at position: 51

``` r
t <- ur.za(y = cons, model = "both",  lag = 1)

table <- cbind("Consumer",
                t@model,
                t@teststat %>% round(2),
                t@cval %>% t(),
                t@lag) %>% as.data.frame() 

colnames(table) <- c("Item", "Test Type", "Test Statistics",
                   "1pct", "5pct", "10pct", "Lag")

table <- table %>% select(Item, `Test Type`, everything())

table
```

    ##       Item Test Type Test Statistics  1pct  5pct 10pct Lag
    ## 1 Consumer      both            -4.6 -5.57 -5.08 -4.82   1

``` r
## convenience function to make Zivot and Andrews stationarity test tables quickly 

get_za_test_result <- function(ts.name, ts.data, 
                            model = c("intercept", "trend", "both"),
                            lag = 1) {
  
  t <- ur.za(y = ts.data, model = model,  lag = lag)

  table <- cbind(ts.name,
                  t@model,
                  t@teststat %>% round(2),
                  t@cval %>% t(),
                  t@lag) %>% as.data.frame() 
  
  colnames(table) <- c("Item", "Test Type", "Test Statistics",
                     "1pct", "5pct", "10pct", "Lag")
  
  table <- table %>% select(Item, `Test Type`, everything())
  
  return(table)
}
```

``` r
get_za_test_result(ts.name = "Consumer", ts.data = cons, model = "both")
```

    ##       Item Test Type Test Statistics  1pct  5pct 10pct Lag
    ## 1 Consumer      both            -4.6 -5.57 -5.08 -4.82   1

``` r
stationarity.table_za <- 
  rbind(
        get_za_test_result("Consumer", cons, model = "intercept"),
        get_za_test_result("Consumer", cons, model = "trend"),
        get_za_test_result("Consumer", cons, model = "both"),
        get_za_test_result("Producer", prod, model = "intercept"),
        get_za_test_result("Producer", prod, model = "trend"),
        get_za_test_result("Producer", prod, model = "both")
        )

stationarity.table_za
```

    ##       Item Test Type Test Statistics  1pct  5pct 10pct Lag
    ## 1 Consumer intercept           -2.79 -5.34  -4.8 -4.58   1
    ## 2 Consumer     trend            -3.6 -4.93 -4.42 -4.11   1
    ## 3 Consumer      both            -4.6 -5.57 -5.08 -4.82   1
    ## 4 Producer intercept           -2.54 -5.34  -4.8 -4.58   1
    ## 5 Producer     trend           -3.39 -4.93 -4.42 -4.11   1
    ## 6 Producer      both           -4.62 -5.57 -5.08 -4.82   1

``` r
stationarity.table_za %>% 
    xtable() %>% 
      print(type = "html",
            file = paste(table_path,
                         product, "_Stationarity_ZA.doc",
                       sep = ""))
```

## Perron Unit Root test

``` r
aTSA::pp.test(cons, lag.short = TRUE, type = "Z_rho")
```

    ## Phillips-Perron Unit Root Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##  lag Z_rho p.value
    ##    4 0.192   0.732
    ## ----- 
    ##  Type 2: with drift no trend 
    ##  lag Z_rho p.value
    ##    4 -1.38   0.837
    ## ----- 
    ##  Type 3: with drift and trend 
    ##  lag Z_rho p.value
    ##    4 -4.32   0.856
    ## --------------- 
    ## Note: p-value = 0.01 means p.value <= 0.01

``` r
aTSA::pp.test(cons.diff, lag.short = TRUE, type = "Z_rho")
```

    ## Phillips-Perron Unit Root Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##  lag Z_rho p.value
    ##    4  -121    0.01
    ## ----- 
    ##  Type 2: with drift no trend 
    ##  lag Z_rho p.value
    ##    4  -119    0.01
    ## ----- 
    ##  Type 3: with drift and trend 
    ##  lag Z_rho p.value
    ##    4  -119    0.01
    ## --------------- 
    ## Note: p-value = 0.01 means p.value <= 0.01

``` r
aTSA::pp.test(prod, lag.short = TRUE, type = "Z_rho")
```

    ## Phillips-Perron Unit Root Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##  lag Z_rho p.value
    ##    4 0.193   0.732
    ## ----- 
    ##  Type 2: with drift no trend 
    ##  lag Z_rho p.value
    ##    4    -2   0.766
    ## ----- 
    ##  Type 3: with drift and trend 
    ##  lag Z_rho p.value
    ##    4 -6.33   0.702
    ## --------------- 
    ## Note: p-value = 0.01 means p.value <= 0.01

``` r
aTSA::pp.test(prod.diff, lag.short = TRUE, type = "Z_rho")
```

    ## Phillips-Perron Unit Root Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##  lag Z_rho p.value
    ##    4 -58.8    0.01
    ## ----- 
    ##  Type 2: with drift no trend 
    ##  lag Z_rho p.value
    ##    4 -63.8    0.01
    ## ----- 
    ##  Type 3: with drift and trend 
    ##  lag Z_rho p.value
    ##    4   -64    0.01
    ## --------------- 
    ## Note: p-value = 0.01 means p.value <= 0.01

``` r
aTSA::pp.test(res, lag.short = F, type = "Z_rho")
```

    ## Phillips-Perron Unit Root Test 
    ## alternative: stationary 
    ##  
    ## Type 1: no drift no trend 
    ##  lag Z_rho p.value
    ##   12 -93.1    0.01
    ## ----- 
    ##  Type 2: with drift no trend 
    ##  lag Z_rho p.value
    ##   12   -93    0.01
    ## ----- 
    ##  Type 3: with drift and trend 
    ##  lag Z_rho p.value
    ##   12 -88.5    0.01
    ## --------------- 
    ## Note: p-value = 0.01 means p.value <= 0.01

# TAR + Cointegration

### TAR

``` r
t3 <- ciTarThd(y=y, x=x, model="tar", lag=0)
```

``` r
(th.tar <- t3$basic)
```

    ##           Item     tar
    ## 1          lag   0.000
    ## 2 thresh final   0.024
    ## 3 thresh range   0.150
    ## 4   sse.lowest   0.086
    ## 5    Total obs 108.000
    ## 6       CI obs 107.000
    ## 7    Lower obs  17.000
    ## 8    Upper obs  91.000

``` r
plot(t3)
```

![](Pork-analysis_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

Choose threshold

``` r
for (i in 1:12) { # 20 seconds
  t3a <- ciTarThd(y=y, x=x, model="tar", lag=i)
  th.tar[i+2] <- t3a$basic[,2]
}
```

``` r
th.tar
```

    ##           Item     tar      V3      V4      V5      V6      V7      V8      V9
    ## 1          lag   0.000   1.000   2.000   3.000   4.000   5.000   6.000   7.000
    ## 2 thresh final   0.024   0.024   0.024   0.024   0.024   0.024   0.024   0.024
    ## 3 thresh range   0.150   0.150   0.150   0.150   0.150   0.150   0.150   0.150
    ## 4   sse.lowest   0.086   0.085   0.084   0.082   0.081   0.081   0.080   0.075
    ## 5    Total obs 108.000 108.000 108.000 108.000 108.000 108.000 108.000 108.000
    ## 6       CI obs 107.000 106.000 105.000 104.000 103.000 102.000 101.000 100.000
    ## 7    Lower obs  17.000  16.000  16.000  16.000  16.000  16.000  16.000  15.000
    ## 8    Upper obs  91.000  91.000  90.000  89.000  88.000  87.000  86.000  85.000
    ##       V10     V11     V12     V13     V14
    ## 1   8.000   9.000  10.000  11.000  12.000
    ## 2   0.024   0.024  -0.036   0.020   0.020
    ## 3   0.150   0.150   0.150   0.150   0.150
    ## 4   0.075   0.074   0.074   0.073   0.071
    ## 5 108.000 108.000 108.000 108.000 108.000
    ## 6  99.000  98.000  97.000  96.000  95.000
    ## 7  15.000  15.000  15.000  15.000  15.000
    ## 8  85.000  84.000  83.000  82.000  81.000

### MTAR treshold

``` r
t4 <- ciTarThd(y=y, x=x, model="mtar", lag=0)
(th.mtar <- t4$basic)
```

    ##           Item    mtar
    ## 1          lag   0.000
    ## 2 thresh final  -0.018
    ## 3 thresh range   0.150
    ## 4   sse.lowest   0.083
    ## 5    Total obs 108.000
    ## 6       CI obs 106.000
    ## 7    Lower obs  16.000
    ## 8    Upper obs  91.000

``` r
plot(t4)
```

![](Pork-analysis_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
for (i in 1:12) {
  t4a <- ciTarThd(y=y, x=x, model="mtar", lag=i)
  th.mtar[i+2] <- t4a$basic[,2]
}
th.mtar
```

    ##           Item    mtar      V3      V4      V5      V6      V7      V8      V9
    ## 1          lag   0.000   1.000   2.000   3.000   4.000   5.000   6.000   7.000
    ## 2 thresh final  -0.018  -0.018  -0.014  -0.014  -0.014  -0.014  -0.014  -0.012
    ## 3 thresh range   0.150   0.150   0.150   0.150   0.150   0.150   0.150   0.150
    ## 4   sse.lowest   0.083   0.083   0.082   0.080   0.080   0.079   0.078   0.075
    ## 5    Total obs 108.000 108.000 108.000 108.000 108.000 108.000 108.000 108.000
    ## 6       CI obs 106.000 106.000 105.000 104.000 103.000 102.000 101.000 100.000
    ## 7    Lower obs  16.000  16.000  16.000  16.000  16.000  16.000  16.000  15.000
    ## 8    Upper obs  91.000  91.000  90.000  89.000  88.000  87.000  86.000  85.000
    ##       V10     V11     V12     V13     V14
    ## 1   8.000   9.000  10.000  11.000  12.000
    ## 2  -0.003  -0.003  -0.003  -0.012  -0.018
    ## 3   0.150   0.150   0.150   0.150   0.150
    ## 4   0.074   0.074   0.073   0.072   0.070
    ## 5 108.000 108.000 108.000 108.000 108.000
    ## 6  99.000  98.000  97.000  96.000  95.000
    ## 7  15.000  15.000  15.000  15.000  15.000
    ## 8  85.000  84.000  83.000  82.000  81.000

The following threshold values are specific to this data. They HAVE TO
be revised for another data set. Otherwise, various errors will occur.

``` r
(t.tar <- th.tar$V3[2])    #Best threshold for tar
```

    ## [1] 0.024

``` r
(t.mtar <- th.mtar$V3[2])
```

    ## [1] -0.018

## Lag selection

``` r
# max lag 
mx <- 12
```

### tar, thresold 0

``` r
g1 <- ciTarLag(y=y, x=x, model="tar", maxlag=mx, thresh = 0)
g1
```

    ##            Item    Value
    ## 1         model      tar
    ## 2       max lag       12
    ## 3     threshold        0
    ## 4 BestLag.byAic        0
    ## 5 BestLag.byBic        0
    ## 6      Best AIC -394.043
    ## 7      Best BIC -386.381

#### MTAR, threshlod 0

``` r
(g2 <-ciTarLag(y=y, x=x, model="mtar",maxlag=mx, thresh=0))
```

    ##            Item    Value
    ## 1         model     mtar
    ## 2       max lag       12
    ## 3     threshold        0
    ## 4 BestLag.byAic        0
    ## 5 BestLag.byBic        0
    ## 6      Best AIC -395.779
    ## 7      Best BIC -388.117

``` r
plot(g1)
```

![](Pork-analysis_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
plot(g2)
```

![](Pork-analysis_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
(g3 <-ciTarLag(y=y, x=x, model="tar", maxlag=mx, thresh=t.tar))
```

    ##            Item    Value
    ## 1         model      tar
    ## 2       max lag       12
    ## 3     threshold    0.024
    ## 4 BestLag.byAic        0
    ## 5 BestLag.byBic        0
    ## 6      Best AIC -394.877
    ## 7      Best BIC -387.216

``` r
(g4 <-ciTarLag(y=y, x=x, model="mtar",maxlag=mx, thresh=t.mtar))
```

    ##            Item    Value
    ## 1         model     mtar
    ## 2       max lag       12
    ## 3     threshold   -0.018
    ## 4 BestLag.byAic        0
    ## 5 BestLag.byBic        0
    ## 6      Best AIC -399.092
    ## 7      Best BIC  -391.43

# Table 3 Results of EG and threshold cointegration tests

Choose Lag

``` r
vv <- 0
```

``` r
(f1 <- ciTarFit(y=y, x=x, model="tar", lag=vv, thresh=0))
```

    ## === Long Run Regression
    ## 
    ## Call:
    ## lm(formula = formula.LR, data = data.LR)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.088887 -0.021688 -0.003014  0.021998  0.084789 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.156850   0.038566   4.067 9.17e-05 ***
    ## x           1.095488   0.009783 111.981  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03075 on 106 degrees of freedom
    ## Multiple R-squared:  0.9916, Adjusted R-squared:  0.9915 
    ## F-statistic: 1.254e+04 on 1 and 106 DF,  p-value: < 2.2e-16
    ## 
    ## === Threshold Cointegration Regression
    ## 
    ## Call:
    ## lm(formula = diff.resid.t_0 ~ 0 + ., data = data.CI)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.09627 -0.02303  0.00072  0.01607  0.07020 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## pos.resid.t_1  -0.6165     0.1342  -4.593 1.22e-05 ***
    ## neg.resid.t_1  -0.6872     0.1259  -5.456 3.26e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02888 on 105 degrees of freedom
    ## Multiple R-squared:  0.3263, Adjusted R-squared:  0.3135 
    ## F-statistic: 25.43 on 2 and 105 DF,  p-value: 9.842e-10
    ## 
    ## === H1: No cointegration b/w two variables
    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## pos.resid.t_1 = 0
    ## neg.resid.t_1 = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: diff.resid.t_0 ~ 0 + (pos.resid.t_1 + neg.resid.t_1)
    ## 
    ##   Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    107 0.129956                                  
    ## 2    105 0.087546  2   0.04241 25.433 9.842e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## === H2: Symmetric adjustment in the long run
    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## pos.resid.t_1 - neg.resid.t_1 = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: diff.resid.t_0 ~ 0 + (pos.resid.t_1 + neg.resid.t_1)
    ## 
    ##   Res.Df      RSS Df  Sum of Sq      F Pr(>F)
    ## 1    106 0.087669                            
    ## 2    105 0.087546  1 0.00012301 0.1475 0.7017

``` r
(f2 <- ciTarFit(y=y, x=x, model="tar", lag=vv, thresh=t.tar ))
```

    ## === Long Run Regression
    ## 
    ## Call:
    ## lm(formula = formula.LR, data = data.LR)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.088887 -0.021688 -0.003014  0.021998  0.084789 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.156850   0.038566   4.067 9.17e-05 ***
    ## x           1.095488   0.009783 111.981  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03075 on 106 degrees of freedom
    ## Multiple R-squared:  0.9916, Adjusted R-squared:  0.9915 
    ## F-statistic: 1.254e+04 on 1 and 106 DF,  p-value: < 2.2e-16
    ## 
    ## === Threshold Cointegration Regression
    ## 
    ## Call:
    ## lm(formula = diff.resid.t_0 ~ 0 + ., data = data.CI)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.094064 -0.024196  0.001516  0.015802  0.067515 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## pos.resid.t_1  -0.5461     0.1418  -3.851 0.000203 ***
    ## neg.resid.t_1  -0.7311     0.1197  -6.107 1.74e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02876 on 105 degrees of freedom
    ## Multiple R-squared:  0.3317, Adjusted R-squared:  0.319 
    ## F-statistic: 26.06 on 2 and 105 DF,  p-value: 6.458e-10
    ## 
    ## === H1: No cointegration b/w two variables
    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## pos.resid.t_1 = 0
    ## neg.resid.t_1 = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: diff.resid.t_0 ~ 0 + (pos.resid.t_1 + neg.resid.t_1)
    ## 
    ##   Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    107 0.129956                                  
    ## 2    105 0.086846  2   0.04311 26.061 6.458e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## === H2: Symmetric adjustment in the long run
    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## pos.resid.t_1 - neg.resid.t_1 = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: diff.resid.t_0 ~ 0 + (pos.resid.t_1 + neg.resid.t_1)
    ## 
    ##   Res.Df      RSS Df  Sum of Sq      F Pr(>F)
    ## 1    106 0.087669                            
    ## 2    105 0.086846  1 0.00082264 0.9946 0.3209

``` r
(f3 <- ciTarFit(y=y, x=x, model="mtar", lag=vv, thresh=0 ))
```

    ## === Long Run Regression
    ## 
    ## Call:
    ## lm(formula = formula.LR, data = data.LR)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.088887 -0.021688 -0.003014  0.021998  0.084789 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.156850   0.038566   4.067 9.17e-05 ***
    ## x           1.095488   0.009783 111.981  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03075 on 106 degrees of freedom
    ## Multiple R-squared:  0.9916, Adjusted R-squared:  0.9915 
    ## F-statistic: 1.254e+04 on 1 and 106 DF,  p-value: < 2.2e-16
    ## 
    ## === Threshold Cointegration Regression
    ## 
    ## Call:
    ## lm(formula = diff.resid.t_0 ~ 0 + ., data = data.CI)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.093331 -0.025327  0.001313  0.015788  0.067242 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## pos.resid.t_1  -0.5389     0.1342  -4.017 0.000112 ***
    ## neg.resid.t_1  -0.7692     0.1253  -6.141 1.52e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02868 on 104 degrees of freedom
    ## Multiple R-squared:  0.3411, Adjusted R-squared:  0.3285 
    ## F-statistic: 26.92 on 2 and 104 DF,  p-value: 3.778e-10
    ## 
    ## === H1: No cointegration b/w two variables
    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## pos.resid.t_1 = 0
    ## neg.resid.t_1 = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: diff.resid.t_0 ~ 0 + (pos.resid.t_1 + neg.resid.t_1)
    ## 
    ##   Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    106 0.129828                                  
    ## 2    104 0.085538  2  0.044289 26.924 3.778e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## === H2: Symmetric adjustment in the long run
    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## pos.resid.t_1 - neg.resid.t_1 = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: diff.resid.t_0 ~ 0 + (pos.resid.t_1 + neg.resid.t_1)
    ## 
    ##   Res.Df      RSS Df Sum of Sq      F Pr(>F)
    ## 1    105 0.086834                           
    ## 2    104 0.085538  1 0.0012956 1.5752 0.2123

``` r
(f4 <- ciTarFit(y=y, x=x, model="mtar", lag=vv, thresh=t.mtar))
```

    ## === Long Run Regression
    ## 
    ## Call:
    ## lm(formula = formula.LR, data = data.LR)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.088887 -0.021688 -0.003014  0.021998  0.084789 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.156850   0.038566   4.067 9.17e-05 ***
    ## x           1.095488   0.009783 111.981  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03075 on 106 degrees of freedom
    ## Multiple R-squared:  0.9916, Adjusted R-squared:  0.9915 
    ## F-statistic: 1.254e+04 on 1 and 106 DF,  p-value: < 2.2e-16
    ## 
    ## === Threshold Cointegration Regression
    ## 
    ## Call:
    ## lm(formula = diff.resid.t_0 ~ 0 + ., data = data.CI)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.090159 -0.026086 -0.000601  0.014910  0.066448 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## pos.resid.t_1  -0.5180     0.1115  -4.645 9.99e-06 ***
    ## neg.resid.t_1  -0.9340     0.1533  -6.092 1.90e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02825 on 104 degrees of freedom
    ## Multiple R-squared:  0.3608, Adjusted R-squared:  0.3485 
    ## F-statistic: 29.35 on 2 and 104 DF,  p-value: 7.848e-11
    ## 
    ## === H1: No cointegration b/w two variables
    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## pos.resid.t_1 = 0
    ## neg.resid.t_1 = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: diff.resid.t_0 ~ 0 + (pos.resid.t_1 + neg.resid.t_1)
    ## 
    ##   Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    106 0.129828                                  
    ## 2    104 0.082992  2  0.046836 29.346 7.848e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## === H2: Symmetric adjustment in the long run
    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## pos.resid.t_1 - neg.resid.t_1 = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: diff.resid.t_0 ~ 0 + (pos.resid.t_1 + neg.resid.t_1)
    ## 
    ##   Res.Df      RSS Df Sum of Sq      F  Pr(>F)  
    ## 1    105 0.086834                              
    ## 2    104 0.082992  1 0.0038421 4.8146 0.03044 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### summary table

``` r
r0 <- cbind(summary(f1)$dia, summary(f2)$dia,
            summary(f3)$dia, summary(f4)$dia)

diag <- r0[c(1:4, 6:7, 12:14, 8, 10, 9, 11), c(1,2,4,6,8)]

rownames(diag) <- 1:nrow(diag)
diag
```

    ##           item      tar    c.tar     mtar   c.mtar
    ## 1          lag    0.000    0.000    0.000    0.000
    ## 2       thresh    0.000    0.024    0.000   -0.018
    ## 3    total obs  108.000  108.000  108.000  108.000
    ## 4    coint obs  107.000  107.000  106.000  106.000
    ## 5          aic -450.948 -451.807 -448.141 -451.345
    ## 6          bic -442.930 -443.788 -440.151 -443.355
    ## 7   LB test(4)    0.374    0.313    0.285    0.240
    ## 8   LB test(8)    0.445    0.349    0.300    0.199
    ## 9  LB test(12)    0.245    0.192    0.202    0.151
    ## 10   H1: no CI   25.433   26.061   26.924   29.346
    ## 11 H1: p.value    0.000    0.000    0.000    0.000
    ## 12  H2: no APT    0.148    0.995    1.575    4.815
    ## 13 H2: p.value    0.702    0.321    0.212    0.030

``` r
e1 <- summary(f1)$out
e2 <- summary(f2)$out
e3 <- summary(f3)$out
e4 <- summary(f4)$out
```

``` r
ee <- list(e1, e2, e3, e4)
vect <- NULL


for (i in 1:4) {
  ef <- data.frame(ee[i])
  vect2 <- c(paste(ef[3, "estimate"], ef[3, "sign"], sep=""),
  paste("(", ef[3, "t.value"], ")", sep=""),
  paste(ef[4, "estimate"], ef[4, "sign"], sep=""),
  paste("(", ef[4, "t.value"], ")", sep=""))
  vect <- cbind(vect, vect2)
}
```

``` r
item <- c("pos.coeff","pos.t.value", "neg.coeff","neg.t.value")
ve <- data.frame(cbind(item, vect))
colnames(ve) <- colnames(diag)
( res.CI <- rbind(diag, ve)[c(1:2, 14:17, 3:13), ] )
```

    ##           item       tar     c.tar      mtar    c.mtar
    ## 1          lag         0         0         0         0
    ## 2       thresh         0     0.024         0    -0.018
    ## 14   pos.coeff -0.617*** -0.546*** -0.539*** -0.518***
    ## 15 pos.t.value  (-4.593)  (-3.851)  (-4.017)  (-4.645)
    ## 16   neg.coeff -0.687*** -0.731*** -0.769*** -0.934***
    ## 17 neg.t.value  (-5.456)  (-6.107)  (-6.141)  (-6.092)
    ## 3    total obs       108       108       108       108
    ## 4    coint obs       107       107       106       106
    ## 5          aic  -450.948  -451.807  -448.141  -451.345
    ## 6          bic   -442.93  -443.788  -440.151  -443.355
    ## 7   LB test(4)     0.374     0.313     0.285      0.24
    ## 8   LB test(8)     0.445     0.349       0.3     0.199
    ## 9  LB test(12)     0.245     0.192     0.202     0.151
    ## 10   H1: no CI    25.433    26.061    26.924    29.346
    ## 11 H1: p.value         0         0         0         0
    ## 12  H2: no APT     0.148     0.995     1.575     4.815
    ## 13 H2: p.value     0.702     0.321     0.212      0.03

``` r
rownames(res.CI) <- 1:nrow(res.CI)

res.CI
```

    ##           item       tar     c.tar      mtar    c.mtar
    ## 1          lag         0         0         0         0
    ## 2       thresh         0     0.024         0    -0.018
    ## 3    pos.coeff -0.617*** -0.546*** -0.539*** -0.518***
    ## 4  pos.t.value  (-4.593)  (-3.851)  (-4.017)  (-4.645)
    ## 5    neg.coeff -0.687*** -0.731*** -0.769*** -0.934***
    ## 6  neg.t.value  (-5.456)  (-6.107)  (-6.141)  (-6.092)
    ## 7    total obs       108       108       108       108
    ## 8    coint obs       107       107       106       106
    ## 9          aic  -450.948  -451.807  -448.141  -451.345
    ## 10         bic   -442.93  -443.788  -440.151  -443.355
    ## 11  LB test(4)     0.374     0.313     0.285      0.24
    ## 12  LB test(8)     0.445     0.349       0.3     0.199
    ## 13 LB test(12)     0.245     0.192     0.202     0.151
    ## 14   H1: no CI    25.433    26.061    26.924    29.346
    ## 15 H1: p.value         0         0         0         0
    ## 16  H2: no APT     0.148     0.995     1.575     4.815
    ## 17 H2: p.value     0.702     0.321     0.212      0.03

### save summary table

``` r
table <- xtable(res.CI)
print(table, type = "html",
      file = paste(table_path,
                   product, ".doc", sep = ""))
```

# APT + ECM

``` r
(sem <- ecmSymFit(y=prod, x=cons, lag=1))  
```

    ## 
    ## ===============================================================
    ## ECM - Symmetric + linear cointegration -  "cons"
    ## ===============================================================
    ## 
    ## Call:
    ## lm(formula = DepVar.x ~ 1 + X.)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.042563 -0.013085 -0.003226  0.007581  0.075609 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.005406   0.002353   2.298 0.023602 *  
    ## X.diff.cons.t_1  0.138933   0.118425   1.173 0.243460    
    ## X.diff.prod.t_1  0.300139   0.086995   3.450 0.000816 ***
    ## X.ECT.t_1       -0.142021   0.116677  -1.217 0.226330    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02268 on 102 degrees of freedom
    ## Multiple R-squared:  0.2618, Adjusted R-squared:  0.2401 
    ## F-statistic: 12.06 on 3 and 102 DF,  p-value: 8.081e-07
    ## 
    ## 
    ## ===============================================================
    ## ECM - Symmetric + linear cointegration -  "prod"
    ## ===============================================================
    ## 
    ## Call:
    ## lm(formula = DepVar.y ~ 1 + X.)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.101594 -0.027730 -0.004167  0.026186  0.151068 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)      0.006427   0.004367   1.472  0.14421   
    ## X.diff.cons.t_1  0.061883   0.219829   0.282  0.77889   
    ## X.diff.prod.t_1  0.188355   0.161486   1.166  0.24618   
    ## X.ECT.t_1       -0.673165   0.216583  -3.108  0.00244 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0421 on 102 degrees of freedom
    ## Multiple R-squared:  0.09521,    Adjusted R-squared:  0.0686 
    ## F-statistic: 3.578 on 3 and 102 DF,  p-value: 0.01652

``` r
summary(sem)
```

    ##           DepVar          IndVar   estimate error t.value p.value signif
    ## 1 diff.cons.t_0  |     (Intercept)    0.005 0.002   2.298   0.024     **
    ## 2                | X.diff.cons.t_1    0.139 0.118   1.173   0.243       
    ## 3                | X.diff.prod.t_1    0.300 0.087   3.450   0.001    ***
    ## 4                |       X.ECT.t_1   -0.142 0.117  -1.217   0.226       
    ## 5 diff.prod.t_0  -     (Intercept)    0.006 0.004   1.472   0.144      .
    ## 6                - X.diff.cons.t_1    0.062 0.220   0.282   0.779       
    ## 7                - X.diff.prod.t_1    0.188 0.161   1.166   0.246       
    ## 8                -       X.ECT.t_1   -0.673 0.217  -3.108   0.002    ***

``` r
ccc <- summary(sem)

tr <- nrow(ccc)
ce <- tr / 2

coe <- cbind(as.character(ccc[1:ce, 2]),
    paste(ccc[1:ce, "estimate"], ccc$signif[1:ce], sep=""), ccc[1:ce, "t.value"],
    paste(ccc[(ce+1):tr,"estimate"], ccc$signif[(ce+1):tr],sep=""), ccc[(ce+1):tr,"t.value"])

colnames(coe) <- c("item",
                   paste(x.name, ".est", sep = ""),
                   paste(x.name, ".t", sep = ""),
                   paste(y.name, ".est", sep = ""),
                   paste(y.name, ".t", sep = "")
                   )

coe %>% data.frame()
```

    ##              item Producer.est Producer.t Consumer.est Consumer.t
    ## 1     (Intercept)      0.005**      2.298       0.006.      1.472
    ## 2 X.diff.cons.t_1       0.139       1.173       0.062       0.282
    ## 3 X.diff.prod.t_1       0.3***       3.45       0.188       1.166
    ## 4       X.ECT.t_1      -0.142      -1.217    -0.673***     -3.108

``` r
edia <- ecmDiag(sem, 3)
ed <- edia[c(1,6,7,8,9, 10), ]
ed2 <- cbind(ed[,1:2], "_", ed[,3], "_")
colnames(ed2) <- colnames(coe)
ed2
```

    ##         item Producer.est Producer.t Consumer.est Consumer.t
    ## 1  R-squared        0.262          _        0.095          _
    ## 6        AIC     -495.949          _     -364.812          _
    ## 7        BIC     -482.632          _     -351.495          _
    ## 8      LB(4)        0.508          _        0.605          _
    ## 9      LB(8)        0.568          _        0.700          _
    ## 10    LB(12)        0.430          _        0.314          _

``` r
table.1.1 <- data.frame(rbind(coe, ed2))

table.1.1
```

    ##               item Producer.est Producer.t Consumer.est Consumer.t
    ## 1      (Intercept)      0.005**      2.298       0.006.      1.472
    ## 2  X.diff.cons.t_1       0.139       1.173       0.062       0.282
    ## 3  X.diff.prod.t_1       0.3***       3.45       0.188       1.166
    ## 4        X.ECT.t_1      -0.142      -1.217    -0.673***     -3.108
    ## 11       R-squared        0.262          _        0.095          _
    ## 6              AIC     -495.949          _     -364.812          _
    ## 7              BIC     -482.632          _     -351.495          _
    ## 8            LB(4)        0.508          _        0.605          _
    ## 9            LB(8)        0.568          _          0.7          _
    ## 10          LB(12)         0.43          _        0.314          _

``` r
table.1.1 %>%  
  xtable() %>% 
  print(type = "html",
      file = paste(table_path, product, "_Symetric_ECM.doc", sep = ""))
```

# Fitting Assymetric ECM

``` r
aem <- ecmAsyFit(y=prod, x=cons, lag=1, model="tar",
                 split=TRUE, thresh=t.tar)
aem
```

    ## 
    ## ===============================================================
    ## ECM - Asymmetric + nonlinear threshold cointegration -  "cons"
    ## ===============================================================
    ## 
    ## Call:
    ## lm(formula = DepVar.x ~ 1 + X.)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.040133 -0.014315 -0.003443  0.007377  0.070735 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)          0.006312   0.003747   1.685  0.09518 . 
    ## X.diff.cons.t_1.pos  0.302501   0.164386   1.840  0.06874 . 
    ## X.diff.cons.t_1.neg -0.170968   0.289938  -0.590  0.55675   
    ## X.diff.prod.t_1.pos  0.192627   0.117254   1.643  0.10359   
    ## X.diff.prod.t_1.neg  0.490190   0.163046   3.006  0.00335 **
    ## X.ECT.t_1.pos       -0.191729   0.174935  -1.096  0.27574   
    ## X.ECT.t_1.neg       -0.137109   0.156636  -0.875  0.38351   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0227 on 99 degrees of freedom
    ## Multiple R-squared:  0.2823, Adjusted R-squared:  0.2388 
    ## F-statistic: 6.489 on 6 and 99 DF,  p-value: 8.501e-06
    ## 
    ## 
    ## ===============================================================
    ## ECM - Asymmetric + nonlinear threshold cointegration -  "prod"
    ## ===============================================================
    ## 
    ## Call:
    ## lm(formula = DepVar.y ~ 1 + X.)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.088324 -0.025890 -0.007058  0.025849  0.151020 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)          0.006307   0.006888   0.916   0.3621  
    ## X.diff.cons.t_1.pos  0.483283   0.302224   1.599   0.1130  
    ## X.diff.cons.t_1.neg -0.794718   0.533051  -1.491   0.1392  
    ## X.diff.prod.t_1.pos -0.078070   0.215571  -0.362   0.7180  
    ## X.diff.prod.t_1.neg  0.631921   0.299761   2.108   0.0375 *
    ## X.ECT.t_1.pos       -0.703113   0.321619  -2.186   0.0312 *
    ## X.ECT.t_1.neg       -0.719296   0.287975  -2.498   0.0141 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04173 on 99 degrees of freedom
    ## Multiple R-squared:  0.1371, Adjusted R-squared:  0.08478 
    ## F-statistic: 2.621 on 6 and 99 DF,  p-value: 0.02117

``` r
summary(aem)
```

    ##            DepVar              IndVar   estimate error t.value p.value signif
    ## 1  diff.cons.t_0  |         (Intercept)    0.006 0.004   1.685   0.095      *
    ## 2                 | X.diff.cons.t_1.pos    0.303 0.164   1.840   0.069      *
    ## 3                 | X.diff.cons.t_1.neg   -0.171 0.290  -0.590   0.557       
    ## 4                 | X.diff.prod.t_1.pos    0.193 0.117   1.643   0.104      .
    ## 5                 | X.diff.prod.t_1.neg    0.490 0.163   3.006   0.003    ***
    ## 6                 |       X.ECT.t_1.pos   -0.192 0.175  -1.096   0.276       
    ## 7                 |       X.ECT.t_1.neg   -0.137 0.157  -0.875   0.384       
    ## 8  diff.prod.t_0  -         (Intercept)    0.006 0.007   0.916   0.362       
    ## 9                 - X.diff.cons.t_1.pos    0.483 0.302   1.599   0.113      .
    ## 10                - X.diff.cons.t_1.neg   -0.795 0.533  -1.491   0.139      .
    ## 11                - X.diff.prod.t_1.pos   -0.078 0.216  -0.362   0.718       
    ## 12                - X.diff.prod.t_1.neg    0.632 0.300   2.108   0.038     **
    ## 13                -       X.ECT.t_1.pos   -0.703 0.322  -2.186   0.031     **
    ## 14                -       X.ECT.t_1.neg   -0.719 0.288  -2.498   0.014     **

``` r
ccc <- summary(aem)
tr <- nrow(ccc)
ce <- tr / 2



coe <- cbind(as.character(ccc[1:ce, 2]),
    paste(ccc[1:ce, "estimate"], ccc$signif[1:ce], sep=""),
    ccc[1:ce, "p.value"],
    paste(ccc[(ce+1):tr,"estimate"], ccc$signif[(ce+1):tr],sep=""),
    ccc[(ce+1):tr,"p.value"])

colnames(coe) <- c("item",
                   paste(x.name, ".est", sep = ""),
                   paste(x.name, ".pval", sep = ""),
                   paste(y.name, ".est", sep = ""),
                   paste(y.name, ".pval", sep = "")
                   )

coe
```

    ##      item                  Producer.est Producer.pval Consumer.est
    ## [1,] "(Intercept)"         "0.006*"     "0.095"       "0.006 "    
    ## [2,] "X.diff.cons.t_1.pos" "0.303*"     "0.069"       "0.483."    
    ## [3,] "X.diff.cons.t_1.neg" "-0.171 "    "0.557"       "-0.795."   
    ## [4,] "X.diff.prod.t_1.pos" "0.193."     "0.104"       "-0.078 "   
    ## [5,] "X.diff.prod.t_1.neg" "0.49***"    "0.003"       "0.632**"   
    ## [6,] "X.ECT.t_1.pos"       "-0.192 "    "0.276"       "-0.703**"  
    ## [7,] "X.ECT.t_1.neg"       "-0.137 "    "0.384"       "-0.719**"  
    ##      Consumer.pval
    ## [1,] "0.362"      
    ## [2,] "0.113"      
    ## [3,] "0.139"      
    ## [4,] "0.718"      
    ## [5,] "0.038"      
    ## [6,] "0.031"      
    ## [7,] "0.014"

``` r
edia <- ecmDiag(aem, 3)
ed <- edia[c(1,6,7,8,9), ]
ed2 <- cbind(ed[,1:2], "_", ed[,3], "_")
colnames(ed2) <- colnames(coe)
ed2
```

    ##        item Producer.est Producer.pval Consumer.est Consumer.pval
    ## 1 R-squared        0.282             _        0.137             _
    ## 6       AIC     -492.932             _     -363.835             _
    ## 7       BIC     -471.625             _     -342.527             _
    ## 8     LB(4)        0.402             _        0.719             _
    ## 9     LB(8)        0.573             _        0.630             _

``` r
tes <- ecmAsyTest(aem)$out
tes2 <- tes[c(2:7,1), -1]   ### change depending on the lag number

tes3 <- cbind(as.character(tes2[,1]),
              paste(tes2[,2], tes2[,6], sep=''),
              paste("[", round(tes2[,4],2), "]", sep=''),
              paste(tes2[,3], tes2[,7], sep=''),
              paste("[", round(tes2[,5],2), "]", sep=''))

colnames(tes3) <- colnames(coe)

tes3
```

    ##      item                                                  Producer.est
    ## [1,] "cons (x) does not Granger cause..."                  "1.693 "    
    ## [2,] "prod (y) does not Granger cause..."                  "6.814***"  
    ## [3,] "X.diff.cons.t_1.pos = X.diff.cons.t_1.neg"           "1.586 "    
    ## [4,] "X.diff.prod.t_1.pos = X.diff.prod.t_1.neg"           "1.91 "     
    ## [5,] "Cumulative positive cons = Cumulative negative cons" "1.586 "    
    ## [6,] "Cumulative positive prod = Cumulative negative prod" "1.91 "     
    ## [7,] "X.ECT.t_1.pos=X.ECT.t_1.neg"                         "0.055 "    
    ##      Producer.pval Consumer.est Consumer.pval
    ## [1,] "[0.19]"      "1.816 "     "[0.17]"     
    ## [2,] "[0]"         "2.222."     "[0.11]"     
    ## [3,] "[0.21]"      "3.419*"     "[0.07]"     
    ## [4,] "[0.17]"      "3.218*"     "[0.08]"     
    ## [5,] "[0.21]"      "3.419*"     "[0.07]"     
    ## [6,] "[0.17]"      "3.218*"     "[0.08]"     
    ## [7,] "[0.81]"      "0.001 "     "[0.97]"

``` r
coe <- data.frame(apply(coe, 2, as.character), stringsAsFactors=FALSE)
ed2 <- data.frame(apply(ed2, 2, as.character), stringsAsFactors=FALSE)
tes3 <- data.frame(apply(tes3,2, as.character), stringsAsFactors=FALSE)
```

``` r
table.2 <- data.frame(rbind(coe, ed2, tes3))

table.2
```

    ##                                                   item Producer.est
    ## 1                                          (Intercept)       0.006*
    ## 2                                  X.diff.cons.t_1.pos       0.303*
    ## 3                                  X.diff.cons.t_1.neg      -0.171 
    ## 4                                  X.diff.prod.t_1.pos       0.193.
    ## 5                                  X.diff.prod.t_1.neg      0.49***
    ## 6                                        X.ECT.t_1.pos      -0.192 
    ## 7                                        X.ECT.t_1.neg      -0.137 
    ## 8                                            R-squared        0.282
    ## 9                                                  AIC     -492.932
    ## 10                                                 BIC     -471.625
    ## 11                                               LB(4)        0.402
    ## 12                                               LB(8)        0.573
    ## 13                  cons (x) does not Granger cause...       1.693 
    ## 14                  prod (y) does not Granger cause...     6.814***
    ## 15           X.diff.cons.t_1.pos = X.diff.cons.t_1.neg       1.586 
    ## 16           X.diff.prod.t_1.pos = X.diff.prod.t_1.neg        1.91 
    ## 17 Cumulative positive cons = Cumulative negative cons       1.586 
    ## 18 Cumulative positive prod = Cumulative negative prod        1.91 
    ## 19                         X.ECT.t_1.pos=X.ECT.t_1.neg       0.055 
    ##    Producer.pval Consumer.est Consumer.pval
    ## 1          0.095       0.006          0.362
    ## 2          0.069       0.483.         0.113
    ## 3          0.557      -0.795.         0.139
    ## 4          0.104      -0.078          0.718
    ## 5          0.003      0.632**         0.038
    ## 6          0.276     -0.703**         0.031
    ## 7          0.384     -0.719**         0.014
    ## 8              _        0.137             _
    ## 9              _     -363.835             _
    ## 10             _     -342.527             _
    ## 11             _        0.719             _
    ## 12             _        0.630             _
    ## 13        [0.19]       1.816         [0.17]
    ## 14           [0]       2.222.        [0.11]
    ## 15        [0.21]       3.419*        [0.07]
    ## 16        [0.17]       3.218*        [0.08]
    ## 17        [0.21]       3.419*        [0.07]
    ## 18        [0.17]       3.218*        [0.08]
    ## 19        [0.81]       0.001         [0.97]

``` r
table.2 %>% 
  xtable() %>% 
  print(type = "html",
      file = paste(table_path, product, "_Asymetric_ECM.doc", sep = ""))
```
