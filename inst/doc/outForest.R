## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(outForest)

# Create data set with truely multivariate outlier
set.seed(3)
t <- seq(0, pi, by = 0.01)
dat <- data.frame(x = cos(t), y = sin(t) + runif(length(t), -0.1, 0.1))
dat[c(100, 200), ] <- cbind(c(-0.5, 0.5), c(0.4, 0.4))

plot(y ~ x, data = dat)

# Let's run outForest on that data
ch <- outForest(dat)

# What outliers did we find?
outliers(ch)

# Bingo! How does the fixed data set looks like?
plot(y ~ x, data = Data(ch))

# The number of outliers per variable
plot(ch)

## -----------------------------------------------------------------------------
head(irisWithOutliers <- generateOutliers(iris, p = 0.02))
out <- outForest(irisWithOutliers, splitrule = "extratrees", 
                 num.trees = 50, verbose = 0)

# The worst outliers
head(outliers(out))

# Summary of outliers
summary(out)

# Basic plot of the number of outliers per variable
plot(out)

# Basic plot of the scores of the outliers per variable
plot(out, what = "scores")

# The fixed data
head(Data(out))

## -----------------------------------------------------------------------------
library(dplyr)

irisWithOutliers %>% 
  outForest(verbose = 0) %>%
  Data() %>% 
  head()
  

## -----------------------------------------------------------------------------
out <- outForest(iris, allow_predictions = TRUE, verbose = 0)
iris1 <- iris[1:2, ]
iris1$Sepal.Length[1] <- -1
pred <- predict(out, newdata = iris1)
outliers(pred)
Data(pred)

## -----------------------------------------------------------------------------
out <- outForest(irisWithOutliers, Sepal.Length ~ Species, verbose = 0)
summary(out)

## -----------------------------------------------------------------------------
out <- outForest(irisWithOutliers, . - Sepal.Length ~ ., verbose = 0)
summary(out)

## -----------------------------------------------------------------------------
outliers(outForest(irisWithOutliers, max_n_outliers = 3, verbose = 0))

## -----------------------------------------------------------------------------
out <- outForest(irisWithOutliers, replace = "NA", verbose = 0)
head(Data(out))

