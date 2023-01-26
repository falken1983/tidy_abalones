## Carga y definicion del objeto tibble (equivalente a antiguo df)
require(readr)
require(dplyr)
abalone <- readr::read_csv("datos/abalone.txt", col_names = F)
spec(abalone)
nombres_col <- c("Sex","Length","Diameter","Height","Who_weight","Shu_weight","Viscera","Shell","Rings")
colnames(abalone) <- nombres_col
abalone$Sex <- as.factor(abalone$Sex)
summary(abalone)

## Filtering abnormal zero values
### Classical Syntax
abalone[abalone$Height==0,]
### Modern Syntax (dplyr)
dplyr::filter(abalone, Height==0)

##
abalone <- dplyr::filter(abalone, Height!=0)

## Outliers 
boxplot(abalone$Height)

## Boxplot After Cleaning (just a View)
abalone %>% 
  filter(Height<0.5) %>% 
  select(Height) %>% 
  boxplot()

## View and Assignment
abalone <- abalone %>% filter(Height<0.5)

## Rerunning ggPairs with no facetting (by Sex) after cleaning outliers
require(GGally)
ggpairs(
  data = abalone %>% select(-Sex),
  upper = list(continuous=wrap("cor", size=4)),
  lower = list(continuous=wrap("smooth", colour="blue")),
  progress = F
)

## First Fit
initial_linear_fit <- lm(
  Rings~.,
  data=abalone
)
summary(initial_linear_fit)

## We choose AIC as the Information Criteria in order to LM Selection
AIC(initial_linear_fit)

## Variance Inflation for Linear Regression (Features Selection)
require(car)
car::vif(initial_linear_fit)

## Homocedasticity and Distribution of Residuals
par(mfrow=c(2,2))
plot(initial_linear_fit)

##
require(EnvStats)
EnvStats::boxcox(
  initial_linear_fit,
  lambda=seq(-.5,.5,.05)
)
