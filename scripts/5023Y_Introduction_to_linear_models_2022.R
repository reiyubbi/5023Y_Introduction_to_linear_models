#### LOAD PACKAGES -----

library(tidyverse)
library(here)
library(janitor)
library(dplyr)
library(kableExtra)
library(GGally)
library(emmeans)
library(performance)
library(rstatix) 

##___________________-----
#### IMPORT DATA -----

darwin <- read_csv(here("data", "darwin.csv"))
##___________________-----
#### CHECK DATA -----

glimpse(darwin) # provides the first few values in the data set (🗸)

head(darwin) # CHECK DATA IS IN A TIDY FORMAT

colnames(darwin) # reveal column names (🗸)

darwin <- janitor::clean_names(darwin) # CLEAN COLUMN NAMES

# CHECK DUPLICATED DATA

darwin %>% 
  duplicated() %>%
  sum() 

# CHECK TYPOS BY CHECKING IMPOSSIBLE VALUES

darwin %>%
  summarise(min=min(height, na.rm = TRUE))
(max=max(height, na.rm = TRUE))


# CHECK TYPOS BY CHECKING AT DISTINCT CHARACTERS/ VALUES

darwin %>%
  distinct(pair)

darwin %>%
  distinct(type)

is.na(darwin) # reveals any missing data (🗸)

# sum of missing data

darwin %>%
  is.na()
sum()

summary(darwin) # summarise the data (🗸)

##___________________-----
#### DATA VISUALISATION -----

darwin %>%
  ggplot(aes(x = type,
              y = height))+
  geom_point()

# violin plot 

darwin %>%
  ggplot(aes(x = type, 
            y = height))+
  geom_violin() 

# boxplot

darwin %>%
  ggplot(aes(x = type,
             y = height))+
  geom_boxplot() 

# using tidy functions to show standard deviation

darwin %>% 
  group_by(type) %>%
  summarise(mean = mean(height),
            sd = sd(height))

# can present both as figure or table because the data set is simple

darwin_summary <- darwin %>% 
  group_by(type) %>%
  summarise(mean = mean(height),
            sd = sd(height))

# create summary plot 

darwin_summary %>%
  ggplot(aes(x = type,
             y = mean))+
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd))+
  theme_classic()
                
# using kable functions to create a table

darwin_summary %>%
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

# calculate mean difference in heights

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

# calculate standard error
# the average difference in height was 2.62 +- 1.22 inches (mean +- SE)

difference_summary %>% 
  mutate(se= sd/sqrt(n))
##___________________-----
#### DISTRIBUTION ----

#Create a sequence of 100 equally spaced numbers between -4 and 4

x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

# confidence intervals

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI



##___________________-----
#### LINEAR MODELS -----

# general function lm() creates a linear model using the "least-squares" technique

lsmodel0 <- lm(formula = height ~ 1, data = darwin)

# lm function can be piped but any function not belonging to the tidyverse family needs a . where the data should be

lsmodel0 <- darwin %>% lm(height ~ 1, data= .)

# first argument of lm() is formula, analyse a response variable as a function of an explanatory variable using the tilde ~

# summary of the model can be obtained with the tidyverse package broom and can interact with model objects

broom::tidy() # summarise information about model components

broom::augment() # add information about individual observations on a dataset in order to make predictions about a new one

broom::glance() # report information about the entire model 

# summarise using base r

summary(lsmodel0)

# summarise using broom tidyverse

broom::tidy(lsmodel0)

# output is called table of coefficients, first row is the intercept equal to the mean in this case

mean(darwin$height)

# analyse the difference in average plant height as a function of pollination type

lsmodel1 <- lm(height ~ type, data=darwin)

# note that the following is identical

# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

# model contains pollination type in addition to the intercept

broom::tidy(lsmodel1)

# value of the intercept has changed,the second row is called typeSelf (combination of variable 'type' with factor 'Self'
# intercept must be typecross, second row represents the difference in mean heights of the two groups

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))

# confirm means

summary(lsmodel1)

# model we can create a plot using the means

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

# confidence intervals in base r

confint(lsmodel1)

# confidence levels in tidyverse

broom::tidy(lsmodel0, conf.int = T)

# ggcoef model produces a graph of the estimated mean difference when cf = 95%

GGally:: ggcoef_model(lsmodel1,
                      show_p_values = FALSE,
                      conf.level = 0.95)

# confidence interval at 0.99

GGally:: ggcoef_model(lsmodel1,
                      show_p_values = FALSE,
                      conf.level = 0.99)

# tidy function can also check this argument

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

# calculating other mean and standard error treatment

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()


# emmeans function and package performs similarly

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means


# data summary visual using emmeans

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

##assumption checking

performance::check_model

##___________________-----
#### TESTING -----

# student's t test using t distribution which has a bigger uncertainty than the z distribution.
# caculated by t = difference / SE

# calculation in base R

x <- seq(-4, 4, length=100)

hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

# calculation in tidyverse

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

# two values of t - critical (dependent on df) and observed (value obtained by test)
# significance is indicated by observed > critical
# critical values of t up to 30 df:

df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

# create linear model

lsmodel1 <- lm(height ~ type, data = darwin)

# use a function to view a summary of the model; applies tests to every row, may not include apriori tests significant to the model

summary(lsmodel1)

# obtain observed value of t ( - 2.437113)

tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]

# paired t - add factor for pairs to linear model formula

# in base r

lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)

# in tidyverse

darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()

# linear model computes every combination of p value and t statistic, we are looking at the mean heights of crossed and self pollinated plants in the same pair.
# therefore we look at the second row :"wat is the difference in height between cross and self pollinated plants when we hold pairs constant? "

# generate CI for paired t test

lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows

# estimate of mean difference is identical but the 95% CI is slightly different; due to greater uncertainty

m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()

# type 1 and type 2 errors
# type 1 error derived from the 1/20 chance a null hypothesis is rejected incorrectly if a = 0.05
# type 2 error is when the null hypothesis is accepted instead of rejected, probability calculated by 1 - B where beta error should be less than 20
# creating a loop in order to find whether this has occured 

set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

#the new dataframe y contains the results of 20 new experiments - how many found a significant difference?

y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())

# visualise inconsistence of estimates and confidence intervals - reducing the effect of sampling error

y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()

##___________________-----
#### REGRESSION -----

# import tree data

janka <- read_csv(here("data", "janka.csv"))

# data checks

glimpse(janka)

head(janka)

colnames(janka)

janka <- janitor::clean_names(janka)

janka %>% 
  duplicated() %>%
  sum() 

# CHECK TYPOS BY CHECKING IMPOSSIBLE VALUES

janka %>%
  summarise(min=min(height, na.rm = TRUE))
(max=max(height, na.rm = TRUE))


# CHECK TYPOS BY CHECKING AT DISTINCT CHARACTERS/ VALUES

janka %>%
  distinct(pair)

janka %>%
  distinct(type)

is.na(janka) # reveals any missing data (🗸)

# sum of missing data

janka %>%
  is.na()
sum()

summary(janka) # summarise the data

# create a plot to view density vs hardness relationship

janka %>% 
  ggplot (aes(x = dens,
              y = hardness))+
  geom_point()

# pearson's r test (rstatix)

cor_test(janka)

janka %>%
  cor_test(dens, hardness)

# pearson's test in base R

with(janka, cor(dens, hardness))

# regress the data - test hypothesis, the response variable goes on the left and the predictor variable goes on the right of the tilde
janka_ls1 <- lm(hardness ~ dens, data = janka) 


# geom smooth adds line of best fit to the data using least squares method

# specify linear model method for line fitting, the uncertainty increases

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

# base r summary

summary(janka_ls1)

# tidyverse summary

janka_ls1 %>% 
  broom::tidy()


# centre the data

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

# predict upper and lower bands of confidence intervals base r

confint(janka_ls1)

# tidyverse confidence interval

broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)

# find value of r2 (base r0

summary(janka_ls1)

# find value of r2 (tidyverse)

janka_ls1 %>%
  broom::glance()

# calculate residuals (difference between the observed values and fitted values from the model) for each data point

# base r

predict(janka_ls1)

resid(janka_ls1)

# tidyverse

janka_ls1 %>%
  broom::augment() %>%
  head()

# plot residuals

augmented_ls1 < - janka_ls1 %>%
  broom::augment()

augmented_ls1 %>%
  ggplot(aes( x = dens,
              y = .fitted))+
  geom_line()+
  geom_point(aes( x = dens, 
                  y = hardness))+
  geom_segment(aes( x = dens,
                    xend = dens,
                    y = .fitted,
                    yend = hardness),
               linetype = "dashed", colour = "red")
  

# a line creating all the data points in order

p1 <- augmented_ls1 %>%
  ggplot(aes(x = dens,
             y = hardness))+
  geom_line()+
  ggtitle("Full Data")

# plotting the fitted values against the independent ( regression line )

p2 <- augmented_ls1 %>%
  ggplot(aes( x = dens,
              y = .fitted ))+
  geom_line()+
  ggtitle("Linear Trend")

# plotting the residuals against the fitted valuables

p3 <- augmented_ls1 %>%
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3



##___________________-----
#### ANOVA -----

# produce anova table for the linear model 

anova(lsmodel1)

# set as two directional test in last argument
# r function pf recreates the exact p value seen in the anova table

pf(5.9395, 1, 28, lower.tail=FALSE)

# two way anova

lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)

pf(5.9395, 1, 28, lower.tail=FALSE)

anova(lsmodel2)


##___________________-----
#### GITHUB SETUP -----


gitcreds::gitcreds_set()

# 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



