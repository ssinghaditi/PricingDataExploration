library(ggplot2)
library(moderndive)
library(tidyverse)
library(infer)

price <- read.csv("/Users/aditi/Documents/BUAD 312/Price.csv", na.string="N/A")
demand <- read.csv("/Users/aditi/Documents/BUAD 312/Demand.csv", na.string="N/A")
price = subset(price, select = -c(X, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9))

# get rid of unnecessary columns
price = subset(price, select = -c(Month, Day, Weekday))

# correlation matrix for demand
cor_matrix = demand %>% 
  select(Price, Demand)
cor(cor_matrix)

# demand vs price 
demand %>% 
  ggplot(aes(x = Price, y = Demand)) +
  geom_point() +
  geom_smooth(method=lm, se=F)

# split the dataset into WTP & competitor prices
WTP <- price %>% 
  filter(Source == "WTP")

Competitior <- price %>% 
  filter(Source == "Competitor Price")

# histogram of price variation for each different product - WTP
WTP %>% 
  ggplot(aes(x = Price)) +
    geom_histogram() +
    facet_wrap(~Product)

# histogram of prices for competitors - Competitor
Competitior %>% 
  ggplot(aes(x = Price)) +
  geom_histogram() +
  facet_wrap(~Product)

# find average price for each product - WTP
avg_price1 <- WTP %>% 
  group_by(Product) %>% 
  summarise(mean = mean(Price, na.rm = TRUE))

avg_price1 %>% 
  ggplot(aes(x = Product, y = mean)) +
  geom_col()

# find average price for each product - Competitor
avg_price2 <- Competitior %>% 
  group_by(Product) %>% 
  summarise(mean = mean(Price, na.rm = TRUE))

avg_price2 %>% 
  ggplot(aes(x = Product, y = mean)) +
  geom_col()

# Egg & Cheese muffin looks interesting - look at prices for WTP vs Competitor
EggMuffin <- price %>% 
  filter(Product == "Egg & Cheese Muffin")

EggMuffin %>% 
  ggplot(aes(x = Price)) +
  geom_histogram() +
  facet_wrap(~Source)

# ------------------------------------------------------------------------------
# French Toast  
FrenchToast_p <- price %>% 
  filter(Product == "French Toast")

FrenchToast_d <- demand %>% 
  filter(Product == "French Toast")

# price distribution for WTP vs Competitor
FrenchToast_p %>% 
  ggplot(aes(x = Price)) +
  geom_histogram() +
  facet_wrap(~Source)

# gender distribution
FrenchToast_p %>% 
  ggplot(aes(x = Gender)) +
  geom_bar()

# age distribution
FrenchToast_p %>% 
  ggplot(aes(x = Age)) +
  geom_histogram()

# ethnicity distribution
FrenchToast_p %>% 
  ggplot(aes(x = Ethnicity)) +
  geom_bar()

FrenchToast_p %>% 
  ggplot(aes(x = Price)) +
  geom_histogram() +
  facet_wrap(~Ethnicity)

# location distribution
FrenchToast_p %>% 
  ggplot(aes(x = Ethnicity)) +
  geom_bar()

FrenchToast_p %>% 
  ggplot(aes(x = Price)) +
  geom_histogram() +
  facet_wrap(~Location)

# price distribution according to demand
FrenchToast_d %>% 
  ggplot(aes(x = Price)) +
  geom_histogram()

# demand vs price 
FrenchToast_d %>% 
  ggplot(aes(x = Price, y = Demand)) +
  geom_point() +
  geom_smooth(method=lm, se=F)

ft_avgP = FrenchToast_p %>% 
  group_by(Source) %>% 
  summarize(meanPrice = mean(Price, na.rm = TRUE))
  
ft_avgD = FrenchToast_d %>% 
  summarize(meanPrice = mean(Price, na.rm = TRUE))

# comparing average prices across WTP, competitor prices, and demand
ft_avg_summary <- tibble(ft_avgP)
ft_avg_summary = ft_avg_summary  %>% 
  add_row(Source = "Demand", ft_avgD)
ft_avg_summary

# bar graph of average prices across diff. categories
ft_avg_summary %>% 
  ggplot(aes(x=Source, y=meanPrice)) +
  geom_col()

#-----------------------------------------------------------------------------
# H0: WTP & Competitor Price are the same
# H1: Competitor Price > WTP Price

obs_stat1 <- FrenchToast_p %>% 
  specify(Price~Source) %>% 
  calculate(stat = "diff in means", order = c("Competitor Price", "WTP"))

null_dist1 <- FrenchToast_p %>%
  specify(Price~Source) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps=1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("Competitor Price", "WTP"))

get_p_value(null_dist1, obs_stat1, direction = "right")

boot1 <- FrenchToast_p %>%
  specify(Price~Source) %>% 
  generate(reps=1000) %>% 
  calculate(stat = "diff in means", order = c("Competitor Price", "WTP"))

ci1 <- boot1 %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
ci1

visualize(null_dist1) +
  shade_p_value(obs_stat1, direction = "right") +
  shade_confidence_interval(ci1)

# reject the null hypothesis because p-value < 0.05; CI says the average 
# difference in price between WTP and Competitor Price is in the positive range

#-----------------------------------------------------------------------------
# hypothesis testing - gender; confounding = age
# H0: WTP price same for Male and Female
# H1: WTP price female > WTP price male

FrenchToast_p_WTP <- FrenchToast_p %>% 
  filter(Source == "WTP")

obs_stat2 <- FrenchToast_p_WTP %>% 
  specify(Price~Gender) %>% 
  calculate(stat = "diff in means", order = c("F", "M"))

null_dist2 <- FrenchToast_p_WTP %>%
  specify(Price~Gender) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps=1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("F", "M"))

get_p_value(null_dist2, obs_stat2, direction = "right")

boot2 <- FrenchToast_p_WTP %>%
  specify(Price~Gender) %>% 
  generate(reps=1000) %>% 
  calculate(stat = "diff in means", order = c("F", "M"))

ci2 <- boot2 %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
ci2

visualize(null_dist2) +
  shade_p_value(obs_stat2, direction = "right") +
  shade_confidence_interval(ci2)

# p-value > 0.05 - fail to reject H0; Gender doesn't seem to have an effect on Price. 
# CI includes 0 meaning chance that average difference in price for female and male 
# could be 0

# Is age a confounding factor? - can't look at age since all the people are young except 1
FrenchToast_p_WTP = FrenchToast_p_WTP %>% 
  mutate(age.range=ifelse(Age<35, "Young", ifelse(Age<59, "Middle", "Old")))

#-----------------------------------------------------------------------------
# linear regression model
lm1 = lm(Demand ~ Price, demand)
summary(lm1)
get_regression_table(lm1)
# Demand = 104.380 + -7.821*Price

# Hypothesis test for slope
# H0: slope = 0
# H1: slope != 0

obs_slope_dp <- demand %>%
  specify(Demand ~ Price) %>% 
  calculate(stat = "slope")

# Hypothesis testing
null_slope_dp <- demand %>%
  specify(Demand ~ Price) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "slope")

get_p_value(null_slope_dp, obs_slope_dp, direction = "both")
# p-value = 0; reject the null hypothesis - the price does have an impact on demand.

# Confidence Interval
boot_strap_dp <- demand %>%
  specify(Demand ~ Price) %>%
  generate(reps=1000) %>% 
  calculate(stat="slope")

percentile_dp <- boot_strap_dp %>%
  get_confidence_interval(type = "percentile")
percentile_dp

visualize(null_slope_dp) +
  shade_p_value(obs_stat = obs_slope_dp, direction = "both") +
  shade_confidence_interval(percentile_dp)
# 95% confidence that the impact is going to be negative. 

# Competitor demand based on this equation
price = price %>% 
  mutate(demandComp = 104.380 + (-7.821*Price))

price  %>% 
  ggplot(aes(x = Price, y = demandComp)) +
  geom_point() +
  geom_smooth(method=lm, se=F)

# ------------------------------------------------------------------------------
# find profit
demand = demand %>% 
  mutate(unit_cost=ifelse(Product=="Chocolate Shake", 1.38, 
                          ifelse(Product=="Greek Salad", 1.7125, 
                                 ifelse(Product=="French Toast", 1.707, 
                                        ifelse(Product=="Oreo Shake", 1.8, 0.897))))) %>% 
  mutate(Profit = Price - unit_cost)

# Estimating demand based on profit
demand  %>% 
  ggplot(aes(x = Profit, y = Demand)) +
  geom_point() +
  geom_smooth(method=lm, se=F)

lm2 = lm(Demand ~ Profit, demand)
summary(lm2)
get_regression_table(lm2)
# Demand = 95.559 + -8.569*Profit

# Hypothesis test for slope for estimating demand based on profit
# H0: slope = 0
# H1: slope != 0

obs_slope_dp2 <- demand %>%
  specify(Demand ~ Profit) %>% 
  calculate(stat = "slope")

# Hypothesis testing
null_slope_dp2 <- demand %>%
  specify(Demand ~ Profit) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "slope")

get_p_value(null_slope_dp2, obs_slope_dp2, direction = "both")
# p-value = 0; reject the null hypothesis - profit does have an impact on demand.

# Confidence Interval
boot_strap_dp2 <- demand %>%
  specify(Demand ~ Profit) %>%
  generate(reps=1000) %>% 
  calculate(stat="slope")

percentile_dp2 <- boot_strap_dp2 %>%
  get_confidence_interval(type = "percentile")
percentile_dp2

visualize(null_slope_dp2) +
  shade_p_value(obs_stat = obs_slope_dp2, direction = "both") +
  shade_confidence_interval(percentile_dp2)
# 95% confidence that the impact is going to be negative.

# -----------------------------------------------------------------------------
# Profit vs Price
demand  %>% 
  ggplot(aes(x = Price, y = Profit)) +
  geom_point() +
  geom_smooth(method=lm, se=F)

# find profit line
lm3 = lm(Profit ~ Price, demand)
summary(lm3)
get_regression_table(lm3)
# Profit = -1.36 + 0.969*Price

# intersection of demand line and profit line based on price to find optimal price
plot(demand$Price, demand$Profit, col = "red") + abline(-1.36, 0.969, col="red")
par(new=TRUE)
plot(demand$Price, demand$Demand, col = "blue") + abline(104.380, -7.821, col="blue") 

# -----------------------------------------------------------------------------
# regression model for WTP using comp price
ggplot() +
  geom_dotplot(data = WTP, aes(x = Price, fill = "red", alpha = 0.5)) +
  geom_dotplot(data = Competitior, aes(x = Price, fill = "blue", alpha = 0.5)) +
  scale_fill_discrete(name = "Price", labels = c("WTP", "Competitor"))

WTP %>% 
  group_by(Product) %>% 
  summarize(n())

Competitior %>% 
  group_by(Product) %>% 
  summarize(n())

demand %>% 
  group_by(Product) %>% 
  summarize(n())

# lm4 = lm(WTP_p ~ Competitor_p, demand)
# create further regression model in the future (num of data points don't match up)