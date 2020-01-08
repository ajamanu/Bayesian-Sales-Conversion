# Bayesian Sales Conversion.R
# An intuitive approach to calculate the sales conversion rate with no historical 
# data to compare results with
# Created by Aja Manu on 08/01/2020

# https://towardsdatascience.com/calculating-sales-conversion-using-bayesian-probability-b08f9fb262f2

#### Background

# Company distribute the brochure to 23 Texans and end up making 9 sales.
brochures_distributed = 23
new_sales = 9
conversion_rate = new_sales/brochures_distributed
conversion_rate

#### Fitting Model

# number of samples to draw from the prior distribution
n_size <- 100000

# drawing sample from the prior distribution - which in our case is uniform distribution
prior_dist <- runif(n_size, 0, 1)

# verify the random sample was generated correctly i.e. uniform distribution.
hist(prior_dist, main = "Histogram of Prior Distribution", 
     xlab = "Prior on the Conversion Rate", 
     ylab = "Frequency")

# defining the generative model - a model or set of rules that we feed to parameters 
# so that it simulates data based on those set of rules
generative_model <- function(rate) {
      sales <- rbinom(1, size = 23, prob = rate)
      sales
}

# simulating the data through our generative model
sales <- rep(NA, n_size)

for(i in 1:n_size) {
      sales[i] <- generative_model(prior_dist[i])
}

# filtering out values from the model that do not match our observed results
post_rate <- prior_dist[sales == 9]

#plotting the posterior distribution
post_rate_hist = hist(post_rate, xlim = c(0,1), main = "Histogram of Posterior Distribution")
post_rate_hist

# sum of frequency of draws between 0.40 and 0.45 divided by total draws
post_rate_hist$counts[7]/length(post_rate)

#### Comparing Strategis

# calculate the frequency of the percentage of conversions greater than 20%
sum(post_rate > 0.20) / length(post_rate)

### Confidence Interval

quantile(post_rate, c(0.025, 0.975))
