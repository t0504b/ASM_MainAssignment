library(jsonlite)
library(ggplot2)
#library(MCMCpack)
library(RColorBrewer)
library(dplyr)
#library(LabelEncoder)
library(plyr)

df <- read.csv("winemag-data-130k-v2.csv")

df <- df[c('country','points','price','region_1','variety')]

summary(df)

df_test <- filter(df,df$variety == "Sauvignon Blanc" & df$country == "South Africa" | 
              df$variety == "Chardonnay" & df$country == "Chile")

tail(df_test)

df_new <- filter(df_test,df_test$price == 15)

tail(df_new)

data_1 = df_new[c("country","points","variety")]

data_1$variety <- factor(data_1$variety)

# To treat variety as a index column, not as a measurement we have changed the class of this object to be a factor

summary(data_1)

sum(is.na(df_new))

p <- ggplot(data_1,aes(variety, points,fill=variety)) + geom_boxplot(outlier.colour = 'red',outlier.shape = 8)
p + scale_fill_manual(breaks = c("Chardonnay", "Sauvignon Blanc"),values=c("green", "blue")) + geom_jitter(aes(variety, points, colour = variety))

tapply(data_1$points,data_1$variety,mean)

# From above output we assume that the average rating of Sauvignon Blanc is better than Chardonnay wine

tapply(data_1$points,data_1$variety,median)

tapply(data_1$points,data_1$variety,sd)

t.test(points ~ variety, data=data_1, var.equal = TRUE)

#selecting the variety of wine based on country
SB_wine <- data_1 %>% filter(country == "South Africa", variety == "Sauvignon Blanc")

CH_wine <- data_1 %>% filter(country == "Chile",  variety == "Chardonnay")

mean_diff <- mean(SB_wine$points) - mean(CH_wine$points)

mean_diff

wine_rating_quality = ((mean(SB_wine$points) - mean(CH_wine$points))/mean(CH_wine$points))*100

wine_rating_quality

#compare_gibbs <- function(y, ind, mu0 = 85, tau0 = 1/25, del0 = 0, gamma0 = 1/25,a0 = 289, b0 = 3.4, maxiter = 5000)
compare_gibbs <- function(y, ind, mu0 = 85, tau0 = 1/100, del0=0, gamma0=1/100, a0 = 50, b0 = 1 , maxiter = 5000)
{
y1 <- y[ind == 1]
y2 <- y[ind == 2]
n1 <- length(y1)
n2 <- length(y2)
##### starting values
mu <- (mean(y1) + mean(y2)) / 2
del <- (mean(y1) - mean(y2)) / 2
mat_store <- matrix(0, nrow = maxiter, ncol = 3)
#####
##### Gibbs sampler
an <- a0 + (n1 + n2)/2
for(s in 1 : maxiter)
{
##update tau
bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
tau <- rgamma(1, an, bn)
##
##update mu
taun <- tau0 + tau * (n1 + n2)
mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
mu <- rnorm(1, mun, sqrt(1/taun))
##
##update del
gamman <- tau0 + tau*(n1 + n2)
deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
del<-rnorm(1, deln, sqrt(1/gamman))
##
## store parameter values
mat_store[s, ] <- c(mu, del, tau)
}
colnames(mat_store) <- c("mu", "del", "tau")
return(mat_store)
}

#install.packages('MCMCpack')

library('MCMCpack')

fit <- compare_gibbs(data_1$points, data_1$variety)
plot(as.mcmc(fit))

raftery.diag(as.mcmc(fit))

# Lower values of dependence factor(Close to zero) explains that performance of the sampler is satisfactory.

#We can use summary statistics as below to understand paramters of the posterior distribution

apply(fit, 2, mean)

apply(fit, 2, sd)

# to interperate tau we convert it to sd
mean(1/sqrt(fit[, 3])) 
sd(1/sqrt(fit[, 3]))

apply(fit, 2, function(x) quantile(x, c(0.05, 0.95)))

y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

## set up ggplot for histogram and density plots
p2 <- ggplot(data.frame(y_sim_diff = y1_sim-y2_sim),aes(y1_sim-y2_sim, ..density..))

## add histogram
p2 <- p2 + geom_histogram(fill = 'green', bins = 25, aes(y_sim_diff,alpha = 0.5),colour = "black", show.legend = FALSE)
p2 <- p2 + geom_density(fill = "lightblue", aes(y_sim_diff,alpha = 0.5),colour = "brown", show.legend = FALSE)
p2 <- p2 + xlab("Difference in simulated samples") + ylab("probability density") + ggtitle("PDF of posterior simulation")
p2 + theme(panel.background = element_rect(fill = "white", colour = "red",size = 0.5, linetype = "solid"))

mean(y1_sim > y2_sim)

ggplot(data.frame(y1_sim, y2_sim)) + geom_point(color='blue',fill="white",aes(y1_sim, y2_sim), alpha = 0.3) + geom_abline(slope = 1, intercept = 0) 

print("The probability that Savo Wine from SA is better than Chardonnay wine from Chile is: 0.73")

df_test_2 <- filter(df,df$country == "Italy")

df_test_3 <- filter(df_test_2,df_test_2$price < 20)

head(df_test_3,10)

df_test_4 = ddply(df_test_3, "region_1", function(d) {if(nrow(d)>3) d else NULL})

head(df_test_4,20)

df_test_4[df_test_4 == ""] <- NA

head(df_test_4,10)

nrow(df_test_4)

summary(df_test_4)

sum(is.na(df_test_4$region_1))

ggplot(df_test_4) + geom_boxplot(aes(x = reorder(region_1, points, median), points,
fill = reorder(region_1, points, median)), show.legend=FALSE) + labs(title = "Ratings for Wines in Italy region",
x = "", y = "Wine Ratings") + theme_gray() + theme(axis.text.x = element_text(size = 7,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))

#plotting the number of ratings for wines in the regions within Italy
df_test_4 %>% ggplot(aes(x = reorder(region_1, region_1, length),color='brown')) + stat_count() +
labs(title = "Variety of Wines within regions in Italy",x = "", y = "Ratings Count") +theme_gray() + 
theme(panel.background = element_rect(fill = "lightblue",
                                colour = "lightblue",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"), axis.text.x = element_text(size = 7,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))

# In above plot we can see that there is not much sample size(count of reviews) for many wines. However there are some wines for which
# there are plent of reviews given

ggplot(data.frame(size = tapply(df_test_4$points, df_test_4$region_1, length),
                  mean_score = tapply(df_test_4$points, df_test_4$region_1, mean)), 
aes(size, mean_score)) + geom_point(color = 'blue') + xlab("Region sample size") + ylab("Mean Score") + 
ggtitle("Effect size versus sample size")

gibbs_m_categories <- function(y, 
                               categories, 
                               mu_0 = 85, 
                               tau_0 = 1/100, 
                               a_0 = 1, 
                               b_0 = 50, 
                               alpha_0 =1, 
                               beta_0 = 50, 
                               max_iterations = 5000)
    
#     mu0 = 85, tau0 = 1/25, del0 = 0, gamma0 = 1/25,
# a0 = 289, b0 = 3.4, maxiter = 5000
{
  a_0 <- 289
  b_0 <- 3.4
  alpha_0 <-289
  beta_0 <- 3.4
  mu_0 <- 85
  tau_0 <- 1/25
  
  m <- nlevels(categories)
  ybar <- theta <- tapply(y, categories, mean)
  tau_w <- mean(1 / tapply(y, categories, var))
  
  mu <- mean(theta)
  tau_b <-var(theta)
  
  n_m <- tapply(y, categories, length)
  alpha_n <- alpha_0 + sum(n_m)/2
  
  theta_matrix <- matrix(0, nrow = max_iterations, ncol = m)
  matrix_store <- matrix(0, nrow = max_iterations, ncol = 3)
  
  for(iter in 1:max_iterations) 
  {
    for(j in 1:m) 
    {
      tau_n <- n_m[j] * tau_w + tau_b
      theta_n <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / tau_n
      theta[j] <- rnorm(1, theta_n, 1/sqrt(tau_n))
    }
    
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[categories == j] - theta[j])^2)
    }
    
    beta_n <- beta_0 + ss/2
    tau_w <- rgamma(1, alpha_n, beta_n)
    
    tau_m <- m * tau_b + tau_0
    mu_m <- (mean(theta) * m * tau_b + mu_0 * tau_0) / tau_m
    mu <- rnorm(1, mu_m, 1/sqrt(tau_m)) 

    a_m <- a_0 + m/2
    b_m <- b_0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, a_m, b_m)
    
    theta_matrix[iter,] <- theta
    matrix_store[iter, ] <- c(mu, tau_w, tau_b)
  }
  colnames(matrix_store) <- c("mean", "precision(w)", "precision(b)")
  colnames(theta_matrix) <- levels(categories)
  return(list(params = matrix_store, theta = theta_matrix))
}

sum(is.na(df_test_4$region_1))

df_test_5 <- na.omit(df_test_4)

head(df_test_5,5)

sum(is.na(df_test_5))

df_test_5$region_1 <- as.character(df_test_5$region_1)
df_test_5 <- df_test_5[order(df_test_5$region_1), ]
df_test_5$region_1 <- as.factor(df_test_5$region_1)

df_test_5$points <- df_test_5$points + rnorm(nrow(df_test_5), 1, 1)/1000

tail(df_test_5,10)

fit2 <- gibbs_m_categories(df_test_5$points, df_test_5$region_1)

plot(as.mcmc(fit2$params))

apply(fit2$params, 2, mean)
apply(fit2$params, 2, sd)

mean(1/sqrt(fit2$params[, 2]))
sd(1/sqrt(fit2$params[, 2]))

mean(1/sqrt(fit2$params[, 3]))
sd(1/sqrt(fit2$params[, 3]))

theta_hat <- apply(fit2$theta, 2, mean)

theta_hat

names(theta_hat) <- colnames(fit2$theta)

sort(theta_hat, decreasing = TRUE)

theta_df_error <- data.frame(lower = theta_quantile_bounds[1, ], 
                       upper = theta_quantile_bounds[2, ], 
                       mean = theta_hat, 
                       region = colnames(fit2$theta))

head(theta_df_error,10)

theta_df <- data.frame(samples = as.numeric(fit2$theta), 
                       region = rep(colnames(fit2$theta), 
                                    each = nrow(fit2$theta))) 

#reformatting the samples as per the order of the median
ggplot(theta_df,aes(colour='pink')) + 
  geom_boxplot(aes(x = reorder(region, samples, median), 
                   samples, 
                   fill = reorder(region, samples, median)), 
               show.legend=FALSE) +
  labs(title = "Ratings for Wines between regions in Italy from generated samples",
       x = "",
       y = "Mean") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))

# reformat samples for ggplot
theta_df <- data.frame(samples = as.numeric(fit2$theta),
region = rep(1:ncol(fit2$theta), each = nrow(fit2$theta)))
ggplot(theta_df) + geom_boxplot(aes(x = reorder(region, samples, median), samples,
fill = reorder(region, samples, median)), colour='blue', show.legend=FALSE) + 
theme(panel.background = element_rect(fill = "lightblue", colour = "lightblue",size = 0.5, linetype = "solid"))

ggplot(data.frame(size = tapply(df_test_5$points, df_test_5$region_1, length), theta_hat = theta_hat),
aes(size, theta_hat)) + geom_point()

theta_hat_df <- data.frame(theta_hat)

head(theta_hat_df,10)

theta_hat_df$region_1 <- unique(sort(df_test_5$region_1))

head(theta_hat_df,5)

# Below are the regions that produce better then average wine

new <- theta_hat_df[theta_hat_df$theta_hat > mean(fit2$params[, 1]), 2]

new

summary(data.frame(new))
