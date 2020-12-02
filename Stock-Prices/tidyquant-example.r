#https://www.r-bloggers.com/diversification-fact-or-fiction/

rm(list=ls())

# Load package
#install.packages("tidyquant")
library(tidyquant)

# Create toy portfolio
set.seed(123)

mu <- seq(-.03/12,.08/12,.001)
sigma <- seq(0.02, 0.065, .005)

mat <- matrix(nrow = 60, ncol = 10)
for(i in 1:ncol(mat)){
  mu_samp <- sample(mu, 1, replace = FALSE)
  sig_samp <- sample(sigma, 1, replace = FALSE)
  mat[,i] <- rnorm(nrow(mat), mu_samp, sig_samp)
}

df <- as.data.frame(mat)rm(list=ls())
asset_names <- toupper(letters[1:10])
colnames(df) <- asset_names


# Generate cumulative returns 
df_comp <- rbind(rep(1,10), cumprod(df+1))

# Graph cumulative returns
df_comp %>% 
  mutate(date = 0:60) %>%
  gather(key, value, -date) %>%
  ggplot(aes(date, (value-1)*100, color = key)) +
  geom_line() +
  ylab("Return (%)") + xlab("Month") +
  theme(legend.position = "none")


# Calculate average cumulative return and excluding large outlier
avg_cumul <- df_comp %>%
  slice(61) %>%
  summarise(avg = (rowMeans(.)-1)*100) %>%
  as.numeric()

avg_ex <- round(mean(as.numeric(df_comp[61, names(df_comp) != "E"]))-1,3)*100
range_ex <- round(range(df_comp[61, names(df_comp) != "E"])-1,3)*100

# Column chart of cumulative return with average
df_comp %>%
  gather(key,value) %>%
  group_by(key) %>%
  slice(61) %>%
  ggplot(aes(reorder(key, value), (value-1)*100)) +
  geom_bar(stat = 'identity', fill = "royalblue") +
  geom_hline(yintercept = avg_cumul, color = "red") + 
  ylab("Return (%)") + xlab("") 

# Create volatility date frame
vol <- df %>% summarise_all(., sd) %>% t() %>% as.numeric()
vol <- data.frame(asset = asset_names, vol = vol)

# Create portfolio volatility
weights <- rep(0.1, 10)
port_vol <- sqrt(t(weights) %*% cov(df) %*% weights)

# Graph asset volatility
vol %>% 
  mutate(vol = vol*sqrt(12)*100) %>%
  ggplot(aes(reorder(asset, vol), vol)) + 
  geom_bar(stat = "identity", fill = "royalblue1") +
  ylab("Volatility (%)") + xlab("") +
  theme(legend.position = "none")


# Graph volatility of assets vs portfolio
vol %>% 
  mutate(vol = vol*sqrt(12)*100) %>%
  ggplot(aes(reorder(asset,vol), vol)) + 
  geom_bar(stat = "identity", fill = "royalblue1") + 
  geom_hline(yintercept = round(port_vol*sqrt(12),3)*100) + 
  ylab("Volatility (%)") + xlab("") +
  theme(legend.position = "none")

# Calculate mean returns
mean_ret <- df %>% summarise_all(., mean) %>% as.numeric()
mean_ret <- data.frame(asset = asset_names, returns = mean_ret)

## Portfolio returns
port_ret <- sum(weights*mean_ret$returns)


# Graph mean asset returns vs portfolio
mean_ret %>% 
  mutate(returns = returns*1200) %>%
  ggplot(aes(reorder(asset,returns), returns)) + 
  geom_bar(stat = "identity", fill = "royalblue1") + 
  geom_hline(yintercept = port_ret*1200) + 
  ylab("Return (%)") + xlab("") +
  theme(legend.position = "none")

# Cumulative portfolio return
ret <- rowSums(df*weights)
cum_ret <- c(1, cumprod(1+ret))

# Graph cumulative return of assets vs portfolio
df_comp %>%
  gather(key, value) %>%
  group_by(key) %>%
  slice(61)%>%
  ggplot(aes(reorder(key, value), (value-1)*100)) + 
  geom_bar(stat = "identity", fill = "royalblue1") +
  geom_hline(yintercept = (cum_ret[61]-1)*100) +
  ylab("Return (%)") + xlab("")

# Create two data frames for stocks and portfolio 
individual <- df_comp %>% 
  mutate(date = 0:60) %>%
  gather(key, value, -date) 

portfolio <- data.frame(date = 0:60, portfolio = cum_ret)

# Graph portfolio and individual stocks
ggplot() + 
  geom_line(aes(date, value*100, color = key), data = individual, size = 0.8, alpha = 0.5) +
  geom_line(aes(date, portfolio*100), data = portfolio, color = "black", size = 1.1) +
  ylab("Return (%)") + xlab("Month") + 
  theme(legend.position = "none")

## Risk return trade-off

# Create portfolio tibble
port <-tibble(key = "Portfolio", 
              return = mean(ret), 
              vol = sd(ret), 
              ret_vol = mean(ret)/sd(ret)*sqrt(12))

# create graph
df %>%
  gather(key, value) %>%
  group_by(key) %>%
  summarize(return = mean(value),
            vol = sd(value),
            ret_vol = return/vol*sqrt(12)) %>%  
  ggplot(aes(reorder(key, ret_vol), ret_vol*100)) +
  geom_bar(stat = "identity", fill = "royalblue1") +
  geom_hline(yintercept = port$ret_vol*100) +
  ylab("Return/risk (%)") + xlab("")
