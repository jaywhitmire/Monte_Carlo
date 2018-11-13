# Jeremiah Whitmire
# November 6, 2018


##### Say there are 99 real coins and 1 trick coin.  What is the probability that the coin is a trick coin given we drew 10 heads in a row?

# P(Trick Coin | 10 Heads) = (  P(10 Heads | Trick Coin) * P(Trick Coin) ) / P( 10 Heads)

# Given that the coin is a trick coin there is a 100% chance we flip 10 heads in row
# there is a 1 in 100 chance the coin is a trick coin
# the chance we flip 10 heads in a row is 50% to the tenth power for 99 of the coins and 100% for 1 of the coins

( 1 * 1/100 ) / ((0.5 ^ 10) * 99/100 + 1 * 1/100)

# 91.18% of the time the coin is a trick coin given we drew 10 heads in a row


###  Monte Carlo Simulation of Probability
set.seed(1234)
instance <- 1:1000000
coin <- sample(1:100, size = 1000000, replace = TRUE)

flip1 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip2 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip3 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip4 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip5 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip6 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip7 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip8 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip9 <- sample(c(1, 0), size = 1000000, replace = TRUE)
flip10 <- sample(c(1, 0), size = 1000000, replace = TRUE)


df <- data.frame(instance, coin, flip1, flip2, flip3, flip4, flip5, flip6, flip7, flip8, flip9, flip10)


# the number of heads is the count of heads in our 10 flips, if the coin is the trick coin than we must have got 10 heads

df <- df %>% 
  mutate(num_heads = ifelse(coin == 100, 10, flip1 + flip2 + flip3 + flip4 + flip5 + flip6 + flip7 + flip8 + flip9 + flip10))

df <- df %>% 
  mutate(all_heads = ifelse(num_heads == 10, 1, 0),
         trick_coin = ifelse(coin == 100, 1, 0)) 


ggplot(df, aes(x = coin, fill = factor(trick_coin))) +
  geom_histogram(bins = 100) +
  labs(x = 'Coin', y = 'Count', fill = 'Trick Coin  = 1', title = 'Uniform Distribution of Coins Used')

ggplot(df, aes(x = factor(trick_coin), fill = factor(trick_coin))) +
  geom_bar() +
  labs(x = 'Coin', y = 'Count', fill = 'Trick Coin  = 1', title = '99% of Coins Drawn are Normal Coins')

ggplot(df, aes(x = num_heads, fill = factor(trick_coin))) +
  geom_histogram(bins = 11) +
  labs(x = 'Number of Heads', y = 'Count', fill = 'Trick Coin  = 1', title = 'Bell Curve Distibution for Number of Heads Flipped', subtitle = "The large majority of 10s are with a Trick Coin")


sum(df$all_heads)

# 11,148 of our million simulations were all heads

mean(df$all_heads)

#  That is 1.1148%


sum(df$trick_coin)

# We drew the trick coin in 10,178 of our million simulations

mean(df$trick_coin)

# that is 1% of coin drawings exactly what we would expect for 1 coin out of 100


#filtering down the data so that it is a given that we flipped 10 heads in a row
all_heads <- df %>% 
  filter(all_heads == 1)

nrow(all_heads)

sum(all_heads$trick_coin)

# again, we had 10,178 trick coins out of our 11,148 simulations that were all heads

mean(all_heads$trick_coin)

# that is 91.29889% of coins drawn are trick coins given that we flipped 10 heads in a row.  
# 91.18% is what we calculated using bayes theorum


ggplot(all_heads, aes(x = factor(trick_coin), fill = factor(trick_coin))) +
  geom_bar() +
  labs(x = 'Coin', y = 'Count', fill = 'Trick Coin = 1', title = 'Number of Coins of Each Type, Given 10 heads')

ggplot(all_heads, aes(x = 0, fill = factor(trick_coin))) +
  geom_bar(position = 'fill') +
  geom_hline(yintercept = 0.9118433) +
  labs(x = "", y = "Proportion of Trick and Normal Coins", fill = "Trick Coin = 1", title = "Coin Type Given 10 Heads in a Row", caption = "The black line is our expected probability of 0.9118.
       Our calculated probability is almost exactly the same as our simulated probability")


####  Took a data camp course on probability and found out a much easier way to do this.  

# actual calculation of probability
prob_fair <- dbinom(10, 10, .5)
prob_trick <- dbinom(10, 10, 1)

#bayes theorem 

prob_trick*.01/(prob_trick*.01 + prob_fair*.99)

## random simulation
fair <- sum(rbinom(990000, 10, .5) == 10)
trick <- sum(rbinom(10000, 10, 1)==10)

trick / (fair + trick)


