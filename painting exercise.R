library(tidyverse)
library(ggplot2)

# 100 paintings, randomly view 1 painting at a time and select or pass, what is optimal number of paintings to view before picking the next painting that is higher than all you have seen

order <- 1:100
win <- NA
win_prob <- NA

for (s in 1:100) {

for (l in 1:10000) {


value <- sample(1:100, size = 100, replace = FALSE)

df <- data.frame(order, value)

for (i in df$order) {
  df$max_so_Far[i] <- max(df$value[1:(i-1)])
}

sample <- s

sample_max <-max(df$value[1:sample])
sample_max


df <- df %>% 
  mutate(highest_value_pos = ifelse(value == 100, order, 0),
         higher_than_sample = ifelse(value > sample_max, 1, 0))


first_after_sample <- min(df$order[df$higher_than_sample==1])
first_after_sample

chosen_painting <- df$value[first_after_sample]
chosen_painting

win[l] <- ifelse(chosen_painting == 100, 1, 0)

}


win[is.na(win)] <- 0

win_prob[s] <- mean(win)

}

strategy <- data.frame(order, win_prob)

ggplot(strategy, aes(x = order, y = win_prob)) +
  geom_line() +
  geom_smooth()
