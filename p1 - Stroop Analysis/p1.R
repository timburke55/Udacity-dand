library(tidyverse)

stroop <- read.csv("stroopdata.csv")

stroop_tidy <- stroop %>%
  rownames_to_column() %>%
  gather(test, time, Congruent:Incongruent) %>%
  rename(participant = rowname)

stroop_tidy$participant <- as.numeric(stroop_tidy$participant)
stroop_tidy$test <- as.factor(stroop_tidy$test)

ggplot(stroop_tidy, aes(x= time)) +
  geom_histogram(aes(fill = test), binwidth = 3) + facet_wrap(~ test)

ggplot(stroop_tidy, aes(x = test, y = time)) +
  geom_boxplot(aes(fill = test))  

mean_congruent <- stroop_tidy %>%
  filter(test == "Congruent") %>%
  with(mean(time)) %>%
  signif(digits = 3)

mean_incongruent <- stroop_tidy %>%
  filter(test == "Incongruent") %>%
  with(mean(time)) %>%
  signif(digits = 3)


summarise(stroop) 

stroop_tidy %>%
  group_by(test) %>%
  summarise(mean(time))

result <- t.test(time_congruent$time, time_incongruent$time, mu = 0, alternative = "two.sided", paired = TRUE, conf.level = 0.95)

t.test(time_incongruent$time, time_congruent$time, mu = mean_time_difference, alternative = "two.sided", paired = TRUE, conf.level = 0.95)
