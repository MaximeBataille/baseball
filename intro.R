library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961 : 2001) %>%
  mutate(win_rate = W / G, f_errors = E / G) %>%
  ggplot(aes(f_errors, win_rate)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) +
  geom_point(alpha = 0.5)

#correlation
library(tidyverse)
library(HistData)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  summarize(mean_father = mean(father), sd_father = sd(father), mean_son = mean(son), sd_son = sd(son))

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)


set.seed(0)
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))

#monte-carlo simulation
N <- 1000
R <- replicate(N, {sample_n(galton_heights, 50, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    .$r} )

data.frame(R) %>%
  ggplot(aes(R)) +
  geom_histogram(binwidth = 0.05, color = I("black"))

sd(R)
mean(R)

data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

#assessment
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(run_per_game = R / G, ab_per_game = AB / G) %>%
  summarize(r = cor(run_per_game, ab_per_game)) %>%
  .$r
