set.seed(161) # + seed 162, 163, 164 for "conscientiousness" barplot

N = 500
b0 = 0
b1 = .3

con <- rnorm(N, 0, 1)
rerror <- sqrt(1 - b1^2)
y = b1*con + rnorm(N, 0, rerror)
model1 = lm(y ~ con)

df <- as_tibble(cbind(con, y))

ggplot(df, aes(x = con, y = y)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_jitter() +
  theme_classic() +
  ylab("pro-environmental behavior") +
  xlab("conscientiousness") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

ggplot(df, aes(x = round(con, 1))) + 
  geom_bar() +
  xlab("conscientiousness") +
  theme_classic()

ggplot(power_lm$results, aes(x = round(p1_x1, 2))) +
  geom_bar() +
  facet_wrap(~ N.test, nrow = 1) +
  xlab("p values") +
  xlim(-0.01, 0.5) +
  theme_classic()
