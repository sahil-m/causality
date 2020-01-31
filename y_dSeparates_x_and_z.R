population_size = 10000
population_mean = 50
population_sd = 10
set.seed(123)
population_x = rnorm(population_size, population_mean, population_sd)

# # y = y0 + coeff_x * x + random_error
# population_y_0 = 5
# population_coeff_x = 2
# set.seed(123)
# population_y = population_y_0 + (population_coeff_x * population_x) + rnorm(population_size)
population_y = cut(population_x, quantile(population_x, probs = seq(0,1,0.25)), c('a','b','c','d'), include.lowest = TRUE)

# # z = z0 + coeff_y * y + random_error
# population_z_0 = 3
# population_coeff_y = 1
# set.seed(123)
# population_y = population_z_0 + (population_coeff_y * population_y) + rnorm(population_size)

set.seed(123)
population_xyz = data.frame(x=population_x, y=population_y) %>% 
  group_by(y) %>% 
  mutate(z = rnorm(n(), runif(1,1,100), runif(1,1,10))) %>% 
  ungroup()

cor(population_xyz$x, population_xyz$z, method = "spearman")
ggplotly(
  ggplot(population_xyz, aes(x=x, y=z)) +
    geom_point() +
    geom_smooth(method=lm)
)

population_xyz %>% 
  group_by(y) %>% 
  summarize(correlation = cor(x, z, method = "spearman"))
ggplotly(
  ggplot(population_xyz, aes(x=x, y=z, color=y)) +
    geom_point() +
    geom_smooth(method=lm)
)


sample_size = 100
