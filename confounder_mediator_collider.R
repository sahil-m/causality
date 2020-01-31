# define parameters --------------------------------------------------------------
population_size = 10000


# mediation: price -> customer_loyalty -> quantity -----------------------------------
set.seed(123)
price = rbinom(population_size, 10, .7)

alpha_customer_loyalty = 1
beta_loyalty_price = -.1
customer_loyalty = round((alpha_customer_loyalty + (beta_loyalty_price * price) + rnorm(population_size, 0, .1)), 1)

alpha_quantity = 100
beta_quantity_loyalty = 30
set.seed(234)
quantity = round((alpha_quantity + (beta_quantity_loyalty * customer_loyalty) + rnorm(population_size, 0, 50)), 2)

# customer_loyalty = a + b * price
# q = c + d * customer_loyalty 
#   = c + d * (a + b * price) 
#   = (c + a * d) + d * b * price 
#   = (100 + 1 * 30) + (30 * (-0.1) * price) 
#   = 130 - 3 * price

data_pop = data.frame(customer_loyalty = customer_loyalty, price = price, quantity = quantity)

# ## checking price distribution for different levels of customer_loyalty
# data_pop %>% 
#   split(data_pop$customer_loyalty) %>% 
#   map(~summary(.x$price))
#   
# # data_pop %>% 
# #   group_by(customer_loyalty) %>% 
# #   group_map(~summary(.x$price))
# 
# ggplot(data_pop, aes(x = price, color = customer_loyalty)) +
#   geom_density()

m.reg_mediation_no_z <- lm(quantity ~ price, data = data_pop)
summary(m.reg_mediation_no_z)

m.reg_mediation_with_z <- lm(quantity ~ price + customer_loyalty, data = data_pop)
summary(m.reg_mediation_with_z)

m.reg_mediation_just_z <- lm(quantity ~ customer_loyalty, data = data_pop)
summary(m.reg_mediation_just_z)

# regression computes CORRECT coefficients in all three cases 

data_pop_customer_loyalty0 = dplyr::filter(data_pop, customer_loyalty == 0)
data_pop_customer_loyalty1 = dplyr::filter(data_pop, customer_loyalty == 0)

# (mediation + direct) or confounding: customer_loyalty -> price -> quantity <- customer_loyalty ------------
z_classes = c(0, 1)
z_prob = c(0.7, 0.3)
set.seed(123)
customer_loyalty = sample(z_classes, population_size, replace = TRUE, prob = z_prob)
round(prop.table(table(customer_loyalty))*100)

alpha_price = 10
beta_price_customer_loyalty = -4
set.seed(123)
price = alpha_price + (beta_price_customer_loyalty * customer_loyalty) + rnorm(population_size, 0, 1)

alpha_quantity = 100
beta_quantity_price = -0.7
beta_quantity_customer_loyalty = -5
set.seed(234)
quantity = alpha_quantity + (beta_quantity_price * price) + (beta_quantity_customer_loyalty * customer_loyalty) + rnorm(population_size, 0, 5)

# price = a + b * customer_loyalty
# q = c + d * price + e * customer_loyalty
#   = c + d * (a + b * customer_loyalty) + e * customer_loyalty
#   = (c + a * d) + (d * b + e) * customer_loyalty 
#   = (100 + 10 * (-0.7)) + ((-0.7) * (-4) + (-5)) * customer_loyalty 
#   = 93 - 2.2 * customer_loyalty

data_pop = data.frame(customer_loyalty = as.factor(customer_loyalty), price = price, quantity = quantity) %>% 
  mutate(price_standardized = scale(price),
         quantity_standardized = scale(quantity))

m.reg_confounding_no_z <- lm(quantity ~ price, data = data_pop)
summary(m.reg_confounding_no_z)

m.reg_confounding_with_z <- lm(quantity ~ price + customer_loyalty, data = data_pop)
summary(m.reg_confounding_with_z)

m.reg_confounding_just_z <- lm(quantity ~ customer_loyalty, data = data_pop)
summary(m.reg_confounding_just_z)

ggplotly(
  ggplot(data_pop, aes(x = price, y = quantity, color = customer_loyalty)) +
    geom_point() +
    geom_smooth(method=lm, se=FALSE)
)

ggplotly(
  ggplot(data_pop, aes(x = price, y = quantity)) +
    geom_point() +
    geom_smooth(method=lm, se=FALSE) +
    facet_grid(customer_loyalty ~ .)
)

# mediation: promo -> price -> quantity -----------------------------------
z_classes = c(0, 1)
z_prob = c(0.7, 0.3)
set.seed(123)
customer_loyalty = sample(z_classes, population_size, replace = TRUE, prob = z_prob)

alpha_price = 10
beta_price_promo = -4
set.seed(123)
price = alpha_price + (beta_price_promo * promo) + rnorm(population_size, 0, 1)

alpha_quantity = 100
beta_quantity_price = -0.7
set.seed(234)
quantity = alpha_quantity + (beta_quantity_price * price) + rnorm(population_size, 0, 1)

# price = a + b * promo
# q = c + d * price 
#   = c + d * (a + b * promo) 
#   = (c + a * d) + d * b * promo 
#   = (100 + 10 * (-0.7)) + (-0.7) * (-4) * promo 
#   = 93 + 2.8 * promo

data_pop = data.frame(promo = as.factor(promo), price = price, quantity = quantity)

# ## checking price distribution for different levels of promo
# data_pop %>% 
#   split(data_pop$promo) %>% 
#   map(~summary(.x$price))
#   
# # data_pop %>% 
# #   group_by(promo) %>% 
# #   group_map(~summary(.x$price))
# 
# ggplot(data_pop, aes(x = price, color = promo)) +
#   geom_density()

m.reg_mediation_no_z <- lm(quantity ~ price, data = data_pop)
summary(m.reg_mediation_no_z)

m.reg_mediation_with_z <- lm(quantity ~ price + promo, data = data_pop)
summary(m.reg_mediation_with_z)

m.reg_mediation_just_z <- lm(quantity ~ promo, data = data_pop)
summary(m.reg_mediation_just_z)

# regression computes CORRECT coefficients in all three cases 

data_pop_promo0 = dplyr::filter(data_pop, promo == 0)
data_pop_promo1 = dplyr::filter(data_pop, promo == 0)
