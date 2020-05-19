size = 1000
x = 1
set.seed(123)
N = sample(1:100, size, replace = TRUE)
n = c()
for (i in 1:size) {
  n = c(n, sample(1:N[i], 1))
}

df = data.frame(N=N,
                n=n,
                log_operation = x * log(N/n, base = 10),
                division_operation = x/(n/N))

sum((df$N - df$n) < 0)


ggplot(df, aes(x=log_operation, y=division_operation)) +
  geom_point()
