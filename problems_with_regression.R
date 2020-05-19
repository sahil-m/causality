# association measure exploration -----------------------------------------
x = c(1,  1,  0,  0,  1,  0,  1,  1,  1)
y = c(1,  1,  0,  0,  0,  0,  1,  1,  1)
cor(x,y)

x = as.character(c(1,  1,  0,  0,  1,  0,  1,  1,  1))
y = as.character(c(1,  1,  0,  0,  0,  0,  1,  1,  1))

table(x, y)

sqrt(chisq.test(table(x,y), correct=FALSE)$statistic/length(x))

sqrt(chisq.test(table(x,y))$statistic/length(x))

library(psych)
phi(table(x, y), digits = 4)

y = as.character(c(1,  1,  0,  0,  0,  0,  1,  1,  1) * -1)
phi(table(x, y), digits = 4)


# causal diagram visualization --------------------------------------------
library(dagitty)
library(ggdag)

g <- dagitty('dag {
    X [pos="0,1"]
    Y [pos="1,1"]
    Z [pos="2,1"]
    W [pos="1,0"]
    T [pos="2,2"]
    
    X -> Y -> Z -> T
    X -> W -> Y -> T
    W -> Z
}')
plot(g)

dag <- dagitty("dag{y <- z -> x}")
ggdag(dag, layout = "circle")

dag = dagify(x ~ z,
             y ~ z,
             exposure = "x",
             outcome = "y")
ggdag(dag, layout = "circle")

# get a tidy data.frame from a dag object
tidy_dag <- tidy_dagitty(dag)

node_parents(tidy_dag, "x")

bigger_dag <- dagify(y ~ x + a + b,
                     x ~ a + b,
                     exposure = "x",
                     outcome = "y")
ggdag(bigger_dag, layout = "circle")
ggdag_paths(bigger_dag)
ggdag_parents(bigger_dag, "x")
ggdag_adjustment_set(bigger_dag)
