library(matlib) # for inv()
library(dplyr)
library(ggplot2)

# Replicates the plot in 
#  https://onlinecourses.science.psu.edu/stat508/book/export/html/696
# with guidance from the lecture notes at
#  https://web.stanford.edu/class/stats202/content/lec9.pdf

p0 <- 0.651
p1 <- 0.349
m0 <- matrix(c(-0.4038, -0.1937), 2, 1)
m1 <- matrix(c(0.7533, 0.3613), 2, 1)
S0 <- matrix(c(1.6790, -0.0461, -0.0461, 1.5985), 2, 2)
S0i <- inv(S0)
S1 <- matrix(c(2.0114, -0.3334, -0.3334, 1.7910), 2, 2)
S1i <- inv(S1)

d0 <- function(x) {
  log(p0) - 
    0.5 * t(m0) %*% S0i %*% m0 +
    t(x) %*% S0i %*% m0 -
    0.5 * t(x) %*% S0i %*% x -
    0.5 * log(abs(det(S0)))
}

d1 <- function(x) {
  log(p1) - 
    0.5 * t(m1) %*% S1i %*% m1 +
    t(x) %*% S1i %*% m1 -
    0.5 * t(x) %*% S1i %*% x -
    0.5 * log(abs(det(S1)))
}

x.coords <- seq(-6, 6, by = 0.01)
y.coords <- seq(-6, 6, by = 0.01)
grid <- expand.grid(x = x.coords, y = y.coords)

d0.coords <- apply(grid, 1, function(x) {
  x <- as.matrix(x, 2, 1)
  d0(x)
})
d1.coords <- apply(grid, 1, function(x) {
  x <- as.matrix(x, 2, 1)
  d1(x)
})

grid <- as.data.frame(grid)
grid <- mutate(grid, d0 = d0.coords, d1 = d1.coords)
grid <- mutate(grid, response = ifelse(d0 > d1, -1,
                                       ifelse(d1 > d0, 1, 0)))

ggplot(grid, aes(x = x, y = y, color = response)) + 
  geom_point()

