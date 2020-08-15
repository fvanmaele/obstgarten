library(obstgarten)

n <- 150
M <- generate_sin_data(n, sigma=0.2)
dimnames(M) <- list(NULL, c(1, "y"))
T2 <- cart_greedy(M, depth=5, threshold=1)
T2$validate()

P <- cart_partition(T2$root, 1)
x <- unname(P$part[, 1])
x <- c(min(M[, 1]), x)
y <- P$y

x <- c(x, max(M[, 1]))
x1 <- numeric(0)
x2 <- numeric(0)
for (i in seq_along(x)) {
  if (i == length(x)) break
  x1 <- c(x1, x[[i]])
  x2 <- c(x2, x[[i+1]])
}

ggplot() +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"),
               size=1, data=data.frame(x1 = x1, x2 = x2, y1 = y, y2 = y)) +
  geom_point(data=data.frame(x= M[, 1], y=M[, 2]), mapping=aes(x=x, y=y), size=1) +
  geom_function(data=data.frame(x = runif(n, min=0, max=1)), fun = function(x) sin(2 * pi * x))
