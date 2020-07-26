library(obstgarten)

# ----
T1 <- Baum$new()
G1 <- T1$root
G1$points <- matrix(1:6, ncol=2, dimnames=list(NULL, c(1, "y")))
G1$points
G1

G2 <- Gabel$new()
G2$points <- matrix(c(1:2, 4:5), ncol=2, dimnames=list(NULL, c(1, "y")))
G2$points
G2

G3 <- Gabel$new()
G3$points <- matrix(c(3, 6), ncol=2, dimnames=list(NULL, c(1, "y")))
G3$points
G3

T1$append(1L, G2, G3) # append to "1"
G1
G2
G3
stopifnot(identical(G1$childL, G2)) # 2L
stopifnot(identical(G1$childR, G3)) # 3L
T1$obstkorb()

# ensure labels are not updated from argument label
T1$append(2L, Gabel$new(), Gabel$new())
stopifnot(G2$childL$label == 4L) # 4L
stopifnot(G2$childR$label == 5L) # 5L

# ----
M <- generate_sin_data(150, sigma=0.2)
dimnames(M) <- list(NULL, c(1, "y"))
T2 <- cart_greedy(M, depth=5)
T2$validate()

P <- T2$partition(T2$root, 1)
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
  geom_function(data=data.frame(x = runif(200, min=0, max=1)), fun = function(x) sin(2 * pi * x))
