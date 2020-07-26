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
M <- generate_sin_data(100, sigma=0.2)
dimnames(M) <- list(NULL, c(1, "y"))
T2 <- cart_greedy(M)
T2$validate()
