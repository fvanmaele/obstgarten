library(obstgarten)

G1 <- Gabel$new()
G1$label <- 1L
G1$partition <- list(c(1,5))
G1$partition
G1

T1 <- Baum$new(G1)
G2 <- Gabel$new()
G2$partition <- list(c(1,3))
G2$partition
G2

G3 <- Gabel$new()
G3$partition <- list(c(3,5))
G3$partition
G3

T1$append(1L, G2, G3) # append to "1"
G1
G1$childL
G1$childR

T1$obstkorb()
