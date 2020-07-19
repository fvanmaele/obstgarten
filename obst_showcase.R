library(obstgarten)

G1 <- Gabel$new()
G1$label <- 1L
G1$partition <- list(c(1,5))
G1

T1 <- Baum$new(G1)
