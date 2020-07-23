library(obstgarten)

T1 <- Baum$new()
G1 <- T1$root
G1$points <- data.frame(x = c(1,2,3), y = c(4, 5, 6))
G1$points
G1

G2 <- Gabel$new()
G2$points <- data.frame(x = c(1,2), y = c(4, 5))
G2$points
G2

G3 <- Gabel$new()
G3$points <- data.frame(x = c(3), y = c(6))
G3$points
G3

T1$append(1L, G2, G3) # append to "1"
G1
G1$childL
G1$childR

T1$obstkorb()
df <- as.data.frame(generate_sin_data(200))
colnames(df) <- c("1", "y")
T2 <- cart_greedy(as.data.frame(sin_data, col.names=c("1", "y")))
