library(obstgarten)

a <- TreeNode$new()
a$add_left_child(TreeNode$new())
a$add_right_child(TreeNode$new())
a$left_child$add_left_child(TreeNode$new())
a$left_child$add_right_child(TreeNode$new())
b_tree1 <- Tree$new("binary tree 1", a)
b_tree1
a
a$left_child$left_child

create_tree <- function(node, depth){
  if(depth > 0){
    node$add_left_child(TreeNode$new())
    node$add_right_child(TreeNode$new())
    create_tree(node$get_left_child(), depth - 1)
    create_tree(node$get_right_child(), depth - 1)
  }
}
b <- TreeNode$new()
create_tree(b, 10)
b_tree2 <- Tree$new("binary tree 2", b)
b_tree2
b

# ------

set.seed(0)
n <- 200
x <- runif(n)
y <- 1 + 2*x + rnorm(n, mean = 0, sd = 0.5)
y <- sin(2*pi*x) + rnorm(n, mean = 0, sd = 0.2**2)
plot(x,y)

create_regression_tree <- function(node, x, y, depth){
  if(depth > 0){
    s = mean(x)
    node$s = s
    node$add_left_child(TreeNode$new())
    node$add_right_child(TreeNode$new())
    create_regression_tree(node$get_left_child(), x[x<s], y[x<s], depth - 1)
    create_regression_tree(node$get_right_child(), x[x>=s], y[x>=s], depth - 1)
  }
  else{
    node$y = mean(y)
  }
}

b <- TreeNode$new()
create_regression_tree(b, x, y, 3)
b_regression_tree <- Tree$new("binary regression tree", b)
b_regression_tree
b
