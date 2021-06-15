# A basic implementation of Forward mode Automatic Differentiation in R
#


#-------------------------------------------------------------
# Define some operators
#-------------------------------------------------------------
# addition
add <- function(x1, x2) {
    if (is.numeric(x1))
	x1$val <- x1; x1$grad <- 0

    if (is.numeric(x2)) 
	x2$val <- x2; x2$grad <- 0

    return(list(val=x1$val + x2$val, grad=x1$grad + x2$grad))
}


# multiplication
mult<- function(x1, x2) {
    if (is.numeric(x1))
	x1$val <- x1; x1$grad <- 0

    if (is.numeric(x2)) 
	x2$val <- x2; x2$grad <- 0

    return(list(val=x1$val * x2$val, grad=x1$grad*x2$val + x2$grad*x1$val))
}


# cosine
cos. <- function(x1) {
    if (is.numeric(x1)) {
	return(list(val=cos(x1), grad=0))
    }
    return(list(val=cos(x1$val), grad=-sin(x1$val)*x1$grad))
}


# sine
sin. <- function(x1) {
    if (is.numeric(x1))
	return(list(val=sin(x1), grad=0))

    return(list(val=sin(x1$val), grad=cos(x1$val)*x1$grad))
}


# exponential
exp. <- function(x1) {
    if (is.numeric(x1))
	return(list(val=exp(x1), grad=0))

    return(list(val=exp(x1$val), grad=exp(x1$val)*x1$grad))
}



#-------------------------------------------------------------
# Testing
#-------------------------------------------------------------

# define a test function
f <- function(x1, x2) {
    suppressWarnings(
	add(exp.(cos.(mult(2, add(x1, x2)))), sin.(x1))
    )
}

# we need objects which also keep track of gradients
x1 <- list(value=2, grad=0)
x2 <- list(value=2, grad=1)
f(x1, x2)

# check via finite difference
g <- function(x1, x2) exp(cos(2*(x1 + x2))) + sin(x1)
(g(2, 2) - g(2, 2-1e-8)) / 1e-8


round(f(x1, x2)$grad, 6) == round((g(2, 2) - g(2, 2-1e-8)) / 1e-8, 6)
