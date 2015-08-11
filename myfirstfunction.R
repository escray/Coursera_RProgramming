add2 <- function(x, y) {
        x + y
}

above10 <- function(x) {
        use <- x > 10
        x[use]
}

above <- function(x, n = 10) {
        use <- x > n
        x[use]
}

columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for (i in 1:nc) {
                means[i] <- mean(y[, i], na.rm = removeNA)
        }
        means
}

y <- 10

f <- function(x) {
        y <- 2
        y ^ 2 + g(x)
}

g <- function(x) {
        x * y
}

make.NegLogLik <- function(data, fixed=c(FALSE, FALSE)) {
        params <- fixed
        print(params)
        function(p) {
                params[!fixed] <- p
                mu <- params[1]
                print(paste("mu : ", mu))
                
                sigma <- params[2]
                print(paste("sigma : ", sigma))
                a <- -0.5*length(data)*log(2*pi*sigma^2)
                print(paste("a : ",  a))
                b <- -0.5*sum((data-mu)^2)/(sigma^2)
                print(paste("b : ", b))
                -(a+b)
        }
}

cube <- function(x, n) {
        x ^ 3
}
cube(3)

x <- 1:10
if( x > 5) {
        x <- 0
}

f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}
z <- 10
f(3)

x <- 5
y <- if (x < 3) {
        NA
} else {
        10
}
y

h <- function(x, y = NULL, d = 3L) {
        z <- cbind(x, d)
        if (!is.NULL(y)) 
                z <- z + y
        else
                z <- z + f
        g <- x + y/z
        if (d == 3L)
                return (g)
        g <- g + 10
        g
}