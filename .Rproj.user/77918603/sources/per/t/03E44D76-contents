library(rlang)
library(dplyr)
library(ggplot2)

x <- 1:10

call2(median, x, na.rm = TRUE)
call2(expr(median), x, na.rm = TRUE)
call2(median, expr(x), na.rm = TRUE)
call2(expr(median), expr(x), na.rm = TRUE)


call_standardise(quote(mean(1:10, na.rm = TRUE)))
#> mean(x = 1:10, na.rm = TRUE)
call_standardise(quote(mean(n = T, 1:10)))
#> mean(x = 1:10, n = T)
call_standardise(quote(mean(x = 1:10, , TRUE)))
#> mean(x = 1:10, , TRUE)




by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(mpg))

ggplot(by_cyl, aes(cyl, mean)) + geom_point()

# Compare and contrast the following two functions.
# Can you predict the output before running them?
  
f1 <- function(x, y) {
    exprs(x = x, y = y)
}
f2 <- function(x, y) {
  enexprs(x = x, y = y)
}

f1(a + b, c + d)
f2(a + b, c + d)


# The following two calls print the same, but are actually different:
  
  (a <- expr(mean(1:10)))
#> mean(1:10)
(b <- expr(mean(!!(1:10))))
#> mean(1:10)
identical(a, b)
#> [1] FALSE
