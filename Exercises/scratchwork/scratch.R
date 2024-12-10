# a pretty unorganized file of random problems from the book, most of the functions i was testing are here

x <- runif(1e6)
y <- list(x, x, x)


library(lobstr)
obj_addr(x)
#> [1] "0x7fe11b31b1e8"
obj_addr(y)
#> [1] "0x7fe11b31b1e8"

mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")


f <- function(a) {
  a
}

x <- c(1, 2, 3)
cat(tracemem(x), "\n")
z <- f(x)
untracemem(x)



c(c(1, 2), c(3, 4))

# Define the custom operator %messagelog%
`%messagelog%` <- function(func, message) {
  # Define the log file path
  log_file <- "function_log.txt"
  
  # Capture the current timestamp
  timestamp <- Sys.time()
  
  # Run the function and capture its result
  result <- func()
  
  # Append the timestamp and message to the log file
  cat(paste(timestamp, "-", message, "\n"), file = log_file, append = TRUE)
  
  # Return the function result
  return(result)
}


a <- 1:3
attr(a, "x") <- "abcdef"
attr(a, "x")
#> [1] "abcdef"

attr(a, "y") <- 4:6
str(attributes(a))
#> List of 2
#>  $ x: chr "abcdef"
#>  $ y: int [1:3] 4 5 6

# Or equivalently
a <- structure(
  1:3, 
  x = "abcdef",
  y = 4:6
)
str(attributes(a))
#> List of 2
#>  $ x: chr "abcdef"
#>  $ y: int [1:3] 4 5 6
#>  


installed.packages()


# Two scalar arguments specify row and column sizes
x <- matrix(1:6, nrow = 2, ncol = 3)
x
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6

# One vector argument to describe all dimensions
y <- array(1:12, c(2, 3, 2))
y
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    9   11
#> [2,]    8   10   12

# You can also modify an object in place by setting dim()
z <- 1:6
dim(z) <- c(3, 2)
z
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6
#> 
#> 
#> 



x <- factor(c("a", "b", "b", "a"))
x
#> [1] a b b a
#> Levels: a b

typeof(x)
#> [1] "integer"
attributes(x)
#> $levels
#> [1] "a" "b"
#> 
#> $class
#> [1] "factor"
#> 

y <- factor(c(1 , "b", "b", "a"))
y
#> [1] a b b a
#> Levels: a b

typeof(y)
#> [1] "integer"
attributes(y)
#> $levels
#> [1] "a" "b"
#> 
#> $class
#> [1] "factor"
#> 
#> 


date <- as.Date("1970-02-01")
unclass(date)
#> [1] 31
date <- as.Date("2003-04-01")
unclass(date)/365.25
now_ct <- as.POSIXct("2024-10-22 22:00", tz = "UTC")
now_ct
typeof(now_ct)
#> [1] "double"
attributes(now_ct)
structure(now_ct, tzone = "Asia/Tokyo")
#> [1] "2018-08-02 07:00:00 JST"
structure(now_ct, tzone = "America/New_York")
#> [1] "2018-08-01 18:00:00 EDT"
structure(now_ct, tzone = "America/Los_Angeles")
Sys.time()



df1 <- data.frame(x = 1:3, y = letters[1:3])
typeof(df1)
attributes(df1)

x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()

f2 <- function(a, b) {
  a * 10
}
f2(10, stop("This is an error!"))

attr(f02, "srcref")

formals(sum)
#> NULL
body(sum)
#> NULL
environment(sum)
#> NULL

args <- list(1:10, na.rm = TRUE)
do.call(mean, args)
#> [1] 5.5
objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)
x <- runif(100)

sqrt(mean(square(deviation(x))))
#> [1] 0.274
out <- deviation(x)
out <- square(out)
out <- mean(out)
out <- sqrt(out)
out
#> [1] 0.274
library(magrittr)

x %>%
  deviation() %>%
  square() %>%
  mean() %>%
  sqrt()
#> [1] 0.274
x <- 10
g01 <- function() {
  x <- 20
  x
}

g01()
x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
g02()
#> [1] 1 2
x <- 2
g03 <- function() {
  y <- 1
  c(x, y)
}
g03()
#> [1] 2 1

# And this doesn't change the previous value of y
y
#> [1] 20
x <- 1
g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
g04()
g07 <- function(x) x + 1
g08 <- function() {
  g07 <- function(x) x + 100
  g07(10)
}
g08()
#> [1] 110
g12 <- function() x + 1
x <- 15
g12()
#> [1] 16

x <- 20
g12()
#> [1] 21
environment(g12) <- emptyenv()
g12()
#> Error in x + 1: could not find function "+"


c <- 10
c(c = c)
f <- function(x) {
  f <- function(x) {
    f <- function() {
      x ^ 2
    }
    f() + 1
  }
  f(x) * 2
}
f(10)

h01 <- function(x) {
  10
}
h01(stop("This is an error!"))
#> [1] 10

h06 <- function(x = 10) {
  list(missing(x), x)
}
str(h06())
#> List of 2
#>  $ : logi TRUE
#>  $ : num 10
str(h06(10))
#> List of 2
#>  $ : logi FALSE
#>  $ : num 10

x_ok <- function(x) {
  !is.null(x) & length(x) == 1 & x > 0
}

x_ok(NULL)
#> logical(0)
x_ok(1)
#> [1] TRUE
x_ok(1:3)
#> [1] FALSE FALSE FALSE

f2 <- function(x = z) {
  z <- 100
  x
}
f2()

y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
}
f1()
y
show_time <- function(x = stop("Error!")) {
  stop <- function(...) Sys.time()
  print(x)
}
show_time()
#> [1] "2021-02-21 19:22:36 UTC"


sum(1, 2, 3)
#> [1] 6
mean(1, 2, 3)
#> [1] 1

sum(1, 2, 3, na.omit = TRUE)
#> [1] 7
mean(1, 2, 3, na.omit = TRUE)
#> [1] 1
plot(1:10, col = "red", pch = 20, xlab = "x", col.lab = "blue")

j01 <- function(x) {
  if (x < 10) {
    0
  } else {
    10
  }
}
j01(5)
#> [1] 0
j01(15)
#> [1] 10
j02 <- function(x) {
  if (x < 10) {
    return(0)
  } else {
    return(10)
  }
}

capture.output2 <- function(code) {
  temp <- tempfile()
  on.exit(file.remove(temp), add = TRUE, after = TRUE)
  
  sink(temp)
  on.exit(sink(), add = TRUE, after = TRUE)
  
  force(code)
  readLines(temp)
}
capture.output2(cat("a", "b", "c", sep = "\n"))
#> [1] "a" "b" "c"

rebind <- function(name, value, env = caller_env()) {
  if (identical(env, empty_env())) {
    stop("Can't find `", name, "`", call. = FALSE)
  } else if (env_has(env, name)) {
    env_poke(env, name, value)
  } else {
    rebind(name, value, env_parent(env))
  }
}
rebind("a", 10)
#> Error: Can't find `a`
a <- 5
rebind("a", 10)
a
#> [1] 10

f2 <- function(..., env = caller_env()) {
  while (!identical(env, empty_env())) {
    if (success) {
      # success case
      return()
    }
    # inspect parent
    env <- env_parent(env)
  }
  
  # base case
}
g <- function(x) {
  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}


f1 <- function(x1) {
  f2 <- function(x2) {
    f3 <- function(x3) {
      x1 + x2 + x3
    }
    f3(3)
  }
  f2(2)
}
f1(1)

f <- function() g()
g <- function() h()
h <- function() stop("This is an error!")

f()
h <- function() stop("This is an error!", call. = FALSE)
f()
h <- function() abort("This is an error!")
f()


fw <- function() {
  cat("1\n")
  warning("W1")
  cat("2\n")
  warning("W2")
  cat("3\n")
  warning("W3")
}
f1 <- function(x) {
  log(x)
  10
}
f1("x")
#> Error in log(x): non-numeric argument to mathematical function
f2 <- function(x) {
  try(log(x))
  10
}
f2("a")
#> Error in log(x) : non-numeric argument to mathematical function
#> [1] 10

cnd <- catch_cnd(stop("An error"))
str(cnd)

f3 <- function(x) {
  tryCatch(
    error = function(cnd) NA,
    log(x)
  )
}

f3("x")
#> [1] NA
library(rlang)

tryCatch(
  message = function(cnd) cat("Caught a message!\n"), 
  {
    message("Someone there?")
    message("Why, yes!")
  }
)
#> Caught a message!

withCallingHandlers(
  message = function(cnd) cat("Caught a message!\n"), 
  {
    message("Someone there?")
    message("Why, yes!")
  }
)

catch_cnd(stop("An error"))
catch_cnd(abort("An error"))
show_condition <- function(code) {
  tryCatch(
    error = function(cnd) "error",
    warning = function(cnd) "warning",
    message = function(cnd) "message",
    {
      code
      NULL
    }
  )
}

show_condition(stop("!"))
show_condition(10)
show_condition(warning("?!"))
show_condition({
  10
  message("?")
  warning("?!")
})

withCallingHandlers(
  message = function(cnd) message("b"),
  withCallingHandlers(
    message = function(cnd) message("a"),
    message("c")
  )
)
#> b
#> a
#> b
#> c

message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}
message2error <- function(code) {
  tryCatch(code, message = function(e) stop(e))
}

bottles_of_beer <- function(i = 99) {
  message(
    "There are ", i, " bottles of beer on the wall, ", 
    i, " bottles of beer."
  )
  while(i > 0) {
    tryCatch(
      Sys.sleep(1),
      interrupt = function(err) {
        i <<- i - 1
        if (i > 0) {
          message(
            "Take one down, pass it around, ", i, 
            " bottle", if (i > 1) "s", " of beer on the wall."
          )
        }
      }
    )
  }
  message(
    "No more bottles of beer on the wall, ", 
    "no more bottles of beer."
  )
}

randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(mean)
randomise(sum)

x <- list(
  list(1, c(3, 9)),
  list(c(3, 6), 7, c(4, 7, 6))
)

triple <- function(x) x * 3
map(x, map, .f = triple)

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))

simple_reduce <- function(x, f) {
  out <- x[[1]]
  for (i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}


integrate(sin, 0, pi)
#> 2 with absolute error < 2.2e-14
str(uniroot(sin, pi * c(1 / 2, 3 / 2)))
#> List of 5
#>  $ root      : num 3.14
#>  $ f.root    : num 1.22e-16
#>  $ iter      : int 2
#>  $ init.it   : int NA
#>  $ estim.prec: num 6.1e-05
str(optimise(sin, c(0, 2 * pi)))
#> List of 2
#>  $ minimum  : num 4.71
#>  $ objective: num -1
str(optimise(sin, c(0, pi), maximum = TRUE))
#> List of 2
#>  $ maximum  : num 1.57
#>  $ objective: num 1


# --------------------------------------------------------------

power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
cube <- power1(3)


i <- 0
new_counter2 <- function() {
  i <<- i + 1
  i
}

new_counter3 <-- function() {
  i <- 0
  function() {
    i <- i + 1
    i
  }
}

plot_dev <- function(ext, dpi = 96) {
  force(dpi)
  
  switch(ext,
         eps =  ,
         ps  =  function(path, ...) {
           grDevices::postscript(
             file = filename, ..., onefile = FALSE, 
             horizontal = FALSE, paper = "special"
           )
         },
         pdf = function(filename, ...) grDevices::pdf(file = filename, ...),
         svg = function(filename, ...) svglite::svglite(file = filename, ...),
         emf = ,
         wmf = function(...) grDevices::win.metafile(...),
         png = function(...) grDevices::png(..., res = dpi, units = "in"),
         jpg = ,
         jpeg = function(...) grDevices::jpeg(..., res = dpi, units = "in"),
         bmp = function(...) grDevices::bmp(..., res = dpi, units = "in"),
         tiff = function(...) grDevices::tiff(..., res = dpi, units = "in"),
         stop("Unknown graphics extension: ", ext, call. = FALSE)
  )
}

plot_dev("pdf")
#> function(filename, ...) grDevices::pdf(file = filename, ...)
#> <bytecode: 0x7fe857744590>
#> <environment: 0x7fe8575f6638>
plot_dev("png")
#> function(...) grDevices::png(..., res = dpi, units = "in")
#> <bytecode: 0x7fe85947f938>
#> <environment: 0x7fe859169548>

funs <- list(
  mean = function(x) mean(x, na.rm = TRUE),
  sum = function(x) sum(x, na.rm = TRUE)
)

attach(funs)
#> The following objects are masked from package:base:
#> 
#>     mean, sum
mean <- function(x) stop("Hi!")
detach(funs)

env_bind(globalenv(), !!!funs)
mean <- function(x) stop("Hi!") 
env_unbind(globalenv(), names(funs))


fit_model <- function(df) {
  glm(y ~ x1 + x2 * x3, data = df)
}

models <- transpose(map(datasets, safely(fit_model)))
ok <- map_lgl(models$error, is.null)

# which data failed to converge?
datasets[!ok]

# which models were successful?
models[ok]


fib2 <- memoise::memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})
system.time(fib2(23))
#>    user  system elapsed 
#>   0.009   0.000   0.008

delay_by <- function(f, amount) {
  force(f)
  force(amount)
  
  function(...) {
    Sys.sleep(amount)
    f(...)
  }
}
system.time(runif(100))
#>    user  system elapsed 
#>       0       0       0
system.time(delay_by(runif, 0.1)(100))
#>    user  system elapsed 
#>    0.00    0.00    0.13


# 17 -----------------------
xx <- expr(x + x)
yy <- expr(y + y)

expr(!!xx / !!yy)
#> (x + x)/(y + y)
string_math <- function(x) {
  e <- env(
    caller_env(),
    `+` = function(x, y) paste0(x, y),
    `*` = function(x, y) strrep(x, y)
  )
  
  eval(enexpr(x), e)
}

name <- "Hadley"
string_math("Hello " + name)
#> [1] "Hello Hadley"
string_math(("x" * 2 + "-y") * 3)
#> [1] "xx-yxx-yxx-y"


with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enexpr(expr), df)
}

df <- data.frame(x = 1:3)
a <- 10
with2(df, x + a)
#> [1] 1001 1002 1003

with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enquo(expr), df)
}

with2(df, x + a)
#> [1] 11 12 13
#> 


y <- x * 10
#> Error in eval(expr, envir, enclos): object 'x' not found
library(rlang)
library(lobstr)

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


call_standardise(quote(mean(1:10, na.rm = TRUE)))
#> mean(x = 1:10, na.rm = TRUE)
call_standardise(quote(mean(n = T, 1:10)))
#> mean(x = 1:10, n = T)
call_standardise(quote(mean(x = 1:10, , TRUE)))
#> mean(x = 1:10, , TRUE)

f((1))
`(`(1 + 1)

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

expr_type(expr("a"))
#> [1] "constant"
expr_type(expr(x))
#> [1] "symbol"
expr_type(expr(f(1, 2)))
#> [1] "call"


logical_abbr_rec <- function(x) {
  switch_expr(x,
              constant = FALSE,
              symbol = as_string(x) %in% c("F", "T")
  )
}

logical_abbr_rec(expr(TRUE))
#> [1] FALSE
logical_abbr_rec(expr(T))
#> [1] TRUE

logical_abbr_rec <- function(x) {
  switch_expr(x,
              constant = FALSE,
              symbol = as_string(x) %in% c("F", "T")
  )
}

logical_abbr_rec(expr(TRUE))
#> [1] FALSE
logical_abbr_rec(expr(T))
#> [1] TRUE
logical_abbr(function(x = TRUE) {
  g(x + T)
})

#-------------------------------------

library(rlang)
library(purrr)
library(MASS)

mtcars2 <- subset(mtcars, cyl == 4)

with(mtcars2, sum(vs))
sum(mtcars2$am)

rm(mtcars2)


library(dplyr)
library(ggplot2)

by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(mpg))

ggplot(by_cyl, aes(cyl, mean)) + geom_point()


f1 <- function(x, y) {
  exprs(x = x, y = y)
}
f2 <- function(x, y) {
  enexprs(x = x, y = y)
}
f1(a + b, c + d)
f2(a + b, c + d)

xy <- expr(x + y)
xz <- expr(x + z)
yz <- expr(y + z)
abc <- exprs(a, b, c)

(a <- expr(mean(1:10)))
#> mean(1:10)
(b <- expr(mean(!!(1:10))))
#> mean(1:10)
identical(a, b)
#> [1] FALSE

set_attr <- function(.x, ...) {
  attr <- rlang::list2(...)
  attributes(.x) <- attr
  .x
}

attrs <- list(x = 1, y = 2)
attr_name <- "z"

1:10 %>%
  set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% 
  str()


exec <- function(f, ..., .env = caller_env()) {
  args <- list2(...)
  do.call(f, args, envir = .env)
}

bc <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}
compose <- function(f, g) {
  function(...) f(g(...))
}


















