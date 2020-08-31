##### Setup -----
## Load required packages
library(gpmss)   ## For GP model inference
library(margins) ## For GLM marginal effects
library(ggplot2) ## For plotting
## Source custom functions and ensure required directories exist
source("code/util.R")
ensure_directories_exist("plots")
## Customize plotting aesthetics
theme_set(theme_bw() + theme(panel.grid = element_blank()))


##### Simulate data -----
set.seed(20200830)                             ## Set seed for reproducibility
n <- 250                                       ## Set number of observations
x <- runif(n, min = -pi, max = pi)             ## Draw predictor values
f <- function(x) 2 * sin(2 * x) + x            ## Define true unknown function,
sigma <- function(f) plogis(f)                 ## prob. of positive response,
dfdx  <- function(x) 4 * cos(2 * x) + 1        ## deriv. of f wrt x,
dsfdf <- function(f) plogis(f) * (1-plogis(f)) ## deriv. of sigma(f) wrt f,
dsfdx <- function(x) dfdx(x) * dsfdf(f(x))     ## & deriv. of sigma(f) wrt x
y_r   <- f(x) + rnorm(n)                       ## and regression outcomes
y_c   <- sapply(sigma(f(x)), function(p) {     ## Draw classification outcomes
    sample(c(1, -1), 1, prob = c(p, 1-p))
})


##### Example plots for regression -----
## Fit models
gprmod <- GPR$new(y_r ~ x, optimize = TRUE)
olsmod <- lm(y_r ~ x)
## Plot prior mean and 95% CI
fbar <- gprmod$prior_mean
K    <- gprmod$covfun$cov(gprmod$X)
fhi  <- qnorm(0.975, mean = fbar, sd = sqrt(diag(K)))
flo  <- qnorm(0.025, mean = fbar, sd = sqrt(diag(K)))
dat  <- data.frame(x = x, y = y_r, f = f(x), fbar = fbar, fhi = fhi, flo = flo)
ggplot(data = dat, mapping = aes(x = x, y = fbar)) +
    geom_ribbon(aes(ymin = flo, ymax = fhi), fill = "#8787875f") +
    geom_line() +
    geom_line(mapping = aes(x = x, y = f), linetype = "dashed") +
    geom_point(aes(x = x, y = y), shape = "cross", alpha = 0.25) +
    ylab("f")
ggsave("plots/gpr-example-prior.pdf", width = 3, height = 2)
## Plot posterior mean and 95% CI
fbar <- gprmod$post_mean
fhi  <- qnorm(0.975, mean = fbar, sd = sqrt(diag(gprmod$post_cov)))
flo  <- qnorm(0.025, mean = fbar, sd = sqrt(diag(gprmod$post_cov)))
dat  <- data.frame(x = x, y = y_r, f = f(x), fbar = fbar, fhi = fhi, flo = flo)
ggplot(data = dat, mapping = aes(x = x, y = fbar)) +
    geom_ribbon(aes(ymin = flo, ymax = fhi), fill = "#8787875f") +
    geom_line() +
    geom_line(mapping = aes(x = x, y = f), linetype = "dashed") +
    geom_point(aes(x = x, y = y), shape = "cross", alpha = 0.25) +
    ylab("f")
ggsave("plots/gpr-example-posterior.pdf", width = 3, height = 2)


##### Example plots for classification -----
## Fit models
gpcmod <- GPCLA$new(y_c ~ x, optimize = TRUE)
glmmod <- glm(ifelse(y_c == -1, 0, 1) ~ x, family = "binomial")
## Plot prior mean and 95% CI
fbar <- gpcmod$prior_mean
K    <- gpcmod$covfun$cov(gpcmod$X)
fhi  <- qnorm(0.975, mean = fbar, sd = sqrt(diag(K)))
flo  <- qnorm(0.025, mean = fbar, sd = sqrt(diag(K)))
dat  <- data.frame(x = x, y = y_c, f = f(x), fbar = fbar, fhi = fhi, flo = flo)
ggplot(data = dat, mapping = aes(x = x, y = fbar)) +
    geom_ribbon(aes(ymin = flo, ymax = fhi), fill = "#8787875f") +
    geom_line() +
    geom_line(mapping = aes(x = x, y = f), linetype = "dashed") +
    geom_rug(data = subset(dat, y == -1), sides = "b", alpha = 1/8) +
    geom_rug(data = subset(dat, y ==  1), sides = "t", alpha = 1/8) +
    ylab("f")
ggsave("plots/gpc-example-prior.pdf", width = 3, height = 2)
## Plot posterior mean and 95% CI
fbar <- gpcmod$post_mean
fhi  <- qnorm(0.975, mean = fbar, sd = sqrt(diag(gpcmod$post_cov)))
flo  <- qnorm(0.025, mean = fbar, sd = sqrt(diag(gpcmod$post_cov)))
dat  <- data.frame(x = x, y = y_c, f = f(x), fbar = fbar, fhi = fhi, flo = flo)
ggplot(data = dat, mapping = aes(x = x, y = fbar)) +
    geom_ribbon(aes(ymin = flo, ymax = fhi), fill = "#8787875f") +
    geom_line() +
    geom_line(mapping = aes(x = x, y = f), linetype = "dashed") +
    geom_rug(data = subset(dat, y == -1), sides = "b", alpha = 1/8) +
    geom_rug(data = subset(dat, y ==  1), sides = "t", alpha = 1/8) +
    ylab("f")
ggsave("plots/gpc-example-posterior.pdf", width = 3, height = 2)


##### Get marginal effects -----
## Analytical AME of x on f is 1
(true_ape <- 1)
## Get true sample APE: 1.075
(true_sape <- mean(dfdx(x)))
## Get GP regression estimate of sample AME of x on f: 1.099 [0.919, 1.278]
(gpr_ame <- gprmod$margins()$average_marginal_effects)
## Compare OLS estimate of sample AME of x on f: 0.687 [0.573, 0.801]
(ols_ame <- summary(margins(olsmod)))
## Get GP classifier estimate of sample AME of x on f: 0.855 [0.428, 1.282]
(gpc_ape <- gpcmod$margins()$average_marginal_effects)
## Compare GLM estimate of sample AME of x on f: 0.614 [0.442, 0.786]
(glm_ape <- summary(margins(glmmod, type = "link")))
## Get approx. theoretical AME of x on sigma(f): approx 0.146
(true_ame <- sum(1e-6 * (1 / (2*pi)) * dsfdx(seq(-pi, pi, 1e-6))))
## Get true sample AME: 0.1496
(true_same <- mean(dsfdx(x)))
## Get GP classifier estimate of sample AME: 0.133 [0.081, 0.169]
(gpc_ame <- gpcmod$margins(force = TRUE, type = "response")$average_marginal_effects)
## Compare GLM sample AME: 0.118 [0.099, 0.137]
(glm_ame <- summary(margins(glmmod, type = "response")))
## Plot AME of x on f(x)
models <- c("GP regression", "OLS", "GP classification", "Logit")
dat <- data.frame(
    Model = factor(models, levels = models),
    AME   = c(gpr_ame$Mean, ols_ame$AME, gpc_ape$Mean, glm_ape$AME),
    Low   = c(gpr_ame$LB, ols_ame$lower, gpc_ape$LB, glm_ape$lower),
    High  = c(gpr_ame$UB, ols_ame$upper, gpc_ape$UB, glm_ape$upper)
)
ggplot(data = dat, mapping = aes(x = Model, y = AME)) +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
    # geom_hline(yintercept = 1.075, linetype = "dotted", alpha = 0.5) +
    geom_pointrange(aes(ymin = Low, ymax = High)) +
    theme(axis.title = element_blank())
ggsave("plots/example-ame-link.pdf", width = 4, height = 2)
dat <- data.frame(
    Model = c("GP classification", "Logit"),
    AME   = c(gpc_ame$Mean, glm_ame$AME),
    Low   = c(gpc_ame$LB, glm_ame$lower),
    High  = c(gpc_ame$UB, glm_ame$upper)
)
ggplot(data = dat, mapping = aes(x = Model, y = AME)) +
    geom_hline(yintercept = 0.146, linetype = "dashed", alpha = 0.5) +
    # geom_hline(yintercept = 0.1496, linetype = "dotted", alpha = 0.5) +
    geom_pointrange(aes(ymin = Low, ymax = High)) +
    theme(axis.title = element_blank())
ggsave("plots/example-ame-prob.pdf", width = 2, height = 2)
