## ----setup, include=FALSE-----------------------------------------------------
# Spacing: operator for separating elements in expression. E.g.,
## x ~ y --> x  y ;
## x ~ ~ y --> x    y

# Ellipsis: ldots or cdots. E.g., 
## x[1] + ... + x[n]

# Radicals sqrt(x, y) --> y√x

# Sets: some symbols in set relations are missing:
## supset, supseteq, notsubset, subset, subseteq, notin

# Phantom: command that produces a space in place of an argument (partially covered by omit() (?)). E.g.:
## x + phantom(0) + y --> x +    + y
## x + over(1, vphantom(0)) --> x + 1/  

# Typeface: plain, bold, italic, bolditalic

# Stylechanges: textstyle(x), scriptstyle(x) scriptscript(x)

# linear models: DepVar ~ X0 + X1

# hat, overline, cancel, box, invisible, phantom

library(mathml)

mathout <- function(..., flags=NULL, env=parent.frame())
{
  if(knitr::is_html_output())
    return(mathml(..., flags=c(flags, list(cat=TRUE)), env=env))

  if(knitr::is_latex_output())
    return(mathjax(..., flags=c(flags, list(cat=TRUE)), env=env))

  warning("mathout: no knitr output specified")  
  mathjax(..., flags=c(flags, list(cat=TRUE)), env=env)
}

catmath <- function(term, ...)
{
  s <- substitute(term)
  mathout(canonical(s), ...)
}

## -----------------------------------------------------------------------------
term <- quote(pbinom(k, N, p))
term

## -----------------------------------------------------------------------------
k <- 10
N <- 22
p <- 0.4
eval(term)

## -----------------------------------------------------------------------------
library(mathml)
mathjax(term)

## ---- results='asis'----------------------------------------------------------
mathout(term)

## ---- results='asis'----------------------------------------------------------
term <- quote(1 + -2L + a + abc + "a" + phi + Phi + varphi + hat(b)[i, j]^2L)
mathout(term)

term <- quote(NaN + NA + TRUE + FALSE + Inf + (-Inf))
mathout(term)

## ---- results="asis"----------------------------------------------------------
term <- quote(a - ((b + c)) - d*e + f*(g + h) + i/j + k^(l + m) + (n*o)^{p + q})
mathout(term)

term <- quote(dot(a, b) + frac(1L, nodot(c, d + e)) + dfrac(1L, times(g, h)))
mathout(term)

## -----------------------------------------------------------------------------
term <- quote(a^(b + c))
paste(term)

## ---- results='asis'----------------------------------------------------------
term <- quote(mean(X) %+-% 1.96 * s / sqrt(N))
mathout(term)
term <- quote('%+-%'(mean(X), 1.96 * s / sqrt(N)))
term <- quote(mean(X) %+-% {1.96 * s / sqrt(N)})   # the same
mathout(term)

## ----custom-operators, echo=FALSE---------------------------------------------
op1 <- list(
  "A\\ %*%\\ B"=quote(A %*% B),
  "A\\ %.%\\ B"=quote(A %.% B),
  "A\\ %x%\\ B"=quote(A %x% B),
  "A\\ %/%\\ B"=quote(A %/% B),
  "A\\ %%\\ B"=quote(A %% B),
  "A\\ &\\ B"=quote(A & B),
  "A\\ |\\ B"=quote(A | B),
  "xor(A,\\ B)"=quote(xor(A, B)),
  "!A"=quote(!A),
  "A\\ ==\\ B"=quote(A == B),
  "A\\ <-\\ B"=quote(A <- B))

m1 <- lapply(op1, FUN=mathout, flags=list(cat=FALSE))

op1 <- names(op1)
if(knitr::is_latex_output())
  op1 <- sapply(op1, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op1 <- sapply(op1, FUN=knitr:::escape_html)

op2 <- list(
  "A\\ !=\\ B"=quote(A != B),
  "A\\ ~ B"=quote(A ~ B),
  "A\\ %~~%\\ B"=quote(A %~~% B),
  "A\\ %==%\\ B"=quote(A %==% B),
  "A\\ %=~%\\ B"=quote(A %=~% B),
  "A\\ %prop%\\ B"=quote(A %prop% B),
  "A\\ %in%\\ B"=quote(A %in% B),
  "intersect(A,\\ B)"=quote(intersect(A, B)),
  "union(A,\\ B)"=quote(union(A, B)),
  "crossprod(A,\\ B)"=quote(crossprod(A, B)),
  "is.null(A)"=quote(is.null(A)))

m2 <- lapply(op2, FUN=mathout, flags=list(cat=FALSE))

op2 <- names(op2)
if(knitr::is_latex_output())
  op2 <- sapply(op2, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op2 <- sapply(op2, FUN=knitr:::escape_html)

op3 <- list(
  "A\\ %<->%\\ B"=quote(A %<->% B),
  "A\\ %->%\\ B"=quote(A %->% B),
  "A\\ %<-%\\ B"=quote(A %<-% B),
  "A\\ %up%\\ B"=quote(A %up% B),
  "A\\ %down%\\ B"=quote(A %down% B),
  "A\\ %<=>%\\ B"=quote(A %<=>% B),
  "A\\ %=>%\\ B"=quote(A %=>% B),
  "A\\ %<=%\\ B"=quote(A %<=% B),
  "A\\ %dblup%\\ B"=quote(A %dblup% B),
  "A\\ %dbldown%\\ B"=quote(A %dbldown% B),
  " "="")

m3 <- lapply(op3, FUN=mathout, flags=list(cat=FALSE))

op3 <- names(op3)
if(knitr::is_latex_output())
  op3 <- sapply(op3, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op3 <- sapply(op3, FUN=knitr:::escape_html)

t <- cbind(Operator=op1, Output=m1,
  Operator=op2, Output=m2,
  Operator=op3, Arrow=m3)

knitr::kable(t, caption="Table 1. Custom operators in mathml",
  row.names=FALSE, escape=FALSE)

## ---- results='asis'----------------------------------------------------------
term <- quote(sin(x) + sin(x)^2L + cos(pi/2L) + tan(2L*pi) * expm1(x))
mathout(term)
term <- quote(choose(N, k) + abs(x) + sqrt(x) + floor(x) + ceiling(x))
mathout(term)

## ----base-stats, echo=FALSE---------------------------------------------------
op1 <- list(
  "sin(x)"=quote(sin(x)),
  "cosh(x)"=quote(cosh(x)),
  "tanpi(alpha)"=quote(tanpi(alpha)),
  "asinh(alpha)"=quote(asinh(alpha)),
  "log(p)"=quote(log(p)),
  "log1p(x)"=quote(log1p(x)),
  "logb(x,\\ e)"=quote(logb(x, e)),
  "exp(x)"=quote(exp(x)),
  "expm1(x)"=quote(expm1(x)),
  "choose(n,\\ k)"=quote(choose(n, k)),
  "lchoose(n,\\ k)"=quote(lchoose(n, k)),
  "factorial(n)"=quote(factorial(n)), 
  "lfactorial(n)"=quote(lfactorial(n)),
  "sqrt(x)"=quote(sqrt(x)),
  "mean(X)"=quote(mean(X)),
  "abs(x)"=quote(abs(x)))
m1 <- lapply(op1, FUN=mathout, flags=list(cat=FALSE))

op1 <- names(op1)
if(knitr::is_latex_output())
  op1 <- sapply(op1, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op1 <- sapply(op1, FUN=knitr:::escape_html)

op2 <- list(
  "dbinom(k,\\ N,\\ pi)"=quote(dbinom(k, N, pi)),
  "pbinom(k,\\ N,\\ pi)"=quote(pbinom(k, N, pi)),
  "qbinom(p,\\ N,\\ pi)"=quote(qbinom(p, N, pi)),
  "dpois(k,\\ lambda)"=quote(dpois(k, lambda)),
  "ppois(k,\\ lambda)"=quote(ppois(k, lambda)),
  "qpois(p,\\ lambda)"=quote(qpois(p, lambda)),
  "dexp(x,\\ lambda)"=quote(dexp(x, lambda)),
  "pexp(x,\\ lambda)"=quote(pexp(x, lambda)),
  "qexp(p,\\ lambda)"=quote(qexp(p, lambda)),
  "dnorm(x,\\ mu,\\ sigma)"=quote(dnorm(x, mu, sigma)),
  "pnorm(x,\\ mu,\\ sigma)"=quote(pnorm(x, mu, sigma)),
  "qnorm(alpha/2L)"=quote(qnorm(alpha/2L)),
  "1L\\ -\\ pchisq(x,\\ 1L)"=quote(1L - pchisq(x, 1L)),
  "qchisq(1L\\ -\\ alpha,\\ 1L)"=quote(qchisq(1L-alpha, 1L)),
  "pt(t,\\ N\\ -\\ 1L)"=quote(pt(t, N-1L)),
  "qt(alpha/2L,\\ N\\ -\\ 1L)"=quote(qt(alpha/2L, N-1L)))
m2 <- lapply(op2, FUN=mathout, flags=list(cat=FALSE))

op2 <- names(op2)
if(knitr::is_latex_output())
  op2 <- sapply(op2, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op2 <- sapply(op2, FUN=knitr:::escape_html)

t <- cbind(Function=op1, Output=m1, Function=op2, Output=m2)
knitr::kable(t, caption="Table 2. R functions from base and stats",
  row.names=FALSE, escape=FALSE)

## ---- results='asis'----------------------------------------------------------
sgn <- function(x)
{
  if(x == 0L) return(0L)
  if(x < 0L) return(-1L)
  if(x > 0L) return(1L)
}

mathout(sgn)
mathout(call("<-", quote(sgn(x)), sgn))

## ---- results='asis'----------------------------------------------------------
term <- quote(S[Y]^2L <- frac(1L, N) * sum(Y[i] - mean(Y))^2L)
mathout(term)

term <- quote(log(subsupscript(prod(L[i]), i==1L, N)) <- 
          subsupscript(sum(log(L[i])), i==1L, N))
mathout(term)

## ---- results='asis'----------------------------------------------------------
term <- quote(integrate(sin, 0L, 2L*pi))
mathout(term)
eval(term)

## -----------------------------------------------------------------------------
term <- quote(integrate(lower=0L, upper=2L*pi, sin))
canonical(term)

## ---- results='asis'----------------------------------------------------------
mathout(canonical(term))

## ---- results='asis'----------------------------------------------------------
# v <- 1:3
# mathout(call("t", v))
# 
# A <- matrix(data=11:16, nrow=2, ncol=3)
# B <- matrix(data=21:26, nrow=2, ncol=3)
# term <- call("+", A, B)
# mathout(term)

## ---- results='asis'----------------------------------------------------------
hook(m_A, mean(X)["A"]) ; hook(s2_A, s["A"]^2L) ;
hook(n_A, n["A"])
hook(m_B, mean(X)["B"]) ; hook(s2_B, s["B"]^2L)
hook(n_B, n["B"]) ; hook(s2_p, s["pool"]^2L)

term <- quote(t <- dfrac(m_A - m_B, 
    sqrt(denote(s2_p, frac((n_A - 1L)*s2_A + (n_B - 1L)*s2_B, n_A + n_B - 2L),
                "the pooled variance.") * (frac(1L, n_A) + frac(1L, n_B)))))
mathout(term)

## -----------------------------------------------------------------------------
m_A <- 1.5; s2_A <- 2.4^2; n_A <- 27; m_B <- 3.9; s2_B <- 2.8^2; n_B <- 20
print(eval(term))

## ---- results='asis'----------------------------------------------------------
t <- quote(dfrac(omit_right(mean(D) - mu[0L]), s / sqrt(N)))
mathout(t, flags=list(error='highlight'))
mathout(t, flags=list(error='fix'))

## ----mistakes, echo=FALSE-----------------------------------------------------
op1 <- list(
  "omit_left(a\\ +\\ b)"=quote(omit_left(a + b)),
  "omit_right(a\\ +\\ b)"=quote(omit_right(a + b)),
  "list(quote(a),\\ quote(omit(b)))"=list(quote(a), quote(omit(b))),
  "add_left(a\\ +\\ b)"=quote(add_left(a + b)),
  "add_right(a\\ +\\ b)"=quote(add_right(a + b)),
  "list(quote(a),\\ quote(add(b)))"=list(quote(a), quote(add(b))),
  "instead(a,\\ b)\\ +\\ c"=quote(instead(a, b) + c))

asis <- lapply(op1, FUN=mathout, flags=list(cat=FALSE, error="asis"))
high <- lapply(op1, FUN=mathout, flags=list(cat=FALSE, error="highlight"))
fix  <- lapply(op1, FUN=mathout, flags=list(cat=FALSE, error="fix"))
igno <- lapply(op1, FUN=mathout, flags=list(cat=FALSE, error="ignore"))

op1 <- names(op1)
if(knitr::is_latex_output())
  op1 <- sapply(op1, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op1 <- sapply(op1, FUN=knitr:::escape_html)

t <- cbind(Operation=op1, 
  "error\\ =\\ asis"=asis, highlight=high, fix=fix, ignore=igno)
knitr::kable(t, caption="Table 3. Highlighting elements of a term",
  row.names=FALSE, escape=FALSE)

## ---- results='asis'----------------------------------------------------------
term <- quote(pbinom(successes, Ntotal, prob))
mathout(term)

hook(successes, k)
hook(quote(Ntotal), quote(N), quote=FALSE)
hook(prob, pi)
mathout(term)

## ---- results='asis'----------------------------------------------------------
ED_single <- function(c, mu)
  dfrac(c, mu)

# display as E(D; mu), c is a scaling parameter
hook(ED_single(.C, .Mu), E(`;`(D, .Mu)))
mathout(call("=", quote(ED_single(c, mu)), ED_single))

## ---- results='asis'----------------------------------------------------------
hook(mu_A, mu["A"])
hook(mu_B, mu["B"])
hook(sigma_A, sigma["A"])
hook(sigma_B, sigma["B"])
hook(mu_M, mu["M"])
hook(M, overline(X))

mathout(call("=", quote(E(D["AB"])), quote(ED_single(c, mu_A + mu_B))))

## ---- results="asis"----------------------------------------------------------
ED_async <- function(tau, c, mu_A, sigma_A, mu_B)
{ dfrac(c, mu_A) + (dfrac(1L, mu_A) - dfrac(1L, mu_A + mu_B)) *
    ((mu_A*tau - c) * pnorm(dfrac(c - mu_A*tau, sqrt(sigma_A^2L*tau)))
      - (mu_A*tau + c) * exp(dfrac(2L*c*mu_A, sigma_A^2L))
        * pnorm(dfrac(-c - mu_A*tau, sqrt(sigma_A^2L*tau))))
}

hook(ED_async(.Tau, .C, .MA, .SA, .MB), E(`;`(D[.Tau], `,`(.MA, .SA, .MB))))
mathout(call("=", quote(ED_async(tau, c, mu_A, sigma_A, mu_B)), ED_async))

## ---- results='asis'----------------------------------------------------------
ED <- function(tau, c, mu_A, sigma_A, mu_B, sigma_B)
{
  if(tau == Inf) return(ED_single(c, mu_A)) ;
  if(tau == -Inf) return(ED_single(c, mu_B)) ;
  if(tau == 0L) return(ED_single(c, mu_A + mu_B)) ;
  if(tau > 0L) return(ED_async(tau, c, mu_A, sigma_A, mu_B)) ;
  if(tau < 0L) return(ED_async(abs(tau), c, mu_B, sigma_B, mu_A))
}

hook(ED(.Tau, .C, .MA, .SA, .MB, .SB), E(`;`(D[.Tau], `,`(.MA, .SA, .MB, .SB))))
mathout(call("=", quote(ED(tau, c, mu_A, sigma_A, mu_B, sigma_B)), ED))

## ---- results='asis'----------------------------------------------------------
ET <- function(tau, c, mu_A, sigma_A, mu_B, sigma_B, mu_M)
  ED(tau, c, mu_A, sigma_A, mu_B, sigma_B) + mu_M

hook(ET(.Tau, .C, .MA, .SA, .MB, .SB, .MM), 
     E(`;`(T[.Tau], `,`(.MA, .SA, .MB, .SB, .MM))))
mathout(call("=", quote(E(T[tau])), ET))

## ----miller-data, echo=FALSE--------------------------------------------------
tau  <- list(-Inf, -167L, -133L, -100L, -67L, -33L, 0L, 33L, 67L, 100L, 133L, 167L, Inf)
m    <- c(231, 234, 230, 227, 228, 221, 217, 238, 263, 277, 298, 316, 348)
s    <- c( 56,  58,  40,  40,  32,  28,  28,  28,  26,  30,  32,  34,  92)
n    <- c(400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400)

ttau <- lapply(tau, FUN=mathout, flags=list(cat=FALSE))
tm   <- lapply(as.integer(m), FUN=mathout, flags=list(cat=FALSE))
ts   <- lapply(as.integer(s), FUN=mathout, flags=list(cat=FALSE))
tn   <- lapply(as.integer(n), FUN=mathout, flags=list(cat=FALSE))

t <- cbind('$\\tau$'=ttau, '$m$'=tm, '$s$'=ts, '$n$'=tn)
knitr::kable(t, caption="Table 4. Miller data", row.names=FALSE, escape=FALSE)

## ---- results='asis'----------------------------------------------------------
z <- function(m, s, n, tau, c, mu_A, sigma_A, mu_B, sigma_B, mu_M)
  dfrac(m - denote(E, ET(tau, c, mu_A, sigma_A, mu_B, sigma_B, mu_M),
              "the expected mean response time"),
    s / sqrt(n))

mathout(call("=", quote(z[tau]), z))

## ---- results='asis'----------------------------------------------------------
zv <- Vectorize(z, vectorize.args = c('m', 's', 'n', 'tau'))

gof <- function(par, tau, m, s, n)
  sum(zv(m, s, n, tau, c=100L, mu_A=par["mu_A"], sigma_A=par["sigma_A"], 
         mu_B=par["mu_B"], sigma_B=par["sigma_B"], mu_M=par["mu_M"])^2L)

hook(zv(.M, .S, .N, .Tau, .C, .MA, .SA, .MB, .SB, .MM), z[.Tau])
mathout(call("=", quote(X["8 df"]^2L), gof))

## ---- results="asis", echo=FALSE----------------------------------------------
lsq <- function(tau, m, s, n)
{
  invisible(theta <- c(mu_A=0.53, sigma_A=4.3, mu_B=1.34, sigma_B=11.7, mu_M=160))
  optim(theta, gof, tau=tau, m=m, s=s, n=n, hessian=TRUE)
}

mathout(call("=", quote(hat(theta)), lsq))

## ----params, echo=FALSE-------------------------------------------------------
fit <- lsq(tau, m, s, n)
theta <- fit$par
se <- sqrt(diag(solve(fit$hessian)))
cilo <- qnorm(0.025, theta, se)
ciup <- qnorm(0.975, theta, se)

ntheta <- lapply(lapply(FUN=as.symbol, names(theta)), FUN=mathout, flags=list(cat=FALSE))
ttheta <- lapply(theta, FUN=mathout, flags=list(cat=FALSE))
tcilo <- lapply(cilo, FUN=mathout, flags=list(cat=FALSE))
tciup <- lapply(ciup, FUN=mathout, flags=list(cat=FALSE))

t <- cbind(Parameter=ntheta, Estimate=ttheta, Lo=tcilo, Up=tciup)
knitr::kable(t, caption="Table 5. Model fit", row.names=FALSE, escape=FALSE)

## ---- results="asis"----------------------------------------------------------
f1 <- function(tau)
{ dfrac(c, mu["A"]) + (dfrac(1L, mu["A"]) - dfrac(1L, mu["A"] + mu["B"]) * 
    ((mu["A"]*tau - c) * pnorm(dfrac(c - mu["A"]*tau, sqrt(sigma["A"]^2L*tau)))
      - (mu["A"]*tau + c) * exp(dfrac(2L*mu["A"]*tau, sigma["A"]^2L))
        * pnorm(dfrac(-c - mu["A"]*tau, sqrt(sigma["A"]^2L*tau)))))
}

mathout(f1)

## ---- results="asis"----------------------------------------------------------
f2 <- function(tau)
{ dfrac(c, mu["A"]) + (dfrac(1L, mu["A"]) - dfrac(1L, mu["A"] + mu["B"])) * 
    ((mu["A"]*tau - c) * pnorm(dfrac(c - mu["A"]*tau, sqrt(sigma["A"]^2L*tau)))
      - (mu["A"]*tau + c) * exp(dfrac(2L*mu["A"]*tau, sigma["A"]^2L))
        * pnorm(dfrac(-c - mu["A"]*tau, sqrt(sigma["A"]^2L*tau))))
}

mathout(f2)

