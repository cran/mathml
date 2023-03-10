.onAttach <- function(libname, pkgname)
{
  if(!requireNamespace("rolog", quietly=TRUE))
    stop("Could not attach library rolog.")

  rolog::consult(system.file("pl/mathml.pl", package=pkgname))
}

#' MathML output
#'
#' @md
#' 
#' @param term
#' an R call or symbol/number. This function translates _term_ into a
#' MathML string.
#'
#' @param flags (default NULL)
#' list of flags that control the translation
#'
#' @param env (default globalenv())
#' The R environment in which r_eval is being executed.
#' 
#' @return
#' A string with the MathML representation or _term_.
#'
#' @seealso [mathjax()]
#'
#' @details In some functions, the Prolog code may ring back R, for example, to
#' find the names of function arguments. For example (see vignette), when
#' rendering the call `integrate(g, lower=0L, upper=Inf)` as Int_0^Inf g(x) dx,
#' Prolog needs to know that the function g is a function of x. The Prolog rule
#' then searches for the formalArgs of g in the environment _env_.
#'
#' @examples
#' mathml(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L))
#'
mathml <- function(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L), flags=NULL,
  env=globalenv())
{
  flags <- c(flags, list(cat=FALSE))
  t <- rolog::once(call("r2mathml", term, expression(X), flags),
    options=list(preproc=list(rolog::preproc, mathml_preproc)),
    env=env)
  r <- paste(t$X, collapse="")
  if(flags$cat)
    return(cat(r))

  return(r)
}

# Prolog representation of not equal etc. (left: R, right: Prolog)
.mathml_operators = c(
  "%%" = "mod",
  "%/%" = "div")

#' Map R operators to their respective Prolog counterparts
#'
#' @param query
#' an R call or symbol/number. This function translates components of _query_
#' into their respective counterparts from Prolog
#'
#' @return
#' The translated query
#'
#' @md
#'
#' @seealso [mathjax()], [mathml()]
#'
#' @examples
#' mathml_preproc(quote(5 %% 2))
#'
mathml_preproc <- function(query=quote(5 %% 2))
{
  if(is.call(query))
  {
    args <- as.list(query)
    index <- (args[[1]] == names(.mathml_operators))
    if(any(index))
      args[[1]] <- as.symbol(.mathml_operators[index])

    args[-1] <- lapply(args[-1], FUN=mathml_preproc)
    return(as.call(args))
  }

  if(is.list(query))
    query[] <- lapply(query, FUN=mathml_preproc)

  if(is.function(query))
    body(query) <- mathml_preproc(body(query))

  return(query)
}

#' Mathjax output
#'
#' @param term
#' an R call or symbol/number. This function translates _term_ into a
#' LaTeX/MathJax string.
#'
#' @param flags (default NULL)
#' list of flags that control the translation
#'
#' @param env (default globalenv())
#' The R environment in which r_eval is being executed (see vignette for
#' details, "Ringing back to R").
#'
#' @return
#' A string with the MathJax representation or _term_.
#'
#' @md
#'
#' @details In some functions, the Prolog code may ring back R, for example, to
#' find the names of function arguments. For example (see vignette), when
#' rendering the call `integrate(g, lower=0L, upper=Inf)` as Int_0^Inf g(x) dx,
#' Prolog needs to know that the function g is a function of x. The Prolog rule
#' then searches for the formalArgs of g in the environment _env_.
#'
#' @seealso [mathml()]
#'
#' @examples
#' mathjax(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L))
#'
mathjax <- function(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L), flags=NULL,
  env=globalenv())
{
  flags <- c(flags, list(cat=FALSE))
  t <- rolog::once(call("r2mathjax", term, expression(X), flags),
    options=list(preproc=list(rolog::preproc, mathml_preproc)),
    env=env)
  r <- paste(t$X, collapse="")
  if(flags$cat)
    return(cat(r))

  return(r)
}

#' Add a name attribute to an element (most often, an R function)
#'
#' @param x
#' an R object, e.g., an R function
#'
#' @param name
#' the name of the object/function
#'
#' @return
#' The object with the name attribute
#'
#' @md
#'
#' @examples
#' f <- function(x) {sin(x)}
#' mathjax(call("integrate", name(f, "sin"), 0L, 2L*pi))
#'
name <- function(x, name)
{
  attributes(x)$name <- name
  return(x)
}

#' Calligraphic font
#'
#' @param x
#' an R symbol. This function is used to render the content in calligraphic font
#' in MathJax. In MathML, script font is used.
#'
#' @return
#' The function cal is a wrapper for the identity function.
#'
#' @md
#'
#' @seealso [identity()]
#'
#' @examples
#' mathjax(quote(K %in% cal(K)))
#'
cal <- identity

#' Subscript. On the R side, this function is a wrapper of identity, but allows
#' for decorations.
#'
#' @param sub
#' an R symbol or call, e.g., i
#'
#' @param fun
#' an R call or symbol, e.g. sum(x). This is the return value of the function.
#'
#' @return
#' The function over is a wrapper for the identity function, returning _fun_
#'
#' @md
#'
#' @seealso [identity()]
#'
#' @examples
#' mathjax(quote(subscript(sub=i, fun=x)))
#'
subscript <- function(fun=quote(x), sub=quote(i))
{
  return(fun)
}

#' Superscript. This is a wrapper for the identity function, but decorates the
#' result with a superscript.
#'
#' @param sup
#' an R symbol, e.g., "*"
#'
#' @param fun
#' an R call or symbol, e.g. x. This is the return value of the function.
#'
#' @return
#' The function over is a wrapper for the identity function, returning _fun_
#'
#' @md
#'
#' @seealso [identity()]
#'
#' @examples
#' mathjax(quote(superscript(fun=A, sup="*")))
#'
superscript <- function(fun=quote(A), sup="*")
{
  return(fun)
}

#' Subsupscript. This is a wrapper for the identity function, but decorates the
#' result with a sub- and a superscript.
#'
#' @md
#'
#' @param sub
#' an R symbol, e.g., `i=1`
#'
#' @param sup
#' an R symbol, e.g., `N`
#'
#' @param fun
#' an R call or symbol, e.g. `sum(x[i])`. This is the return value.
#'
#' @return
#' The function over is a wrapper for the identity function, returning _fun_
#'
#' @seealso [identity()]
#'
#' @examples
#' N <- 10
#' i <- 1:N
#' x <- rnorm(N)
#' mathjax(call("subsupscript", fun=sum(x[i]), sub=quote(`=`(i, 1L)), sup=quote(N)))
#'
subsupscript <- function(fun=quote(sum(x[i])), sub=quote(`=`(i, 1)), sup=quote(N))
{
  return(fun)
}

#' Canonicalize an R call: Reorder the function arguments
#'
#' @param term
#' an R call.
#'
#' @param drop
#' whether to drop the argument names or not
#'
#' @return
#' The R function, with arguments rearranged
#'
#' @md
#'
#' @examples
#' canonical(term=quote(`%in%`(table=Table, x=X)))
#'
canonical <- function(term=quote(`%in%`(table=Table, x=X)), drop=TRUE)
{
  attr <- attributes(term)

  if(is.call(term))
  {
    f <- match.fun(term[[1]])
    if(!is.primitive(f))
      term <- match.call(f, term)
    term[-1] <- lapply(term[-1], canonical, drop=drop)
  }

  if(drop)
    term <- unname(term)

  attributes(term) <- attr
  return(term)
}

#' Hook for custom symbols
#'
#' @param term
#' an R call or symbol/number. This is the expression to replace.
#'
#' @param display
#' an R call or symbol/number. This is shown instead of _term_.
#'
#' @param quote (default is TRUE)
#' indicates that _term_ and _display_ should be quoted.
#'
#' @param as.rolog (default is TRUE)
#' indicates that simplified quasi-quotoation is to be used.
#'
#' @return
#' TRUE on success
#'
#' @md
#'
#' @examples
#' hook(t0, subscript(t, 0))
#' mathml(quote(t0))
#'
#' hook(term=quote(t0), display=quote(subscript(t, 0)), quote=FALSE)
#' mathml(quote(t0))
#'
hook <- function(term, display, quote=TRUE, as.rolog=TRUE)
{
  if(quote)
  {
    term <- substitute(term)
    display <- substitute(display)
  }

  if(as.rolog)
  {
    term <- rolog::as.rolog(term)
    display <- rolog::as.rolog(display)
  }

  r <- rolog::once(call("assert", call("math_hook", term, display)))
  if(isFALSE(r))
    return(FALSE)

  invisible(r)
}

#' Multiplication
#'
#' @name dot
#'
#' @param e1
#' numerator
#'
#' @param e2
#' denominator
#'
#' @return
#' e1 * e2
#'
dot <- function(e1, e2)
  e1 * e2

#' @rdname dot
#' @export
nodot <- dot

#' @rdname dot
#' @export
times <- dot

#' Division displayed as fraction
#'
#' @param e1
#' numerator
#'
#' @param e2
#' denominator
#'
#' @return
#' e1 / e2
#'
frac <- function(e1, e2)
  e1 / e2

#' Division displayed as fraction
#'
#' @param e1
#' numerator
#'
#' @param e2
#' denominator
#'
#' @return
#' e1 / e2
#'
over <- frac

#' Division displayed as large fraction
#'
#' @param e1
#' numerator
#'
#' @param e2
#' denominator
#'
#' @return
#' e1 / e2
#'
#' @md
#' @seealso [frac()], [over()]
dfrac <- frac

#' Return function body
#'
#' @param fname
#' not clear
#'
#' @param body
#' not clear
#'
#' @return
#' body
#'
fname <- function(fname, body)
{
  return(body)
}

#' Plus Minus, it shows x and calculates x +- y
#'
#' @param x 
#' first term
#'
#' @param y 
#' second term
#'
#' @return c(x - y, x + y)
#' x plus min y
#'
'%+-%' <- function(x, y)
{
  return(c(x - y, x + y))
}


#'Product x * y, shown as x dot y
#'
#' @param x
#' first factor
#'
#' @param y
#' second factor
#'
#' @return
#' x * y 
#'
'%.%' <- function(x, y)
  x * y

#'Approximate equality, shown as x ~~ y
#'
#' @param x
#' first argument
#'
#' @param y
#' second argument
#'
#' @return
#' The result of isTRUE(all.equal(x, y))
#'
'%~~%' <- function(x, y)
  isTRUE(all.equal(x, y))

#'Equivalence, shown as x == y
#'
#' @param x 
#' first argument
#'
#' @param y 
#' second argument
#'
#' @return 
#' x=y , e.g., a = b
#'
'%==%' <- function(x, y)
  x=y


#'Congruence, shown as x =~ y
#'
#' @param x 
#' first argument
#'
#' @param y 
#' second argument
#'
#' @return 
#' x=y , e.g., a cong b
#'
'%=~%' <- function(x, y)
  x=y

#'Proportional, shown as x prop y
#'
#' @param x 
#' first argument
#'
#' @param y 
#' second argument
#'
#' @return 
#' x=y e.g, x prop y
#'
'%prop%' <- function(x, y)
  x=y

#' Double sided arrow, presented as x <-> y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y ,it produces a double sided arrow
#'
#'
'%<->%' <- function(x, y)
  x=y

#'Right arrow, presented as x -> y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y , it produces a right arrow
#'
'%->%' <- function(x, y)
  x=y

#' Left arrow, presented as x <- y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y , it produces a left arrow
#'
'%<-%' <- function(x, y)
  x=y

#'Up arrow, presented as x up y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y , it produces an upward arrow
#'
'%up%' <- function(x, y)
  x=y

#'Down arrow, presented as x downarrow y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y , it produces a downward arrow
#'
'%down%' <- function(x, y)
  x=y

#'If and only if condition, displayed as x <=> y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y , it produces a double arrow double-sided
#'
'%<=>%' <- function(x, y)
  x=y

#'Right double arrow, displayed as x => y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y , it produces a right double arrow
#'
'%<=%' <- function(x, y)
  x=y

#'Left double arrow, displayed as x <= y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y , it produces a left doublearrow
#'
'%=>%' <- function(x, y)
  x=y

#'Up double arrow, displayed as x uArr y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y ,it produces a upward double arrow
#'
'%dblup%' <- function(x, y)
  x=y

#'Down double arrow, displayed as x dArr y
#'
#' @param x 
#' first element
#'
#' @param y 
#' second element
#'
#' @return 
#' x=y ,it produces a downward double arrow
#'
'%dbldown%' <- function(x, y)
  x=y


#' denote
#' This is a function that allows the user to insert abbreviations in the formula,
#' explain them and make the needed computations
#'
#' @param abbr
#' Abbreviation used in the text to refer to the calculation, for example 's_p' for the pooled
#' variance.
#'
#' @param expr
#' Expression: calculations to be made in order to obtain the value to which the abbreviation
#' refers to.
#'
#' @param info
#' Information: Explanation of the formula used to provide the value of the abbreviation.
#' e.g. 'the pooled variance'
#'
#' @return expr
#' e.g., x denotes a^2 + b
#'
denote <- function(abbr, expr, info)
  return(expr)

#' omit_left
#' This is a function that allows the user to highlight the mistakes,
#' in particular the omissions in the left-hand side of the expression
#'
#' @param expr 
#' The expression, e.g. a + b
#'
#' @return substitute(expr)[[3]], e.g., b from a + b
#'
omit_left <- function(expr)
{
  # use third element of [-, A, B]
  eval(substitute(expr)[[3]])
}

#' omit_right
#' This is a function that allows the user to highlight the mistakes,
#' in particular the omissions in the right-hand side of the expression
#'
#' @param expr expression
#'
#' @return substitute(expr)[[2]], e.g., a from a + b
#'
omit_right <- function(expr)
{
  eval(substitute(expr)[[2]])
}

#' omit
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular the omission of an element from a list.
#'
#' @param expr expression
#'
#' @return NULL
#' e.g., remove a + b from a + b
#'
omit <- function(expr)
{
  NULL
}


#' add_left
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular the redundancies in the left-hand side of the expression.
#'
#' @param expr expression
#'
#' @return expr  e.g., highlights a + from a + b
#'
add_left <- function(expr)
{
  return(expr)
}

#' add_right
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular the redundancies in the right-hand side of the expression.
#'
#' @param expr expression
#'
#' @return expr , e.g., highlights + b from a + b
#'
add_right <- function(expr)
{
  return(expr)
}

#' add
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular an extra element in a list
#'
#' @param expr expression
#'
#' @return expr ,  e.g., highlights a + b from a + b
#'
add <- function(expr)
{
  return(expr)
}

#' instead
#'
#' This is a function that allows the user to highlight the mistakes,
#' in particular adds a curly bracket under the wrong term and it provides the
#' correct solutions.
#'
#' @param inst instead
#'
#' @param of of
#'
#' @return inst , e.g. a + c instead of a + b
#'
instead <- function(inst, of)
{
  return(inst)
}
