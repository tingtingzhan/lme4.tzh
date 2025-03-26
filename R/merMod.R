
#' @title S3 Method Dispatches for \link[lme4]{merMod} Class
#' 
#' @description
#' ..
#' 
#' @param x a \link[lme4]{merMod} object
#' 
#' @param level \link[base]{double} scalar, confidence level
#' 
#' @param method ..
#' 
#' @param ... ..
#' 
#' @examples
#' # see ?lme4::glmer or ?lme4::cbpp
#' library(lme4)
#' class(m1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), 
#'   data = cbpp, family = binomial))
#' desc_.glmerMod(m1)
#' coef_.merMod(m1)
#' confint_.merMod(m1)
#' nobsText.merMod(m1)
#' 
#' startvec = c(Asym = 200, xmid = 725, scal = 350)
#' (m2 <- nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,
#'   Orange, start = startvec))
#' class(m2)
#' desc_.merMod(m2)
#' @name s3_merMod
#' @export
.pval.summary.merMod <- function(x) {
  # ('glmerMod' inherits from 'merMod')
  # ('nlmerMod' inherits from 'merMod')
  # returned value from ?lme4:::summary.merMod
  cf <- x$coefficients
  ret <- cf[, 'Pr(>|z|)'] # has error on 'nlmerMod' object!!
  names(ret) <- rownames(cf)
  return(ret)
}





#' @rdname s3_merMod
#' @importFrom stats family
#' @export
desc_.glmerMod <- function(x) {
  fam <- family(x) # ?lme4:::family.merMod
  switch(fam$family, binomial = {
    switch(fam$link, logit = {
      return('mixed logistic regression')
    }, stop('write more'))
  }, gaussian = {
    switch(fam$link, log = {
      'generalized linear mixed regression with log-link'
    }, stop('write more'))
  }, stop('write more'))
}

#' @rdname s3_merMod
#' @importFrom lme4 methTitle
#' @export
desc_.merMod <- function(x) {
  # see inside ?lme4:::print.merMod
  x@devcomp$dims |> 
    methTitle() |>
    gsub(pattern = ' fit by .*$', replacement = '') |> 
    tolower()
}



# ?lme4:::coef.merMod not want I need
#' @rdname s3_merMod
#' @importFrom nlme fixef
#' @export
coef_.merMod <- function(x) fixef(x) # ?lme4:::fixef.merMod


# \link[lme4]{confint.merMod} is very slow with default `method = 'profile'`
# \link[lme4]{confint.merMod} returns 'sigmas' and cannot be suppressed
#' @rdname s3_merMod
#' @importFrom lme4 confint.merMod
#' @export
confint_.merMod <- function(x, level = .95, method = 'Wald', ...) {
  ci <- confint.merMod(object = x, level = level, method = method, ...)
  ret <- ci[names(coef_.merMod(x)), , drop = FALSE]
  attr(ret, which = 'conf.level') <- level
  return(ret)
}



#' @rdname s3_merMod
#' @importFrom lme4 ngrps
#' @export
nobsText.merMod <- function(x) {
  # ?lme4:::.prt.grps (from ?lme4:::print.merMod)
  # or ?lme4:::nobs.merMod
  dims <- x@devcomp$dims
  ng <- ngrps(x) # ?lme4:::ngrps.merMod
  sprintf(fmt = '%d records from %s', 
          dims[['n']],
          paste(sprintf(fmt = '%d `%s`', ng, names(ng)), collapse = ' nested in '))
} # same as ?nobsText.rlmerMod



# do I need this??
# @method vcov VarCorr.merMod
# @export
#vcov.VarCorr.merMod <- function(object, ...) unclass(object) # return object of ?lme4:::VarCorr.merMod





