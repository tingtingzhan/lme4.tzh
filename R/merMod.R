
#' @title S3 Method Dispatches for \link[lme4]{merMod} Class
#' 
#' @description
#' ..
#' 
#' @param x,object a \link[lme4]{merMod} object
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
#' .pval.merMod(m1)
#' desc_.glmerMod(m1)
#' coef0.merMod(m1)
#' confint_.merMod(m1)
#' nobsText.merMod(m1)
#' 
#' startvec = c(Asym = 200, xmid = 725, scal = 350)
#' (m2 <- nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,
#'   Orange, start = startvec))
#' class(m2)
#' @name s3_merMod


#' @rdname s3_merMod
#' @export
.pval.merMod <- function(x) {
  # 'glmerMod' inherits from 'merMod'
  # 'nlmerMod' inherits from 'merMod'
  x |> 
    summary() |> #?lme4:::summary.merMod
    .pval.summary.merMod()
}

#' @rdname s3_merMod
#' @export
.pval.summary.merMod <- function(x) {
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
  tolower(gsub(' fit by .*$', replacement = '', methTitle(x@devcomp$dims)))
}



# ?lme4:::coef.merMod not want I need
#' @rdname s3_merMod
#' @importFrom nlme fixef
#' @export
coef0.merMod <- function(x) fixef(x) # ?lme4:::fixef.merMod


# \link[lme4]{confint.merMod} is very slow with default `method = 'profile'`
# \link[lme4]{confint.merMod} returns 'sigmas' and cannot be suppressed
#' @rdname s3_merMod
#' @importFrom lme4 confint.merMod
#' @export
confint_.merMod <- function(object, method = 'Wald', ...) {
  ci <- confint.merMod(object, method = method, ...)
  ci[names(coef0.merMod(object)), , drop = FALSE]
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





