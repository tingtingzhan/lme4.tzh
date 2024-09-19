
#' @title Additional S3 Method Dispatches for \link[lme4]{merMod} Class
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
#' 
#' @name s3_merMod


#' @rdname s3_merMod
#' @export .pval.merMod
#' @export
.pval.merMod <- function(x) {
  # 'glmerMod' inherits from 'merMod'
  xsum <- summary(x) #?lme4:::summary.merMod returns 'summary.merMod' class
  xsum$coefficients[, 'Pr(>|z|)']
}



#' @rdname s3_merMod
#' @importFrom stats family
#' @export model_desc.glmerMod
#' @export
model_desc.glmerMod <- function(x, ...) {
  fam <- family(x) # ?lme4:::family.merMod
  if (fam$family == 'binomial' && fam$link == 'logit') return('mixed logistic regression model')
  stop('write more')
}

#' @rdname s3_merMod
#' @importFrom lme4 methTitle
#' @export model_desc.merMod
#' @export
model_desc.merMod <- function(x, ...) {
  # see inside ?lme4:::print.merMod
  tolower(gsub(' fit by .*$', replacement = '', methTitle(x@devcomp$dims)))
}

# we have
# lme4:::formula.merMod
# [endpoint.*] will use [tzh::endpoint.default]




# ?lme4:::coef.merMod not want I need
#' @rdname s3_merMod
#' @importFrom nlme fixef
#' @export coef0.merMod
#' @export
coef0.merMod <- function(x) fixef(x) # ?lme4:::fixef.merMod


# \link[lme4]{confint.merMod} is very slow with default `method = 'profile'`
# \link[lme4]{confint.merMod} returns 'sigmas' and cannot be suppressed
#' @rdname s3_merMod
#' @importFrom lme4 confint.merMod
#' @export confint2.merMod
#' @export
confint2.merMod <- function(object, method = 'Wald', ...) {
  ci <- confint.merMod(object, method = method, ...)
  ci[names(coef0.merMod(object)), , drop = FALSE]
}



#' @rdname s3_merMod
#' @importFrom lme4 ngrps
#' @export nobsText.merMod
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







#' @title Sprintf.merMod
#' 
#' @description ..
#' 
#' @param model ..
#' 
#' @param ... ..
#' 
#' @importFrom stats formula terms.formula
#' @export Sprintf.merMod
#' @export
Sprintf.merMod <- function(model, ...) {
  ffom <- formula(model, fixed.only = TRUE) # ?lme4:::formula.merMod
  
  # no variable selection in \pkg{lme4}, that I am aware of ..
  xvar <- unique.default(all.vars(ffom[[3L]]))
  
  if (FALSE) { # KEEP FOR NOW!!
    ranfom <- formula(model, random.only = TRUE) # ?lme4:::formula.merMod
    # random effects should be represented in [nobsText.merMod]
    # ranfom = y ~ (1 | a/b/c) # tested
    # ranfom = y ~ (1 | herd) # tested
    # ranfom = y ~ (1 | herd) + (1 | obs) # tested
    ranterms <- as.list.default(attr(terms.formula(ranfom), which = 'variables', exact = TRUE))[-(1:2)]
    nestedText <- function(x) {
      if (is.symbol(x)) return(deparse1(x))
      if (x[[1L]] == '/') {
        c(nestedText(x[[2L]]), deparse1(x[[3L]])) # recursive!
      } else deparse1(x)
    }
    ran_txt <- vapply(ranterms, FUN = function(i) { # (i = ranterms[[1L]])
      if (i[[1L]] != '|') stop('wont happen')
      paste0('`', nestedText(i[[3L]]), '`', collapse = '-nested-in-')
    }, FUN.VALUE = '')
    cat(sprintf(fmt = 'with random effect(s) of %s', paste0(ran_txt, collapse = ' and ')), '\n')
  } # KEEP FOR NOW!!
  
  sprintf(
    fmt = 'The relationship between **`%s`** and %s is analyzed based on %s by fitting a %svariable %s model using <u>**`R`**</u> package <u>**`lme4`**</u>.', 
    deparse1(ffom[[2L]]), 
    paste0('`', xvar, '`', collapse = ', '),
    nobsText.merMod(model),
    if (length(xvar) > 1L) 'multi' else 'uni',
    model_desc.merMod(model)
  )
}






