

#' @title Additional S3 Method Dispatches for \link[robustlmm]{rlmer} Return
#' 
#' @description
#' ..
#' 
#' @param x,object a \link[lme4]{merMod} object
#' 
#' @param parm,level see \link[stats]{confint.default}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Package \CRANpkg{robustlmm} imports package \CRANpkg{lme4}.
#' 
#' Object returned by function \link[robustlmm]{rlmer} does **not** \link[base]{inherits} from \link[lme4]{merMod} class.
#' 
#' 
#' @examples
#' # ?robustlmm::rlmer
#' library(robustlmm)
#' m = rlmer(Reaction ~ Days + (Days|Subject), data = sleepstudy,
#'  rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
#'  rho.b = chgDefaults(smoothPsi, k = 5.14, s=10),
#'  rho.sigma.b = chgDefaults(smoothPsi, k = 5.14, s=10))
#' 
#' ecip::coef_(m)
#' confint(m)
#' @name s3_rlmerMod


#' @rdname s3_rlmerMod
#' @export
desc_.rlmerMod <- function(x) {
  # ?robustlmm:::print.rlmerMod
  # ?robustlmm:::.methTitle
  'robust linear mixed'
}

# ?robustlmm:::coef.rlmerMod not what I want
#' @rdname s3_rlmerMod
#' @importFrom nlme fixef
#' @export
coef_.rlmerMod <- function(x) fixef(x) # ?lme4:::fixef.merMod


# ?robustlmm:::vcov.rlmerMod returns 'dpoMatrix' object, defined in \CRANpkg{Matrix}
# ... \link[base]{diag} can handle it
# however ?robustlmm:::coef.rlmerMod is crazy
# therefore ?stats:::confint.default fails
#' @rdname s3_rlmerMod
#' @export confint.rlmerMod
#' @export
confint.rlmerMod <- stats::confint.default
body(confint.rlmerMod)[[2L]] <- quote(cf <- coef_.rlmerMod(object))
body(confint.rlmerMod)[[7L]] <- quote(pct <- sprintf(fmt = '%.1f%%', 1e2*a))
# ?stats:::.format_perc not exported
body(confint.rlmerMod)[[10L]] <- quote(ses <- sqrt(diag(as.matrix(vcov(object))))[parm])
environment(confint.rlmerMod) <- asNamespace(ns = 'lme4.tzh') # important!



# cran.r-project.org/web/packages/robustlmm/vignettes/rlmer.pdf  # Section 5. Further information
# 'We avoided the topic of robust testing for linear mixed-eﬀects models in this tutorial.'
#' @rdname s3_rlmerMod
#' @export
.pval.summary.rlmerMod <- function(x) {
  stop('not available as of 2023-12-10') # packageDate("robustlmm")
}


# generic function ?lme4::ngrps
#' @rdname s3_rlmerMod
#' @export
ngrps.rlmerMod <- function(object, ...) {
  # same as ?lme4:::ngrps.merMod
  object@flist |> 
    vapply(FUN = nlevels, FUN.VALUE = 0L)
}


#' @rdname s3_rlmerMod
#' @export
nobsText.rlmerMod <- nobsText.merMod 


