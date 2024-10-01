library(survey)
data(api, package = "survey")
dataset <- apiclus1

str2formula <- function(x) {
  is.null(x) && return(NULL)
  (length(x) == 1) && return(as.formula(paste("~", x)))
  as.formula(paste("~", paste(x, collapse = "+")))
}

options <- list(
  id         = "dnum",
  hasWeights = TRUE,
  probs      = "",
  weights    = "pw",
  fpc        = "fpc",
  strata     = NULL,
  variables  = c(
    # "cds", "stype", "name", "sname", "snum", "dname", "cname",
    "cnum", "flag", "pcttest", "api00", "api99", "target", "growth"#,
    # "sch.wide", "comp.imp", "both", "awards", "meals", "ell", "yr.rnd",
    # "mobility", "acs.k3", "acs.46", "acs.core", "pct.resp", "not.hsg",
    # "hsg", "some.col", "col.grad", "grad.sch", "avg.ed", "full",
    # "emer", "enroll", "api.stu"
  ),

  mean = TRUE,
  var  = TRUE,
  total = TRUE,
  ci = TRUE,
  ciLevel = .95,
  se = TRUE,
  cv = TRUE,
  split = "stype"
)


id      <- str2formula(options[["id"]]) %||% ~0
strata  <- options[["strata"]]
if (options[["hasWeights"]]) {
  weights <- str2formula(options[["weights"]])
  probs   <- NULL
} else {
  weights <- NULL
  probs <- str2formula(options[["probs"]])
}

fpc <- str2formula(options[["fpc"]]) %||% NULL

variables <- str2formula(options[["variables"]])

dataDesign <- survey::svydesign(
  # variables = c(variables, options[["split"]]),
  id        = id,
  weights   = weights,
  probs     = probs,
  fpc       = fpc,
  data      = dataset
)
svyby(~api99, ~stype, dataDesign, svymean)

dclus1<-svydesign(id=~dnum, weights=~pw, data=dataset, fpc=~fpc)
svyby(~api99, ~stype, dclus1, svymean)


hasSplit <- !is.null(options[["split"]])
if (hasSplit) {

  vartype <- c()
  if (options[["ci"]])    vartype <- c(vartype, "ci")
  if (options[["se"]])    vartype <- c(vartype, "se")
  if (options[["cv"]])    vartype <- c(vartype, "cv")
  if (options[["var"]])   vartype <- c(vartype, "var")
  # if (options[["cvpct"]]) vartype <- c(vartype, "cvpct")
  ciLevel <- options[["ciLevel"]]

  splitFormula <- str2formula(options[["split"]])
  tableDf <- list(
    mean  = survey::svyby(variables, splitFormula, design = dataDesign, survey::svymean, vartype  = vartype, level = ciLevel),
    total = survey::svyby(variables, splitFormula, design = dataDesign, survey::svytotal, vartype = vartype, level = ciLevel)
  )

  survey::svyby(variables, splitFormula, design = dataDesign, survey::svyquantile, quantiles=c(.1, 0.5, .9), vartype = vartype, level = ciLevel)
  survey::svyby(variables, splitFormula, design = dataDesign, survey::svyquantile, quantiles=.5, vartype = vartype, level = ciLevel)

  # these things can fail!
  svyby(~target, ~stype, dataDesign, svyquantile, quantiles=c(.1, 0.5, .9))
  # but it can also be fixed
  svyby(~target, ~stype, dataDesign, svyquantile, quantiles=c(.1, 0.5, .9), na.rm = TRUE)
  # only some quantiles are unavailable? that's... odd
  svyby(~growth, ~stype, dataDesign, svyquantile, quantiles=c(.1, 0.5, .9), na.rm = TRUE)


  # format is
  # colname      for coef
  # se.colname   for se
  # ci_l.colname for lower ci
  # ci_u.colname for upper ci
  # cv.colname   for coefficient of variation
  # var.colname  for variance
  # cv%          for coefficient of variation in percentage (?)


} else {

  stats <- list(
    list(optionName = "mean",  columnName = "mean",  fun = survey::svymean),
    list(optionName = "total", columnName = "total", fun = survey::svytotal)#,
    # list(optionName = "var",   columnName = "var",   fun = survey::svyvar)
  )

  varStats <- list(
    list(optionName = "se", fun = \(x) data.frame(se = survey::SE(x))),
    list(optionName = "cv", fun = \(x) data.frame(cv = survey::cv(x))),
    list(optionName = "ci", fun = \(x) stats::setNames(as.data.frame(stats::confint(x, level = options[["ciLevel"]])), c("lower", "upper")))
  )

  tableDf <- list()
  for (i in seq_along(stats)) {
    si <- stats[[i]]
    if (options[[si[["optionName"]]]]) {

      sv <- si[["fun"]](variables, dataDesign)
      df <- data.frame(a = coef(sv))
      colnames(df) <- si[["columnName"]]

      if (si[["optionName"]] != "var") { # does not work well for variances
        for (j in seq_along(varStats)) {
          vsj <- varStats[[j]]
          if (options[[vsj[["optionName"]]]]) {

            df <- cbind(df, vsj[["fun"]](sv))

          }
        }
      }

      tableDf[[si[["optionName"]]]] <- df

    }
  }


  setNames(as.data.frame(stats::confint(meanEsts)), c("lower", "upper"))
  , level = options[["confidenceIntervalLevel"]]))


  meanEsts <- survey::svymean (variables, dataDesign)
  meanEsts <- survey::svyvar  (variables, dataDesign)
  meanEsts <- survey::svytotal(variables, dataDesign)

  tableDf <- data.frame(var = options[["variables"]])


    mean  = stats::coef(meanEsts),
    se    = survey::SE(meanEsts)
  )

  if (options[["confidenceInterval"]]) {
    meanCIs  <- stats::confint(meanEsts, level = options[["confidenceIntervalLevel"]])
    tableDf[["lower"]] <- meanCIsMat[, 1]
    tableDf[["upper"]] <- meanCIsMat[, 2]
  }

}

debugonce(survey:::svyvar.survey.design)
survey:::svyvar.survey.design(variables, dataDesign)

as.matrix(meanEsts)
survey::svyquantile()

debugonce(as.matrix, signature = "svystat")
as.matrix(meanEsts)

vcov(survey::svymean(variables, dataDesign))
confint(survey::svymean(variables, dataDesign))

vcov(survey::svytotal(variables, dataDesign))
confint(survey::svymean(variables, dataDesign))

stats::coef(meanEsts)
survey::SE(meanEsts)

sdEsts <- survey::svyby(variables, dataDesign)

dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
svyby(~api99, ~stype, dclus1, svymean)

data(api)

## one-stage cluster sample

svyby(~api99+api00, ~stype, dclus1, svymean, deff=TRUE,vartype="ci")

svyby(~api99+api00, ~stype+comp.imp, dclus1, svymean,vartype=c("se", "ci"))

svyby(~api99+api00, ~stype+comp.imp, design = dclus1, FUN = svymean,vartype=c("se", "ci"))
svyby(~api99+api00, ~stype+comp.imp, design = dclus1, FUN = svymean,vartype=c("se", "ci"))


rclus1<-as.svrepdesign(dclus1)

svyby(~api99+api00, ~stype+sch.wide, rclus1, unwtd.count, keep.var=FALSE)
svybys(~api99+api00, ~stype+sch.wide, rclus1, unwtd.count, keep.var=FALSE)


svymean(~api00, dclus1, deff=TRUE)
svymean(~api00 + factor(stype),dclus1)

apiclus1$fpc
apiclus1$pw
table(apiclus1$dnum)
length(table(apiclus1$dnum))


data(api)
srs_design <- svydesign(id=~1, fpc=~fpc, data=apisrs)
svytotal(~enroll, srs_design)
svymean( ~enroll, srs_design)
ee <- svyvar(  ~enroll, srs_design)


means <- svymean(~api00+api99, srs_design)
svycontrast(means, c(api00=1, api99=-1))

srs_design2 <- update(srs_design, apidiff = api00 - api99)
srs_design2 <- update(srs_design2, apipct  = apidiff / api99)
srs_design2 <- update(srs_design2, apipct  = -exp(apidiff) / api99)
svymean(~apidiff+apipct, srs_design2)
svymean(~apidiff+apipct, srs_design2)

apisrs2 <- apisrs
apisrs2$test <- apisrs2$api00 - apisrs2$api99
srs_design3 <- svydesign(id=~1, fpc=~fpc, data=apisrs2)
svymean(~apidiff, srs_design2)
svymean(~test, srs_design3)


svydesign(id=~1, strata=~stype, fpc=~fpc, data=apistrat)
svydesign(id=~1, strata=~stype, data=apistrat)
svydesign(id=~1, data=apistrat)
svydesign(id=~full, data=apistrat)
# strata -> NULL -> Stratified else

svydesign(id=~1, strata=~stype, fpc=~fpc, data=apistrat[c("stype", "fpc")])


strat_design <- svydesign(id=~1, strata=~stype, fpc=~fpc, data=apistrat)

survey:::print.survey.design(strat_design)
survey:::print.survey.design2(strat_design)

survey:::print.survey.design2(strat_design, varnames = TRUE, design.summaries = TRUE)

summarizeDesign <- function(options) {

}

# should these be determined from the options?
isStratified <- function(surveyObject) {
  isTRUE(x[["has.strata"]])
}
isStratified <- function(surveyObject) {
  isTRUE(x[["has.strata"]])
}

survey:::summary.survey.design2
survey:::print.summary.survey.design2
summary(strat_design)
