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
  split = "stype",

  # statistics, missing svyquantile
  mean = TRUE,
  var  = TRUE,
  total = TRUE,

  # uncertainty, missing vcov, see ?survey::svyvar
  # should use vcov, SE, coef, confint, svycontrast as extractors
  # also add cvpct
  ci = TRUE,
  ciLevel = .95,
  se = TRUE,
  cv = TRUE
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

survey::svyby(api99 ~stype, dataDesign, survey::svymean, keep.var=FALSE)

graphics.off(); dev.cur()
aa <- survey::svyboxplot(api99 ~stype, dataDesign, plot = FALSE)
survey:::svyboxplot.default
dev.cur()

# dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
# svyby(~api99, ~stype, dclus1, svyquantile, quantiles=0.5, keep.var=FALSE, multicore = FALSE)

computeSummaryTables <- function(dataDesign, options) {

  stats <- list(
    list(optionName = "mean",     columnName = "mean",     fun = survey::svymean),
    list(optionName = "total",    columnName = "total",    fun = survey::svytotal)#,
    # list(optionName = "var",      columnName = "var",      fun = survey::svyvar),
    # list(optionName = "quantile", columnName = "quantile", fun = survey::svyquantile)
  )

  varStats <- list(
    list(optionName = "se", fun = \(x) data.frame(se = unlist(survey::SE(x), use.names = FALSE))),
    list(optionName = "cv", fun = \(x) data.frame(cv = unlist(survey::cv(x), use.names = FALSE))),
    list(optionName = "ci", fun = \(x) stats::setNames(as.data.frame(stats::confint(x, level = options[["ciLevel"]])), c("lower", "upper")))
  )

  variables <- str2formula(options[["variables"]])

  hasSplit <- !is.null(options[["split"]])
  hasSplit <- FALSE
  executor <- if (hasSplit) {
    function(f) survey::svyby(variables, splitFormula, design = dataDesign, f)
  } else {
    function(f) f(variables, dataDesign)
  }
  splitFormula <- str2formula(options[["split"]])

  tableDf <- list()
  for (i in seq_along(stats)) {
    si <- stats[[i]]
    if (options[[si[["optionName"]]]]) {

      sv <- executor(si[["fun"]])
      if (hasSplit) {

        variableNms <- colnames(sv)[seq_along(options[["variables"]]) + 1]
        splitNms    <- rownames(sv)
        df <- data.frame(
          variable = rep(variableNms, each = length(splitNms)),
          split    = splitNms,
          a        = coef(sv)
        )
        colnames(df)[3] <- si[["columnName"]]
      } else {
        df <- data.frame(variable = names(sv), a = coef(sv))
        colnames(df)[2] <- si[["columnName"]]
      }

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

}
  for (optionName in c("mean", "total", "var")) {
    if (options[[optionName]])
      tableDf[[optionName]] <- cbind(variable = rownames(df), tableDf[[optionName]])
  }

  hasSplit <- !is.null(options[["split"]])
  if (hasSplit) {

    stats <- list(
      list(optionName = "mean",     columnName = "mean",     fun = survey::svymean),
      list(optionName = "total",    columnName = "total",    fun = survey::svytotal)#,
      # list(optionName = "var",      columnName = "var",      fun = survey::svyvar),
      # list(optionName = "quantile", columnName = "quantile", fun = survey::svyquantile)
    )

    varStats <- list(
      list(optionName = "se", fun = \(x) data.frame(se = unlist(survey::SE(x), use.names = FALSE))),
      list(optionName = "cv", fun = \(x) data.frame(cv = unlist(survey::cv(x), use.names = FALSE))),
      list(optionName = "ci", fun = \(x) stats::setNames(as.data.frame(stats::confint(x, level = options[["ciLevel"]])), c("lower", "upper")))
    )

    tableDf <- list()
    for (i in seq_along(stats)) {
      si <- stats[[i]]
      if (options[[si[["optionName"]]]]) {

        sv <- survey::svyby(variables, splitFormula, design = dataDesign, si[["fun"]])
        if (split )
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

    for (optionName in c("mean", "total", "var")) {
      if (options[[optionName]])
        tableDf[[optionName]] <- cbind(variable = rownames(df), tableDf[[optionName]])
    }


    vartype <- c()
    colTitles <- c()
    colNames  <- c()
    if (options[["ci"]])  {  vartype <- c(vartype, "ci" ); colNames <- c(colNames, "lower", "upper"); colTitles <- c(colTitles, "lower ci", "upper ci")   }
    if (options[["se"]])  {  vartype <- c(vartype, "se" ); colNames <- c(colNames, "se");             colTitles <- c(colTitles, "SE")   }
    if (options[["cv"]])  {  vartype <- c(vartype, "cv" ); colNames <- c(colNames, "cv");             colTitles <- c(colTitles, "Coefficient of Variation")   }
    if (options[["var"]]) {  vartype <- c(vartype, "var"); colNames <- c(colNames, "var");            colTitles <- c(colTitles, "Variance")   }
    # if (options[["cvpct"]]) vartype <- c(vartype, "cvpct")
    ciLevel <- options[["ciLevel"]]

    splitFormula <- str2formula(options[["split"]])
    tableDf <- list(
      mean  = survey::svyby(variables, splitFormula, design = dataDesign, survey::svymean,  vartype = vartype, level = ciLevel),
      total = survey::svyby(variables, splitFormula, design = dataDesign, survey::svytotal, vartype = vartype, level = ciLevel),
      var   = survey::svyby(variables, splitFormula, design = dataDesign, survey::svyvar,   vartype = vartype, level = ciLevel)
    )

    cnms <- colnames(tableDf[[1]])
    nvar <- length(options[["variables"]])
    noRows <- nrow(tableDf[[1]])
    totalMat <- meanMat <- as.data.frame(matrix(NA, nrow = nrow(tableDf[[1]]) * nvar, ncol = 3 + length(colTitles),
                                                dimnames = list(NULL, c("variable", "split", "mean", colNames))))
    colnames(totalMat)[3] <- "total"


    totalMat[["split"]]    <- meanMat[["spit"]]     <- rep(tableDf[[1]][[1]], nvar)
    totalMat[["variable"]] <- meanMat[["variable"]] <- rep(options[["variables"]], each = noRows)

    rowStart <- 1
    for (i in seq_along(options[["variables"]])) {

      colIdx <- which(endsWith(cnms, options[["variables"]][i]))
      rowIdx <- rowStart:(rowStart + noRows - 1)
      newColIdx <- gsub(pattern = paste0(".", options[["variables"]][i]), replacement = "", x = colnames(tableDf[[1]])[colIdx])
      newColIdx[1] <- "mean"
      meanMat[rowIdx, newColIdx] <- (tableDf[["mean"]][, colIdx, drop = FALSE])
      newColIdx[1] <- "total"
      totalMat[rowIdx, newColIdx] <- tableDf[["total"]][, colIdx, drop = FALSE]

      rowStart <- rowStart + noRows

    }

    sv <- survey::svyby(variables, splitFormula, design = dataDesign, si[["fun"]])

    # could do the above more generically using try-catch or a fallback
    survey::svyby(variables, splitFormula, design = dataDesign, survey::svyquantile, quantiles=0.5, na.rm = TRUE, vartype = vartype, level = ciLevel)    survey::svyby(~cnum, splitFormula, design = dataDesign, survey::svyquantile, quantiles=0.5, vartype = vartype, level = ciLevel)
    debugonce(survey::svyquantile)

    survey::svyby(~flag, splitFormula, design = dataDesign, survey::svyquantile, quantiles=0.5, na.rm = TRUE, vartype = vartype, level = ciLevel)

    survey::svyby(~cnum + flag, splitFormula, design = dataDesign, survey::svyvar, na.rm = TRUE, vartype = vartype, level = ciLevel)
    survey::svyby(~flag, splitFormula, design = dataDesign, survey::svyvar, na.rm = TRUE, vartype = vartype, level = ciLevel)

    survey::svyby(~cnum, splitFormula, design = dataDesign, survey::svymean,  vartype = vartype, level = ciLevel)
    survey::svyvar(~cnum, design = dataDesign)
    survey::svyby(~cnum + api99, splitFormula, design = dataDesign, survey::svymean, vartype = vartype, level = ciLevel)
    survey::svyby(~cnum + api99, splitFormula, design = dataDesign, survey::svyvar,  vartype = vartype, level = ciLevel)

    survey::SE(survey::svyvar(~cnum + flag, design = dataDesign, na.rm = TRUE))
    survey::cv(survey::svyvar(~cnum + flag, design = dataDesign, na.rm = TRUE))
    survey::cv(survey::svyvar(~cnum + api99, design = dataDesign, na.rm = TRUE))
    survey::SE(survey::svyvar(~cnum + api99, design = dataDesign, na.rm = TRUE))
    survey::SE(survey::svyby(variables, splitFormula, design = dataDesign, survey::svymean,  vartype = vartype, level = ciLevel))
    survey::var(survey::svyby(variables, splitFormula, design = dataDesign, survey::svymean,  vartype = vartype, level = ciLevel))

    confint(survey::svyvar(~cnum + flag, design = dataDesign))
    confint(survey::svyvar(~cnum + api99, design = dataDesign))
    vcov(survey::svyvar(~cnum + api99, design = dataDesign))
    survey::SE(survey::svyvar(~cnum + api99, design = dataDesign))

    confint(survey::svyquantile(~cnum + api99, design = dataDesign))




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
      list(optionName = "mean",     columnName = "mean",  fun = survey::svymean),
      list(optionName = "total",    columnName = "total", fun = survey::svytotal),
      list(optionName = "var",      columnName = "var",   fun = survey::svyvar),
      list(optionName = "quantile", columnName = "var",   fun = survey::svyquantile)
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

    for (optionName in c("mean", "total", "var")) {
      if (options[[optionName]])
        tableDf[[optionName]] <- cbind(variable = rownames(df), tableDf[[optionName]])
    }

  }
  return(tableDf)

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
