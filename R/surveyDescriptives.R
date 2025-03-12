#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' @importFrom jaspBase createJaspTable createJaspContainer
#' createJaspState createJaspPlot jaspDeps .extractErrorMessage

#'@export
surveyDescriptives <- function(jaspResults, dataset, options) {

  surveyDesign <- setupDesign(jaspResults, dataset, options)

  designTable(  surveyDesign, jaspResults,          options)
  summaryTable( surveyDesign, jaspResults, dataset, options)
  alphaTable(   surveyDesign, jaspResults, dataset, options)
  histogramPlot(surveyDesign, jaspResults, dataset, options)
  scatterPlot(  surveyDesign, jaspResults, dataset, options)
  boxPlot(      surveyDesign, jaspResults, dataset, options)

}

setupDesign <- function(jaspResults, dataset, options) {

  ready <- !(isEmpty(options[["variables"]]) || isEmpty(options[["weights"]]))
  print(sprintf("R says: ready: %s", ready))

  jaspResults$dependOn(designDependencies())

  if (!ready)
    return(list(design = NULL, ready = FALSE))

  id      <- str2formula(options[["id"]]) %||% ~0
  strata  <- options[["strata"]]
  if (hasWeights(options)) {
    weights <- str2formula(options[["weights"]])
    probs   <- NULL
  } else {
    weights <- NULL
    probs <- str2formula(options[["weights"]])
  }

  fpc <- str2formula(options[["fpc"]]) %||% NULL

  # variables <- str2formula(options[["variables"]])

  design <- survey::svydesign(
    # variables = variables,
    id        = id,
    weights   = weights,
    probs     = probs,
    fpc       = fpc,
    data      = dataset
  )

  return(list(design = design, ready = TRUE))

}

isReady <- function(surveyDesign) {
  return(surveyDesign[["ready"]])
}

getDesignTypeFmt <- function(independent, strat, withReplacement) {

  # generated via
  # for (independent in c(TRUE, FALSE)) {
  #   for (strat in c(FALSE, TRUE)) {
  #     for (withReplacement in c(FALSE, TRUE)) {
  #
  #       cat("if (independent == ", independent, " && strat == ", strat, " && withReplacement == ", withReplacement, ") ", sep = "")
  #       cat("return(gettext(\"")
  #       # if (independent)     cat("gettext(\"")
  #       # else                 cat("gettextf(\"")
  #
  #       if (strat)           cat("Stratified ")
  #       if (independent)     cat("Independent Sampling design")
  #       else                 cat("%1$d - level Cluster Sampling design with %2$s clusters")
  #       if (withReplacement) cat(" (with replacement)")
  #
  #       cat("\"))\n")
  #       # if (independent)     cat("\")\n")
  #       # else                 cat("\", noLevels, noClusters)\n")
  #
  #     }
  #   }
  # }

  if (independent == TRUE && strat == FALSE && withReplacement == FALSE) return(gettext("Independent Sampling design"))
  if (independent == TRUE && strat == FALSE && withReplacement == TRUE) return(gettext("Independent Sampling design (with replacement)"))
  if (independent == TRUE && strat == TRUE && withReplacement == FALSE) return(gettext("Stratified Independent Sampling design"))
  if (independent == TRUE && strat == TRUE && withReplacement == TRUE) return(gettext("Stratified Independent Sampling design (with replacement)"))
  if (independent == FALSE && strat == FALSE && withReplacement == FALSE) return(gettext("%1$d - level Cluster Sampling design with %2$s clusters"))
  if (independent == FALSE && strat == FALSE && withReplacement == TRUE) return(gettext("%1$d - level Cluster Sampling design with %2$s clusters (with replacement)"))
  if (independent == FALSE && strat == TRUE && withReplacement == FALSE) return(gettext("Stratified %1$d - level Cluster Sampling design with %2$s clusters"))
  if (independent == FALSE && strat == TRUE && withReplacement == TRUE) return(gettext("Stratified %1$d - level Cluster Sampling design with %2$s clusters (with replacement)"))

}

# ---- tables ----
designTable <- function(surveyDesign, jaspResults, options) {

  if (!is.null(jaspResults[["designTable"]]))
    return()

  designTable <- createJaspTable(title = gettext("Survey design"))
  designTable$addColumnInfo(name = "type", title = gettext("Type"), type = "string")

  if (!isReady(surveyDesign)) {
    jaspResults[["summaryTable"]] <- designTable
    return()
  }

  # the code below mimics survey:::print.survey.design2
  x <- surveyDesign$design
  hasStrata <- x$has.strata
  nObs <- NROW(x$variables)

  uniqueClusters <- length(unique(x$cluster[,1]))
  if (nObs == uniqueClusters) {

    isIndependent   <- TRUE
    withReplacement <- is.null(x$fpc$popsize)

  } else {

    isIndependent   <- FALSE
    noLevels        <- NCOL(x$cluster)
    withReplacement <- is.null(x$fpc$popsize)
    noClusters      <- paste(unlist(lapply(x$cluster,function(i) length(unique(i)))),collapse = ", ")

  }

  fmt <- getDesignTypeFmt(isIndependent, hasStrata, withReplacement)
  designTable[["type"]] <- if (isIndependent) fmt else sprintf(fmt, noLevels, noClusters)

  jaspResults[["summaryTable"]] <- designTable

}

summaryTable <- function(surveyDesign, jaspResults, dataset, options) {

  summaryContainer <- jaspResults[["summaryContainer"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Survey Descriptives"),
                        dependencies = c(designDependencies(), "split", "ci", "ciLevel", "se", "cv"))

  if (!is.null(summaryContainer[["meanTable"]]) &&
      (!options[["total"]] || !is.null(summaryContainer[["totalTable"]])) &&
      (!options[["var"]]   || !is.null(summaryContainer[["varTable"]])))
    return()

  statisticsAskedFor <- c("mean", "total", "var")[c(options[["mean"]], options[["total"]], options[["var"]])]

  lst <- list()
  for (stat in statisticsAskedFor) {
    lst[[stat]] <- initializeSummaryTable(stat, options)
    nm <- paste0(stat, "Table")
    summaryContainer[[nm]] <- lst[[stat]]
  }

  if (isReady(surveyDesign)) {
    tablesR <- computeSummaryTables(surveyDesign[["design"]], options)
    for (stat in statisticsAskedFor)
      lst[[stat]]$setData(tablesR[[stat]])
  }
}

initializeSummaryTable <- function(stat, options) {

  table <- createJaspTable(title = gettextf("Summary Statistics - %s", stat))
  table$dependOn(stat)

  table$addColumnInfo(name = "variable", title = "", type = "string")
  if (hasSplit(options))
    table$addColumnInfo(name = "split", title = "", type = "string")

  title <- switch(stat,
                  mean  = gettext("Mean"),
                  total = gettext("Total"),
                  var   = gettext("Variance"))
  name <- switch(stat,
                 mean  = "mean",
                 total = "total",
                 var   = "var")
  table$addColumnInfo(name = name, title = title, type = "number")

  if (options[["se"]]) table$addColumnInfo(name = "se", title = gettext("SE"),                       type = "number")
  if (options[["cv"]]) table$addColumnInfo(name = "cv", title = gettext("Coefficient of Variation"), type = "number")

  if (options[["ci"]]) {
    ciLevelFormatted <- format(100 * options[["ciLevel"]], digits = 3, drop0trailing = TRUE)
    overtitle <- gettextf("%s%% Confidence interval", ciLevelFormatted)
    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  return(table)

}

computeSummaryTables <- function(design, options) {

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
  splitFormula <- str2formula(options[["splitBy"]])

  split <- is.null(splitFormula)
  executor <- if (split) {
    function(f) survey::svyby(variables, splitFormula, design = design, f)
  } else {
    function(f) f(variables, design)
  }

  tableDf <- list()
  for (i in seq_along(stats)) {
    si <- stats[[i]]
    if (options[[si[["optionName"]]]]) {

      sv <- executor(si[["fun"]])
      if (split) {

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
  return(tableDf)
}

# TODO: this is unused?
fillSummaryTable <- function(table, surveyDesign, dataset, options) {

  split <- hasSplit(options)
  if (hasSplit) {

    vartype <- c()
    if (options[["ci"]])  vartype <- c(vartype, "ci")
    if (options[["se"]])  vartype <- c(vartype, "se")
    if (options[["cv"]])  vartype <- c(vartype, "cv")
    if (options[["var"]]) vartype <- c(vartype, "var")
    # if (options[["cvpct"]]) vartype <- c(vartype, "cvpct")
    ciLevel <- options[["ciLevel"]]

    splitFormula <- str2formula(options[["split"]])
    tableDf <- list(
      mean  = survey::svyby(variables, splitFormula, design = dataDesign, survey::svymean, vartype  = vartype, level = ciLevel),
      total = survey::svyby(variables, splitFormula, design = dataDesign, survey::svytotal, vartype = vartype, level = ciLevel)
    )

    # survey::svyby(variables, splitFormula, design = dataDesign, survey::svyquantile, quantiles=c(.1, 0.5, .9), vartype = vartype, level = ciLevel)
    # survey::svyby(variables, splitFormula, design = dataDesign, survey::svyquantile, quantiles=.5, vartype = vartype, level = ciLevel)

    # these things can fail!
    # svyby(~target, ~stype, dataDesign, svyquantile, quantiles=c(.1, 0.5, .9))
    # but it can also be fixed
    # svyby(~target, ~stype, dataDesign, svyquantile, quantiles=c(.1, 0.5, .9), na.rm = TRUE)
    # only some quantiles are unavailable? that's... odd
    # svyby(~growth, ~stype, dataDesign, svyquantile, quantiles=c(.1, 0.5, .9), na.rm = TRUE)


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
  }
}

alphaTable <-  function(surveyDesign, jaspResults, dataset, options) {

  if (!options[["coefficientAlpha"]] || !is.null(jaspResults[["alphaTable"]]))
    return()

  alphaTable <- createJaspTable(title = gettext("Coefficient α"), dependencies = c("coefficientAlpha", designDependencies()))
  alphaTable$addColumnInfo(name = "alpha", title = gettext("α"), type = "number")
  jaspResults[["alphaTable"]] <- alphaTable

  if (isReady(surveyDesign)) {
    alphaValue <- try(survey::svycralpha(str2formula(options[["variables"]]), surveyDesign[["design"]]))
    if (inherits(alphaValue, "try-error")) {
      alphaTable$setError(.extractErrorMessage(alphaValue))
    } else {
      alphaTable[["alpha"]] <- unname(alphaValue)
    }
  }
}

# ---- plots ----
## ---- ggplot2 ----
#' Survey histogram
#'
#' @param design
#' @param xName
#' @param binWidthType
#' @param numberOfBins
#' @param density
#' @param xBreaks
#' @param yBreaks
#' @param addRangeFrame
#'
#' @returns a ggplot2 object
#'
#' @examples
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#'
#' ggSvyHist(dstrat, "api99", binWidthType = "sturges")
#' svyhist(~api99,design=dstrat, breaks = "sturges", xlim = c(300, 900))
#'
#' ggSvyHist(dstrat, "api99", binWidthType = "sturges", density = TRUE)
#'
#' svyhist(~api99,design=dstrat, breaks = "sturges", xlim = c(300, 900))
#' dens1 <- survey::svysmooth(~api99, dstrat, xlim = c(300, 900))
#' lines(dens1)
#'
#' # note that survey's density estimate may be negative, but we cut it off at 0
#' svyhist(~api99,design=dstrat, breaks = "sturges", xlim = c(300, 900), ylim = c(-.001, .003))
#' dens1 <- survey::svysmooth(~api99, dstrat, xlim = c(300, 900))
#' lines(dens1)
#' abline(h = 0)
ggSvyHist <- function(design, xName, binWidthType = "doane", numberOfBins = NA,
                      density = FALSE, xBreaks = NULL, yBreaks = NULL,
                      addRangeFrame = TRUE) {


  mf <- stats::model.frame(stats::as.formula(paste("~", xName)), stats::model.frame(design))
  x <- mf[, 1]

  binWidthType <- jaspGraphs::jaspHistogramBinWidth(x, binWidthType, numberOfBins)

  h <- graphics::hist(x, plot = FALSE, breaks = binWidthType)

  props <- unname(stats::coef(survey::svymean(~cut(x, h[["breaks"]], right = TRUE, include.lowest = TRUE), design, na.rm = TRUE)))
  h[["density"]] <- props / diff(h[["breaks"]])
  h[["counts"]]  <- props * sum(stats::weights(design, "sampling"))

  yKey  <- if (density) "density" else "counts"
  yName <- if (density) gettext("Density") else gettext("Counts")


  yhigh <- max(h[[yKey]])
  xBreaks <- xBreaks %||% jaspGraphs::getPrettyAxisBreaks(c(x, h[["breaks"]]), min.n = 3)
  yBreaks <- yBreaks %||% jaspGraphs::getPrettyAxisBreaks(c(0, yhigh))

  frame <- if (addRangeFrame) jaspGraphs::geom_rangeframe() else NULL

  densityLine <- NULL
  if (density) {
    fromTo <- range(xBreaks)
    densityEstimate <- survey::svysmooth(str2formula(xName), design, xlim = fromTo)

    densityLine <- jaspGraphs::geom_line(
      data = data.frame(
        x = densityEstimate[[xName]][["x"]],
        y = pmax(0, densityEstimate[[xName]][["y"]])
      ),
      mapping = ggplot2::aes(x = x, y = y)
    )
    yBreaks <- yBreaks %||% jaspGraphs::getPrettyAxisBreaks(c(0, max(h[[yKey]], densityEstimate[[xName]][["y"]])))
  } else {
    yBreaks <- yBreaks %||% jaspGraphs::getPrettyAxisBreaks(c(0, yhigh))
  }

  data.frame(
    x = h[["mids"]],
    y = h[[yKey]]
  ) |>
    # subset(y != 0) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = x, y = y)
    ) +
    ggplot2::geom_col(
      col      = "black",
      fill     = "grey75",
      linewidth = 0.7,
      width    = diff(h[["breaks"]])[1]
    ) +
    densityLine +

    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks)) +

    frame +
    jaspGraphs::themeJaspRaw()

}

ggSvycoPlot <- function(design, x, y, color = NULL, splitVars = NULL,
                              splitMethod = c("facet", "group"),
                              mapWeightsToAlpha = TRUE,
                              minAlpha = .2, maxAlpha = 1.,
                              mapWeightsToSize = FALSE,
                              minSize = 1, maxSize = 6,
                              xBreaks = NULL, yBreaks = NULL
                              ) {

  splitMethod <- match.arg(splitMethod)

  # normalize colors
  # wt <- unname(stats::weights(design, "sampling"))
  # maxw   <- max(wt)
  # minw   <- 0
  # alpha  <- c(minAlpha, maxAlpha)
  # alphas <- (alpha[1] * (maxw - wt) + alpha[2] * (wt - minw)) / (maxw - minw)
  # cols   <- transcol(basecol, alphas)

  facet <- NULL
  if (!isEmpty(splitVars) && splitMethod == "facet")
    facet <- ggplot2::facet_wrap(str2formula(splitVars))

  df <- stats::model.frame(design)
  # these names should be unique w.r.t x & y
  # df$color <- cols
  df$weights <- unname(stats::weights(design, "sampling"))

  # xvar <- ggplot2::sym(x)
  # yvar <- ggplot2::sym(y)
  mapping <- ggplot2::aes()
  mapping$x <- ggplot2::sym(x)
  mapping$y <- ggplot2::sym(y)

  colorScale <- fillScale <- NULL
  if (!isEmpty(color)) {
    colorvar <-
      if (length(color) == 1) rlang::sym(color)
      else                    rlang::call2(quote(interaction), !!!lapply(color, rlang::sym))

    mapping$color <- colorvar
    mapping$fill  <- colorvar
    colorScale <- jaspGraphs::scale_JASPcolor_discrete()
    fillScale  <- jaspGraphs::scale_JASPfill_discrete()
  }

  alphaScale <- NULL
  if (mapWeightsToAlpha) {
    mapping$alpha <- quote(weights)
    alphaScale <- ggplot2::scale_alpha(range = c(minAlpha, maxAlpha))
  }

  sizeScale <- NULL
  if (mapWeightsToSize) {
    mapping$size <- quote(weights)
    sizeScale <- ggplot2::scale_size_binned(range = c(minSize, maxSize))
  }

  xBreaks <- xBreaks %||% jaspGraphs::getPrettyAxisBreaks(df[[x]])
  yBreaks <- yBreaks %||% jaspGraphs::getPrettyAxisBreaks(df[[y]])

  # what if noncontinuous scale?
  scale_x <- ggplot2::scale_x_continuous(name = x, breaks = xBreaks, limits = range(xBreaks))
  scale_y <- ggplot2::scale_y_continuous(name = y, breaks = yBreaks, limits = range(yBreaks))

  ggplot2::ggplot(data = df, mapping) +
    jaspGraphs::geom_point() +
    alphaScale + sizeScale + colorScale + fillScale +
    scale_x + scale_y +
    facet +
    jaspGraphs::geom_rangeframe() +
    ggplot2::labs(color = NULL, fill = NULL) +
    jaspGraphs::themeJaspRaw(legend.position = "right")

}

#' Survey Box plot
#' @param design the survey design
#' @param variable the variable to plot
#' @param split NULL or one or more variables to split the boxplot by
#' @param all.outliers logical, whether to show all outliers or just the extreme ones, for compatability with `survey::svyboxplot`.
#' @examples
#' library(survey)
#' data(api)
#' apistrat$interaction <- factor(paste(apistrat$stype, apistrat$comp.imp, sep=" & "))
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
#'
#' svyboxplot(enroll~1,dstrat, plot = FALSE, all.outliers = TRUE, ylims = c(0, 3500))
#' ggSvyBoxplot("enroll", dstrat) + ggplot2::ylim(c(0, 3500))
#'
#' svyboxplot(enroll~stype,dstrat,all.outliers=TRUE)
#' ggSvyBoxplot("enroll", dstrat, "stype")
#'
#' svyboxplot(enroll~stype * comp.imp,dstrat,all.outliers = TRUE) # fails
#' svyboxplot(enroll~interaction,dstrat,all.outliers = TRUE) # workaround
#' ggSvyBoxplot("enroll", dstrat, c("stype", "comp.imp"))
#'
ggSvyBoxplot <- function(design, variable, split = NULL, all.outliers = TRUE,
                         returnDataOnly = FALSE) {

  outcome <- str2formula(variable)
  mf <- stats::model.frame(outcome, stats::model.frame(design))
  outcomeValues <- mf[[1]]

  outlierGeom <- NULL
  outlierDf <- data.frame()
  if (!is.null(split)) {

    groups <- str2formula(split)
    qs <- survey::svyby(outcome, groups, design, survey::svyquantile, ci = FALSE,
                        keep.var = FALSE,
                        quantiles = c(0,0.25,0.5,0.75,1),
                        na.rm = TRUE
    )

    groupValues <- stats::model.frame(groups, stats::model.frame(design), na.action = na.pass)
    nGroupValues <- ncol(groupValues)

    subdataSplit <- split(outcomeValues, groupValues)

    n <- NCOL(qs)
    iqr <- qs[, n - 1] - qs[, n - 3]

    low <- pmax(qs[, n - 4], qs[, n - 3] - 1.5 * iqr)
    hi  <- pmin(qs[, n],     qs[, n - 1] + 1.5*iqr)

    stats <- t(as.matrix(cbind(low, qs[, n - (3:1)], hi)))

    outlierDf <- data.frame()

    # note nrow(qs) == ncol(stats)
    stopifnot(nrow(qs) == ncol(stats)) # DEBUG

    splitLevels <- unname(apply(qs[, 1:nGroupValues, drop = FALSE], 1L, \(x) paste(x, collapse = " & ")))
    splitLevelsOrder <- match(rownames(qs), colnames(stats))
    stopifnot(identical(rownames(qs)[splitLevelsOrder], colnames(stats))) # DEBUG

    splitLevels <- splitLevels[splitLevelsOrder]

    cnmsStats <- colnames(stats)
    for (i in 1:ncol(stats)) {


      out <- c(if (qs[i, n] != hi[i]) qs[i, n], if (qs[i, n - 4] != low[i]) qs[i, n - 4])

      outcomeValues_i <- subdataSplit[[cnmsStats[i]]]

      if (all.outliers) {
        outlo <- sort(outcomeValues_i[!is.na(outcomeValues_i) & outcomeValues_i < low[i]])
        outhi <- sort(outcomeValues_i[!is.na(outcomeValues_i) & outcomeValues_i > hi[i] ])
        outlierPoints <- na.omit(unique(c(outlo, outhi)))
      } else {
        outlierPoints <- c(if (qs[i, n] != hi[i]) qs[i, n], if (qs[i, n - 4] != low[i]) qs[i, n - 4])
      }

      if (length(outlierPoints) > 0)
        outlierDf <- rbind(outlierDf, data.frame(x = splitLevels[i], y = outlierPoints))

    }

    if (nrow(outlierDf) > 0)
      outlierGeom <- jaspGraphs::geom_point(data = outlierDf, ggplot2::aes(x = x, y = y))

    xName <- paste(split, collapse = jaspBase::interactionSymbol)



  } else {

    qs <- stats::coef(survey::svyquantile(outcome, design, ci = FALSE, quantiles = c(0,0.25,0.5,0.75,1), na.rm = TRUE))
    iqr <- qs[4] - qs[2]
    stats <- matrix(c(max(qs[1], qs[2] - 1.5 * iqr), qs[2:4], min(qs[5], qs[4] + 1.5*iqr)))

    if (all.outliers) {
      outlo <- sort(outcomeValues[!is.na(outcomeValues) & outcomeValues < qs[2] - 1.5 * iqr])
      outhi <- sort(outcomeValues[!is.na(outcomeValues) & outcomeValues > qs[4] + 1.5 * iqr])
      outlierPoints <- na.omit(unique(c(outlo, outhi)))
    } else {
      outlierPoints <- c(if (qs[5] != stats[5]) qs[5], if (qs[1] != stats[1]) qs[1])
    }

    if (length(outlierPoints) > 0) {
      outlierDf <- data.frame(x = "", y = outlierPoints)
      outlierGeom <- jaspGraphs::geom_point(data = outlierDf, ggplot2::aes(x = x, y = y))
    }

    xName <- gettext("Total")
  }
  yName <- variable

  boxplotDf <- data.frame(
    x      = if (is.null(split)) "" else splitLevels,
    ymin   = stats[1, ],
    lower  = stats[2, ],
    middle = stats[3, ],
    upper  = stats[4, ],
    ymax   = stats[5, ]
  )

  if (returnDataOnly)
    return(list(boxplotDf = boxplotDf, outlierDf = outlierDf))

  ggplot2::ggplot(boxplotDf, ggplot2::aes(x = x)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax), width = 0.3) +
    ggplot2::geom_boxplot(ggplot2::aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
                          stat = "identity") +
    outlierGeom +
    ggplot2::labs(x = xName, y = yName) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

}

## ---- jaspResults wrappers ----
scatterPlot <- function(surveyDesign, jaspResults, dataset, options) {

  if (!options[["scatterPlots"]])
    return()

  scatterPlotContainer <- jaspResults[["scatterPlotContainer"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Scatter Plots"),
                        dependencies = c("scatterPlots", setdiff(designDependencies(), "variables")))

  variables <- options[["variables"]]
  for (i in 1:(length(variables) - 1)) {
    for (j in (i + 1):length(variables)) {

      v1 <- variables[i]
      v2 <- variables[j]

      # TODO: this check is not working!
      if (is.factor(dataset[[v1]]) || is.factor(dataset[[v2]])) {
        message("skipping pair ", v1, " - ", v2)
        next
      }

      pairName <- sprintf("%s - %s", v1, v2)
      scatterPlotContainer[[pairName]] %setOrRetrieve% {
        # maybe just fix https://github.com/jasp-stats/INTERNAL-jasp/issues/516 for the extra nice syntax?
        # error <- ggplt <- NULL
        # if (isReady(surveyDesign)) {
        #   ggplt <- try({
        #     ggSvycoPlot(
        #       surveyDesign[["design"]], v1, v2, options[["splitBy"]]
        #       # TODO: add more options
        #     )
        #   })
        #   if (inherits(plot, "try-error")) {
        #     error <- .extractErrorMessage(ggplt)
        #     ggplt <- NULL
        #   }
        # }
        # createJaspPlot(
        #   title        = sprintf("%s - %s", v1, v2),
        #   plot         = ggplt,
        #   dependencies = jaspDeps(optionContainsValue = list(variables = c(v1, v2))),
        #   error        = error
        # )
        createJaspPlot(
          title        = sprintf("%s - %s", v1, v2),
          plot         = if (isReady(surveyDesign)) {
            ggSvycoPlot(
              surveyDesign[["design"]], v1, v2, options[["splitBy"]]
              # TODO: add more options
            )
          },
          dependencies = jaspDeps(optionContainsValue = list(variables = c(v1, v2)))
        )
      }

    }
  }
}

histogramPlot <- function(surveyDesign, jaspResults, dataset, options) {

  if (!options[["distributionPlots"]])
    return()

  histogramContainer <- jaspResults[["histogramPlotContainer"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Histograms"),
                        dependencies = c("distributionPlots", setdiff(designDependencies(), "variables")))

  for (variable in options[["variables"]]) {

    histogramContainer[[variable]] %setOrRetrieve% {
      # maybe just fix https://github.com/jasp-stats/INTERNAL-jasp/issues/516 for the extra nice syntax?
      # error <- ggplt <- NULL
      # if (isReady(surveyDesign)) {
      #   ggplt <- try(ggSvyHist(surveyDesign[["design"]], variable))
      #   if (inherits(ggplt, "try-error")) {
      #     error <- .extractErrorMessage(ggplt)
      #     ggplt <- NULL
      #   }
      # }
      # createJaspPlot(
      #   title        = variable,
      #   plot         = ggplt,
      #   dependencies = jaspDeps(optionContainsValue = list(variables = variable)),
      #   error        = error
      # )
      createJaspPlot(
        title        = variable,
        plot         = ggSvyHist(surveyDesign[["design"]], variable),
        dependencies = jaspDeps(optionContainsValue = list(variables = variable)),
      )
    }
  }

}


boxPlot <- function(surveyDesign, jaspResults, dataset, options) {

  if (!options[["boxPlots"]])
    return()

  boxPlotContainer <- jaspResults[["boxPlotContainer"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Boxplots"),
                        dependencies = c("boxPlots", "splitBy", setdiff(designDependencies(), "variables")))

  for (variable in options[["variables"]]) {

    boxPlotContainer[[variable]] %setOrRetrieve% {
      # error <- ggplt <- NULL
      # if (isReady(surveyDesign)) {
      #   ggplt <- try(ggSvyBoxplot(surveyDesign[["design"]], variable, split = options[["splitBy"]]))
      #   if (inherits(ggplt, "try-error")) {
      #     error <- .extractErrorMessage(ggplt)
      #     ggplt <- NULL
      #   }
      # }
      # createJaspPlot(
      #   title        = variable,
      #   plot         = ggplt,
      #   dependencies = jaspDeps(optionContainsValue = list(variables = variable)),
      #   error        = error
      # )
      createJaspPlot(
        title        = variable,
        plot         = ggSvyBoxplot(surveyDesign[["design"]], variable, split = options[["splitBy"]]),
        dependencies = jaspDeps(optionContainsValue = list(variables = variable)),
      )
    }
  }

}

raincloudPlot <- function(surveyDesign, jaspResults, dataset, options) {

  if (!options[["boxPlots"]])
    return()

  boxPlotContainer <- jaspResults[["boxPlotContainer"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Boxplots"),
                        dependencies = c("boxPlots", "splitBy", setdiff(designDependencies(), "variables")))

  for (variable in options[["variables"]]) {

    boxPlotContainer[[variable]] %setOrRetrieve% {

      createJaspPlot(
        title        = variable,
        plot         = ggSvyRaincloudplot(surveyDesign[["design"]], variable, split = options[["splitBy"]]),
        dependencies = jaspDeps(optionContainsValue = list(variables = variable)),
      )
    }
  }

}

# ---- helpers ----
str2formula <- function(x) {
  isEmpty(x) && return(NULL)
  (length(x) == 1) && return(as.formula(paste("~", x)))
  return(as.formula(paste("~", paste(x, collapse = "+"))))
}

isEmpty <- function(x) (length(x) == 0L) || (is.character(x) && identical(x, ""))

hasWeights <- function(options) return(identical(options[["weightsOrProbs"]], "weights"))
hasSplit   <- function(options) return(!is.null(options[["split"]]))

designDependencies <- function() {
  c("variables", "weights", "probs", "hasWeights", "strata")
}
