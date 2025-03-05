#
# Copyright (C) 2013-2024 University of Amsterdam
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
#' createJaspState createJaspPlot jaspDeps

#'@export
surveyDescriptives <- function(jaspResults, dataset, options) {

  # print("R says: options")
  # print(options)
  # print("R says: head(dataset)")
  # print(head(dataset, 10))

  surveyDesign <- setupDesign(jaspResults, dataset, options)

  designTable(surveyDesign, jaspResults, options)
  summaryTable(surveyDesign,  jaspResults, dataset, options)
  histogramPlot(surveyDesign, jaspResults, dataset, options)

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
    noClusters      <- paste(unlist(lapply(x$cluster,function(i) length(unique(i)))),collapse=", ")

  }

  fmt <- getDesignTypeFmt(isIndependent, hasStrata, withReplacement)
  designTable[["type"]] <- if (isIndependent) fmt else sprintf(fmt, noLevels, noClusters)

  jaspResults[["summaryTable"]] <- designTable

}

summaryTable <- function(surveyDesign, jaspResults, dataset, options) {

  summaryContainer <- jaspResults[["summaryContainer"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Survey Descriptives"), dependencies = c("variables", "split", "ci", "ciLevel", "se", "cv"))

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
  splitFormula <- str2formula(options[["split"]])

  split <- hasSplit(options)
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

    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks)) +

    frame +
    jaspGraphs::themeJaspRaw()

}

scatterPlotDesign <- function(design, x, y, splitVars = NULL,
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
  if (!isEmpty(splitVars)) {
    colorvar <-
      if (length(splitVars) == 1) rlang::sym(splitVars)
      else                        rlang::call2(quote(interaction), !!!lapply(splitVars, rlang::sym))

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
    ggplot2::geom_point() +
    alphaScale + sizeScale + colorScale + fillScale +
    scale_x + scale_y +
    facet +
    jaspGraphs::geom_rangeframe() +
    ggplot2::labs(color = NULL, fill = NULL) +
    jaspGraphs::themeJaspRaw(legend.position = "right")

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
      error <- ggplt <- NULL
      if (isReady(surveyDesign)) {
        ggplt <- try(ggSvyHist(surveyDesign[["design"]], variable))
        if (inherits(plot, "try-error"))
          error <- .extractErrorMessage(ggplt)
      }
      createJaspPlot(
        title        = gettextf("Histogram of %s", variable),
        plot         = ggplt,
        dependencies = jaspDeps(optionContainsValue = list(variables = variable)),
        error        = error
      )
    }
  }

}

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
