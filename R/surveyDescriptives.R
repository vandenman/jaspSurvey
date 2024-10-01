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
#' createJaspState createJaspPlot

#'@export
surveyDescriptives <- function(jaspResults, dataset, options) {

  surveyDesign <- setupDesign(jaspResults, dataset, options)

  designTable(surveyDesign, jaspResults, options)
  summaryTable(surveyDesign,  jaspResults, dataset, options)
  testPlot(jaspResults)
}

str2formula <- function(x) {
  is.null(x) && return(NULL)
  (length(x) == 1) && return(as.formula(paste("~", x)))
  return(as.formula(paste("~", paste(x, collapse = "+"))))
}

setupDesign <- function(jaspResults, dataset, options) {

  ready <- (length(options[["variables"]]) > 0) && (
    (options[["hasWeights"]] && length(options[["weights"]]) > 0) ||
      length(options[["probs"]]) > 0
  )

  jaspResults$dependOn(c("variables", "weights", "probs", "hasWeights", "strata"))

  if (!ready)
    return(list(design = NULL, ready = FALSE))

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

  design <- survey::svydesign(
    variables = variables,
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

  if (!isReady(surveyDesign))
    return()

}

summaryTable <- function(surveyDesign, jaspResults, dataset, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  table <- createJaspTable(title = gettext("Summary statistics"))
  table$addColumnInfo(name = "Variable", title = "", type = "string")

  # if (wantsSplit) {
  #   stats$transposeWithOvertitle <- TRUE
  #   stats$addColumnInfo(name = "Variable", title = "", type = "string")
  #   stats$addColumnInfo(name = "Level",    title = "", type = "string")
  # } else {
  #   stats$addColumnInfo(name = "Variable", title = "", type = "string")
  # }


  jaspResults[["summaryTable"]] <- table

  if (!isReady(surveyDesign))
    return()

}

testPlot <- function(jaspResults) {

  if (!is.null(jaspResults[["testPlot"]]))
    return()

  daxis_name <- parse(text = "italic(M)[diff]")

  set.seed(123)
  data.frame(wt = rnorm(10), mpg = rnorm(10))
  myplot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(transform = identity, name = daxis_name)
    )

  plot <- createJaspPlot(title = gettext("Test plot"))
  plot$plotObject <- myplot
  jaspResults[["testPlot"]] <- plot

}
