# renv::install('.', prompt = FALSE)

# renv::install(c("jasp-stats/jaspGraphs", "jasp-stats/jaspBase"))

# renv::install("jasp-stats/jaspTools")
library(jaspTools)
# setupJaspTools("~/github/jasp/jasp-desktop", FALSE, FALSE)
setPkgOption("module.dirs", ".")
setPkgOption("reinstall.modules", FALSE)

options <- analysisOptions("apiclus1_2.jasp")
options <- options[[4]]
for (key in names(options)) {
  if (is.list(options[[key]])) {
    value <- options[[key]][["value"]]
    types <- options[[key]][["types"]]
    if (!is.null(value) && !is.null(types)) {
      options[[key]] <- value
    }
  }
}

# options <- analysisOptions("surveyDescriptives")
data(api, package = "survey")
dataset <- apiclus1

options$splitBy
dataset$stype
dataset$comp.imp

# debugonce(jaspSurvey:::histogramPlot)
runAnalysis(name = "surveyDescriptives", options = options, dataset = dataset)
# write.csv(apiclus1, "apiclus1.csv", row.names = FALSE)


options2 <- list(
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
  cv = TRUE,
  distributionPlots = TRUE,
  scatterPlots = TRUE
)
for (key in names(options2)) {
  options[[key]] <- options2[[key]]
}


# debugonce(runAnalysis)
# debugonce(jaspBase::runJaspResults)

# debugonce(jaspSurvey:::surveyDescriptives)


debug(jaspSurvey:::scatterPlot)
undebug(jaspSurvey:::scatterPlot)
devtools::load_all()
result <- runAnalysis(name = "surveyDescriptives", options = options, dataset = dataset)

dclus2 <- survey::svydesign(id=~dnum+snum, weights=~pw, data=apiclus2, fpc=~fpc1+fpc2)
survey::svyboxplot(api99 ~ stype, design)
boxplot(api99 ~ stype, apiclus2)

survey::svyboxplot(api99 ~ 1, design)
boxplot(apiclus2$api99)

data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
res <- svycralpha(~ell+mobility+avg.ed+emer+meals, dstrat)
str(res)
library(survey)
dclus2 <- survey::svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)
# debugonce(survey::svyttest)
# tt <- survey::svyttest(enroll~comp.imp, dclus2, use_rcpp = FALSE)
# tt
# confint(tt, level=0.9)
# debugonce(svyglm)
# m <- svyglm(enroll~comp.imp, dclus2, family=gaussian())



ggplot2::ggplot(boxplotDf, ggplot2::aes(x = x)) +
  ggplot2::geom_boxplot(ggplot2::aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
                        stat = "identity") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax), width = 0.3, linewidth = 1)


outcome<-eval(bquote(~.(formula[[2]])))
outcome.values<-model.frame(outcome, model.frame(design),na.action=na.pass)

if (length(attr(terms(formula),"term.labels"))) {

  groups<-eval(bquote(~.(formula[[3]])))
  qs <- svyby(outcome,groups,design,svyquantile,ci=FALSE,
              keep.var=FALSE,
              quantiles=c(0,0.25,0.5,0.75,1),na.rm=TRUE)
  group.values<-model.frame(groups, model.frame(design),na.action=na.pass)[[1]]
  n<-NCOL(qs)
  iqr<- qs[,n-1]-qs[,n-3]
  low<-pmax(qs[,n-4],qs[,n-3]-1.5*iqr)
  hi<-pmin(qs[,n],qs[,n-1]+1.5*iqr)
  stats<-t(as.matrix(cbind(low,qs[,n-(3:1)],hi)))
  z<-list(stats=stats,n=coef(svytotal(groups,design,na.rm=TRUE)))
  for(i in 1:ncol(stats)){
    out<-c(if(qs[i,n]!=hi[i]) qs[i,n],
           if(qs[i,n-4]!=low[i])qs[i,n-4])
    if (all.outliers){
      outlo<-sort(outcome.values[!is.na(outcome.values) & (as.numeric(group.values) %in% i) & outcome.values<low[i] ])
      outhi<-sort(outcome.values[!is.na(outcome.values) & (as.numeric(group.values) %in% i) & outcome.values>hi[i] ])
      out<-na.omit(unique(c(outlo,outhi)))
    }
    z$out<-c(z$out,out)
    z$group<-c(z$group,rep(i,length(out)))
    z$names<-as.character(qs[,1])
  }
} else {
  qs<-coef(svyquantile(outcome,design,ci=FALSE,
                       quantiles=c(0,0.25,0.5,0.75,1),na.rm=TRUE))
  iqr<-qs[4]-qs[2]
  z<-list(stats=matrix(c(max(qs[1],qs[2]-1.5*iqr),
                         qs[2:4],min(qs[5],qs[4]+1.5*iqr))),
          n=sum(weights(design,"sampling")))
  z$out<-c(if(qs[5]!=z$stats[5]) qs[5],
           if(qs[1]!=z$stats[1]) qs[1])
  if (all.outliers){
    outlo<-sort(outcome.values[!is.na(outcome.values) &  outcome.values<qs[2]-1.5*iqr ])
    outhi<-sort(outcome.values[!is.na(outcome.values) & outcome.values>qs[4]+1.5*iqr])
    z$out<-na.omit(unique(c(outlo,outhi)))
  }
  z$group<-rep(1,length(z$out))
}


jaspGraphs::jaspHistogram

library(survey)
data(api)
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

ggSvyHist(dstrat, "api99", binWidthType = "sturges")
svyhist(~api99,design=dstrat, breaks = "sturges", xlim = c(300, 900))

ggSvyHist(dstrat, "api99", binWidthType = "sturges", density = TRUE)

svyhist(~api99,design=dstrat, breaks = "sturges", xlim = c(300, 900))
dens1 <- survey::svysmooth(~api99, dstrat, xlim = c(300, 900))
lines(dens1)

# note that survey's density estimate may be negative, but we cut it off at 0
svyhist(~api99,design=dstrat, breaks = "sturges", xlim = c(300, 900), ylim = c(-.001, .003))
dens1 <- survey::svysmooth(~api99, dstrat, xlim = c(300, 900))
lines(dens1)
abline(h = 0)

design <- dstrat
variable <- "growth"
split <- "stype"

survey::svyboxplot(growth ~ stype, dstrat, all.outliers = TRUE)
survey::svyboxplot(growth ~ 1, dstrat, all.outliers = TRUE)
ggSvyRaincloud(dstrat, "growth")


aaa <- survey::svysmooth(stype ~ growth, design, bandwidth = 30)
str(aaa)

debugonce(survey::svysmooth)

unwrap.svysmooth <- function(x, ...) {
  x
}

ggSvyRaincloud <- function(design, variable, split = NULL) {

  if (is.null(split)) {

    dens0 <- survey::svysmooth(str2formula(variable), design)
    dens0[[variable]][["y"]] <- pmax(0, dens0[[variable]][["y"]])
    violinGeom <- ggplot2::geom_ribbon(data = violinDf, ggplot2::aes(y = y, xmin = 0, xmax = 0 + width),
                         fill = "grey80", color = "black")
  } else {

    # pick bandwidth using dpik, as done by svysmooth and then survey:::svylocpoly
    mf <- stats::model.frame(str2formula(variable), stats::model.frame(design))
    bandwidth <- KernSmooth::dpik(mf[, 1], gridsize = 401)

    splitData <- stats::model.frame(str2formula(split), stats::model.frame(design))
    splitIndices <- split(1:nrow(splitData), splitData)

    subDesigns <- lapply(splitIndices, \(i) design[i, ])

    # here we have
    stopifnot(lengths(splitIndices) == sapply(subDesigns, \(x) length(x$prob)))

    densityEstimates <- lapply(subDesigns, \(x) survey::svysmooth(str2formula(variable), x, bandwidth = bandwidth))

    violinDf <- do.call(rbind, lapply(seq_along(densityEstimates), \(i) {
      dens0 <- densityEstimates[[i]]
      dx <- dens0[[variable]][["x"]]
      dy <- dens0[[variable]][["y"]]
      violinSubDf <- data.frame(
        y     = c(dx, dx[length(dx)]),
        width = c(dy / sum(dy), 0),
        split = names(splitIndices)[i] # TODO: does this work for multiple split variables?
      )
    }))

    violinGeom <- ggplot2::geom_ribbon(data = violinDf, ggplot2::aes(y = y, xmin = 0, xmax = 0 + width,
                                                                     fill = split), alpha = .7)
  }


  # dx <- dens0[[variable]][["x"]]
  # dy <- dens0[[variable]][["y"]]
  # violinDf <- data.frame(
  #   y     = c(dx, dx[length(dx)]),
  #   width = c(dy / sum(dy), 0)
  # )

  temp <- ggSvyBoxplot(design, variable, split = split, returnDataOnly = TRUE, all.outliers = TRUE)
  outlierDf <- temp$outlierDf
  boxplotDf <- temp$boxplotDf
  boxplotWidth <- .8 * max(violinDf$width)

  if (is.null(split)) {
    boxplotDf$x <- -boxplotWidth
    if (nrow(outlierDf) > 0)
      outlierDf$x <- -boxplotWidth

    errorbar <- ggplot2::geom_errorbar(data = boxplotDf, ggplot2::aes(x = x, ymin = ymin, ymax = ymax), width = .8 *boxplotWidth)
    boxplot <- ggplot2::geom_boxplot(data = boxplotDf, ggplot2::aes(x = x, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
                            stat = "identity", width = boxplotWidth)
  } else {

    boxplotDf$group <- as.factor(boxplotDf$x)
    boxplotDf$x <- -boxplotWidth * 1.1 * as.numeric(as.factor(boxplotDf$x))

    if (nrow(outlierDf) > 0) {
      outlierDf$group <- as.factor(outlierDf$x)
      outlierDf$x <- -boxplotWidth * 1.1 * as.numeric(as.factor(outlierDf$x))
    }

    errorbar <- ggplot2::geom_errorbar(data = boxplotDf, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, color = group, group = group), width = .8 *boxplotWidth)
    boxplot <- ggplot2::geom_boxplot(data = boxplotDf, ggplot2::aes(x = x, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax, color = group, group = group),
                                     stat = "identity", width = boxplotWidth, position = ggplot2::position_identity())
  }

  # TODO: this needs the colors as well!
  mf <- model.frame(str2formula(variable), design[["variables"]], na.action = stats::na.pass)
  # Y <- stats::model.response(mf)
  # X <- mf[,attr(attr(mf,"terms"),"term.labels")]

  # TODO: the -3 is not general for split, and the x should be separated when there is a split
  xmax <- min(boxplotDf$x)
  pointsData <- data.frame(
    y = mf[, 1],
    x = stats::runif(length(mf[, 1]), min = -3*boxplotWidth + xmax, max = -2*boxplotWidth + xmax),
    w = stats::weights(design, "sampling")
  )
  if (is.null(split)) {
    jaspGraphs::geom_point(data = pointsData, ggplot2::aes(x = x, y = y, size = w), alpha = .5)
  } else {
    pointsData$split <- factor(splitData[[1]]) # FIXME: this is not general
    pointsGeom <- jaspGraphs::geom_point(data = pointsData, ggplot2::aes(x = x, y = y, size = w, fill = split), alpha = .5)
  }

  ggplot2::ggplot() +
    errorbar +
    boxplot +
    jaspGraphs::geom_point(data = outlierDf, ggplot2::aes(x = x, y = y)) +
    violinGeom +
    pointsGeom +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
}

ggplot2::ggplot(
  data.frame(x = dens0$api99$x, y = 100 * dens0$api99$y / sum(dens0$api99$y)),
  ggplot2::aes(x = 1, y = x, violinwidth = y)) +
  ggplot2::geom_violin(stat = "identity")



svyplot(~api00, design=dstrat, style="bubble")
svyplot(api00~runif(length(api00)), design=dstrat, style="bubble")
svyplot(api00~rep(1,length(api00)), design=dstrat, style="transparent")
svyplot(api00~api99, design=dstrat, style="transparent",pch=19)
svycoplot(api00~api99, design=dstrat, style="transparent")


qsmth<-svysmooth(api00~ell,dstrat, quantile=0.75, df=3,method="quantreg")
lines(qsmth, col="red")

smth<-svysmooth(api00~api99+ell,dstrat)



plot(smth)
plot(smth$api99)
plot(smth, which="ell",lty=2,ylim=c(500,900))
lines(qsmth, col="red")

dens  <- survey::svysmooth(~api99, dstrat,bandwidth=30)
dens1 <- survey::svysmooth(~api99, dstrat)
svyhist(~api99,design=dstrat)
lines(dens,col="purple",lwd=3)
lines(dens1, col="forestgreen",lwd=2)



m<-svyglm(api00~sin(api99/100)+stype, design=dstrat)
termplot(m, data=model.frame(dstrat), partial.resid=TRUE, se=TRUE,
         smooth=make.panel.svysmooth(dstrat))
