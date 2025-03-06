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


