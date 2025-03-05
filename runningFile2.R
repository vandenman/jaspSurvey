# renv::install('.', prompt = FALSE)

# renv::install(c("jasp-stats/jaspGraphs", "jasp-stats/jaspBase"))

# renv::install("jasp-stats/jaspTools")
library(jaspTools)
# setupJaspTools("~/github/jasp/jasp-desktop", FALSE, FALSE)
setPkgOption("module.dirs", ".")
setPkgOption("reinstall.modules", FALSE)

options <- analysisOptions("surveyDescriptives")
data(api, package = "survey")
dataset <- apiclus1

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


# library(survey)
# dclus2 <- survey::svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)
# debugonce(survey::svyttest)
# tt <- survey::svyttest(enroll~comp.imp, dclus2, use_rcpp = FALSE)
# tt
# confint(tt, level=0.9)
# debugonce(svyglm)
# m <- svyglm(enroll~comp.imp, dclus2, family=gaussian())
