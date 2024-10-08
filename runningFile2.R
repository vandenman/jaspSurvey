renv::install('.')

# renv::install(c("jasp-stats/jaspGraphs", "jasp-stats/jaspBase"))

library(jaspTools)
# setupJaspTools("~/github/jasp/jasp-desktop", FALSE, FALSE)
setPkgOption("module.dirs", ".")
setPkgOption("reinstall.modules", FALSE)

options <- analysisOptions("surveyDescriptives")
data(api, package = "survey")
dataset <- apiclus1

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
  )
)

# debugonce(runAnalysis)
# debugonce(jaspBase::runJaspResults)

debugonce(jaspSurvey::surveyDescriptives)
result <- runAnalysis(name = "surveyDescriptives", options = options, dataset = dataset)
