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
  )
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
  variables = variables,
  id        = id,
  weights   = weights,
  probs     = probs,
  fpc       = fpc,
  data      = dataset
)

summary(dataDesign)

survey::svymean(variables, dataDesign)
survey::svyvar( variables, dataDesign)
survey::svytotal( variables, dataDesign)
survey::svyratio(
  str2formula(options[["variables"]][1]),
  str2formula(options[["variables"]][3]),
  dataDesign
)

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
