# TODO:
# do we want the hexbin version?
# legend?

library(survey)
renv::install("~/github/jasp/jasp-desktop/Engine/jaspGraphs", prompt = FALSE)


ggSvyHist <- function(xName, design, binWidthType = "doane", numberOfBins = NA,
                      density = FALSE) {


  mf <- stats::model.frame(stats::as.formula(paste("~", xName)), stats::model.frame(design))
  x <- mf[, 1]

  binWidthType <- jaspGraphs::jaspHistogramBinWidth(x, binWidthType, numberOfBins)

  h <- graphics::hist(x, plot = FALSE, breaks = binWidthType)
  # plot(h)


  props <- unname(stats::coef(survey::svymean(~cut(x, h[["breaks"]], right = TRUE, include.lowest = TRUE), design, na.rm = TRUE)))
  h[["density"]] <- props / diff(h[["breaks"]])
  h[["counts"]]  <- props * sum(stats::weights(design, "sampling"))

  yKey  <- if (density) "density" else "counts"
  yName <- if (density) gettext("Density") else gettext("Counts")


  yhigh <- max(h[[yKey]])
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(x, h[["breaks"]]), min.n = 3)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, yhigh))

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

    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

}


data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                    fpc = ~fpc)

xName <- "enroll"
design <- dstrat

mf <- model.frame(as.formula(paste("~",xName)), model.frame(design))
x <- mf[, 1]

k <- jaspGraphs::jaspHistogramBinWidth(x, "doane", 0)
hist(apistrat$enroll, main="Sample unweighted",col="purple",prob=TRUE,
     breaks = k, probability = FALSE)

svyhist(~enroll, dstrat, main="Survey weighted",col="purple",
        breaks = k, probability = FALSE)

ggplot2::GeomBar$draw_panel
ggplot2::GeomCol$draw_panel

ggSvyHist("enroll", design)
jaspGraphs::jaspHistogram(x, "enroll")


data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                    fpc = ~fpc)
svyhist(~enroll, dstrat, main="Survey weighted",col="purple",ylim=c(0,1.3e-3))


opar<-par(mfrow=c(1,3))
hist(apistrat$enroll, main="Sample unweighted",col="purple",prob=TRUE,ylim=c(0,1.3e-3))
hist(apipop$enroll, main="Population",col="purple",prob=TRUE,ylim=c(0,1.3e-3))
par(mfrow=c(1,1))

svyhist(~enroll, dstrat, main="Survey weighted",col="purple",ylim=c(0,1.3e-3), plot = FALSE)

svyboxplot(enroll~stype,dstrat,all.outliers=TRUE)
svyboxplot(enroll~1,dstrat)
par(opar)

data(api)
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
svyplot(api00~api99, design=dstrat, style="bubble")
svyplot(api00~api99, design=dstrat, style="transparent",pch=19)


## these two require the hexbin package
svyplot(api00~api99, design=dstrat, style="hex", xlab="1999 API",ylab="2000 API")
svyplot(api00~api99, design=dstrat, style="grayhex",legend=0)
dclus2<-svydesign(id=~dnum+snum, weights=~pw,
                  data=apiclus2, fpc=~fpc1+fpc2)
svyplot(api00~api99, design=dclus2, style="subsample")
svyplot(api00~api99, design=dclus2, style="subsample",
        amount=list(x=25,y=25))
svyplot(api00~api99, design=dstrat,
        basecol=function(df){c("goldenrod","tomato","sienna")[as.numeric(df$stype)]},
        style="transparent",pch=19,alpha=c(0,1))
legend("topleft",col=c("goldenrod","tomato","sienna"), pch=19, legend=c("E","H","M"))
## For discrete data, estimate a population table and plot the table.
plot(svytable(~sch.wide+comp.imp+stype,design=dstrat))

fourfoldplot(svytable(~sch.wide+comp.imp+stype,design=dstrat,round=TRUE))
par

mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)
# see https://github.com/haleyjeppson/ggmosaic

## To draw on a hexbin plot you need grid graphics, eg,
library(grid)
h<-svyplot(api00~api99, design=dstrat, style="hex", xlab="1999 API",ylab="2000 API")
s<-svysmooth(api00~api99,design=dstrat)
grid.polyline(s$api99$x,s$api99$y,vp=h$plot.vp@hexVp.on,default.units="native",
              gp=gpar(col="red",lwd=2))

library(survey)
data(api, package = "survey")
dclus2 <- survey::svydesign(id=~dnum+snum, weights=~pw, data=apiclus2, fpc=~fpc1+fpc2)
svycoplot(api00~api99|sch.wide*comp.imp, design=dclus2, style="hexbin")
svycoplot(api00~api99|sch.wide*comp.imp, design=dclus2, style="hexbin", hexscale="absolute")
svycoplot(api00~api99|sch.wide, design=dclus2, style="trans")
svycoplot(api00~meals|stype,design=dclus2,
          style="transparent",
          basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)],
          alpha=c(0,1))


dclus2$awards
svycoplot(api00~meals|stype*awards,design=dclus2,
          style="transparent",
          basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)],
          alpha=c(0,1), plot=FALSE)


design <- dclus2
wt <- weights(design,"sampling")

df <- model.frame(design)

basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)]
if(is.function(basecol)) basecol<-basecol(model.frame(design))
transcol<-function(base,opacity){
  rgbs<-col2rgb(base)/255
  rgb(rgbs[1,],rgbs[2,], rgbs[3,], alpha=opacity)
}
maxw<-max(wt)
minw<-0
alpha <- c(0, 1)
alphas<- (alpha[1]*(maxw-wt)+alpha[2]*(wt-minw))/(maxw-minw)
cols<-transcol(basecol,alphas)

df$color <- cols
df$awards <- factor(df$awards, levels = c("Yes", "No"))
ggplot2::ggplot(
  data = df,
  ggplot2::aes(x = meals, y = api00)) +
  ggplot2::geom_point(color = cols, fill = cols) +
  ggplot2::facet_wrap(~awards*stype) +
  # ggplot2::facet_grid(cols=ggplot2::vars(stype), rows = ggplot2::vars(awards)) +
  jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw()

scatterPlotDesign <- function(design, x, y, splitVars = NULL,
                              splitMethod = c("facet", "group"),
                              minAlpha = .2, maxAlpha = 1.) {

  splitMethod <- match.arg(splitMethod)

  # TODO: use jasp colors
  basecol = function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)]
  transcol <- function(base,opacity){
    rgbs<-col2rgb(base)/255
    rgb(rgbs[1,],rgbs[2,], rgbs[3,], alpha=opacity)
  }
  if(is.function(basecol)) basecol <- basecol(model.frame(design))



  wt <- weights(design,"sampling")
  df <- model.frame(design)

  # normalize colros
  maxw   <- max(wt)
  minw   <- 0
  alpha  <- c(minAlpha, maxAlpha)
  alphas <- (alpha[1] * (maxw - wt) + alpha[2] * (wt - minw)) / (maxw - minw)
  cols   <- transcol(basecol,alphas)

  xvar <- ggplot2::sym(x)
  yvar <- ggplot2::sym(y)

  mapping <- ggplot2::aes(x = !!xvar, y = !!yvar)
  facet <- NULL
  if (!isEmpty(splitVars) && splitMethod == "facet")
    facet <- ggplot2::facet_wrap(str2formula(splitVars))

  df$color <- cols
  df$awards <- factor(df$awards, levels = c("Yes", "No"))
  ggplot2::ggplot(
    data = df,
    mapping
  ) +
    ggplot2::geom_point(color = cols, fill = cols) +
    facet +
    # ggplot2::facet_grid(cols=ggplot2::vars(stype), rows = ggplot2::vars(awards)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

}

debugonce(scatterPlotDesign)
scatterPlotDesign("meals", "api00", design = dclus2)
scatterPlotDesign("meals", "api00", "stype", dclus2)
scatterPlotDesign("meals", "api00", c("stype", "awards"), dclus2)


scatterPlotDesign("meals", "api00", "stype", dclus2, splitMethod = "group",
                  minAlpha = .4)
svycoplot(api00~meals|stype,design=dclus2,
          style="transparent",
          basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)],
          alpha=c(0,1))


scatterPlotDesign("meals", "api00", c("stype", "awards"), dclus2)
svycoplot(api00~meals|stype*awards,design=dclus2,
          style="transparent",
          basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)],
          alpha=c(0,1))


svycoplot(api00~meals|stype*awards,design=dclus2,
          style="transparent",
          basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)],
          alpha=c(0,1), plot=FALSE)




svycoplot(api00~meals|stype,design=dclus2,
          style="hex",
          basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)],
          alpha=c(0,1), plot=FALSE)

ggplot2::ggplot(
  data = df,
  ggplot2::aes(x = meals, y = api00)) +
  ggplot2::geom_hex() +
  ggplot2::facet_grid(cols=ggplot2::vars(stype)) +
  jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw()



class(aaa)
str(aaa)

aaa

temp <- model.frame(design)
temp2 <- split(temp, temp$stype)
x <- temp2$E$meals
y <- temp2$E$api00
W <- wt[temp$stype == "E"]

vp<-current.viewport()
wd<-convertWidth(vp$width,unitTo="cm",valueOnly=TRUE)
ht<-convertHeight(vp$height,unitTo="cm",valueOnly=TRUE)

rval<-hexbin::hexbin(x,y,IDs=TRUE,xbins=15,shape=ht/wd,xbnds=range(x),ybnds=range(y))
cell<-rval@cID
rval@count<-as.vector(tapply(W,cell,sum))
rval@xcm<-as.vector(tapply(1:length(x), cell,
                           function(ii) weighted.mean(x[ii],W[ii])))
rval@ycm<-as.vector(tapply(1:length(y), cell,
                           function(ii) weighted.mean(x[ii],W[ii])))


hexscale <- "relative"
lattice::xyplot(api00~meals|stype, data=model.frame(design), xbins=15,
       panel=function(x,y,style="centroids",xbins,subscripts,...) {
         if (!length(x)) return(panel.xyplot(x,y,...))
         vp<-current.viewport()
         wd<-convertWidth(vp$width,unitTo="cm",valueOnly=TRUE)
         ht<-convertHeight(vp$height,unitTo="cm",valueOnly=TRUE)
         W<-wt[subscripts]
         rval<-hexbin::hexbin(x,y,IDs=TRUE,xbins=xbins,shape=ht/wd,xbnds=vp$xscale,ybnds=vp$yscale)
         cell<-rval@cID
         rval@count<-as.vector(tapply(W,cell,sum))
         rval@xcm<-as.vector(tapply(1:length(x), cell,
                                    function(ii) weighted.mean(x[ii],W[ii])))
         rval@ycm<-as.vector(tapply(1:length(y), cell,
                                    function(ii) weighted.mean(x[ii],W[ii])))
         hexbin::grid.hexagons(rval,style=style, maxarea=switch(hexscale, relative=0.8,
                                                                absolute=0.8*sum(W)/sum(wt)))
       }
)

basecol=function(d) c("darkred","purple","forestgreen")[as.numeric(d$stype)]
if(is.function(basecol)) basecol<-basecol(model.frame(design))
transcol<-function(base,opacity){
  rgbs<-col2rgb(base)/255
  rgb(rgbs[1,],rgbs[2,], rgbs[3,], alpha=opacity)
}
maxw<-max(wt)
minw<-0
alpha <- c(0, 1)
alphas<- (alpha[1]*(maxw-wt)+alpha[2]*(wt-minw))/(maxw-minw)
cols<-transcol(basecol,alphas)
xyplot(formula, data=model.frame(design),
       panel=function(x,y,basecol="black",subscripts,...) {
         a<-alphas[subscripts]
         panel.xyplot(x,y,col=cols[subscripts],pch=19,...)
       },...)

design <- dclus2
wt <- weights(design,"sampling")

df <- model.frame(design)
df$color <- cols
ggplot2::ggplot(
  data = df,
  ggplot2::aes(x = meals, y = api00)) +
  ggplot2::geom_hex(bins = 15, color = cols, fill = cols) +
  ggplot2::facet_grid(cols=ggplot2::vars(stype)) +
  jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw()

ggplot2::ggplot(
  data = model.frame(design),
  ggplot2::aes(x = meals, y = api00, color = stype, fill = stype)) +
  jaspGraphs::geom_point() +
  ggplot2::facet_grid(cols=ggplot2::vars(stype)) +
  jaspGraphs::geom_rangeframe() +
  jaspGraphs::themeJaspRaw()


get_majority <- function(x){

  x <- as.vector(x)
  tally <- table(x)
  max_idx <- seq_along(tally)[tally == max(tally, na.rm = TRUE)]

  if(length(max_idx) > 1){
    max_idx <- sample(max_idx, size = 1)
  }

  majority <- names(tally)[max_idx]

  return(majority)

}

umap_coords <- tibble::tibble( x = rnorm(1000),
                       y = rnorm(1000),
                       cluster = rep(c(1,2,3,4,5), 200))

colors <- c("#8DD3C7",
            "#FFFFB3",
            "#BEBADA",
            "#FB8072",
            "#80B1D3")
names(colors) <- 1:5


hexb <- hexbin::hexbin(umap_coords$x,
                       umap_coords$y,
                       xbins = 10,
                       xbnds = c(min(umap_coords$x),
                                 max(umap_coords$x)),
                       ybnds = c(min(umap_coords$y),
                                 max(umap_coords$y)),
                       IDs = TRUE)

gghex <- data.frame(hexbin::hcell2xy(hexb),
                    count = hexb@count,
                    cell = hexb@cell,
                    xo = hexb@xcm,
                    yo = hexb@ycm,
                    hexclust = NA)


for (i in seq_along(gghex$cell)){

  cell_id <- gghex$cell[i]
  hcnt <- gghex$count[i]

  orig_id <- which(hexb@cID == cell_id)
  umap_coords[orig_id,"hexbin"] <- cell_id

  gghex$hexclust[i] <- get_majority(umap_coords[orig_id, "cluster"])

}


hex_colors <- vector(mode = "character", length = length(gghex$cell))


# For simplicity, here I assign a fixed color per cluster.
for (n in seq_along(gghex$cell)){

  hex_colors[n] <- colors[names(colors) == gghex$hexclust[n]]

}

gghex$colors <- hex_colors

ggplot2::ggplot(gghex, ggplot2::aes(x, y, fill = colors)) +
  ggplot2::geom_hex(stat = 'identity') +
  ggplot2::geom_text(ggplot2::aes(label = paste(colors, hexclust, sep = '\n')), size = 2.5) +
  ggplot2::scale_fill_identity()






data(api, package = "survey")
dclus2 <- survey::svydesign(id=~dnum+snum, weights=~pw, data=apiclus2, fpc=~fpc1+fpc2)
scatterPlotDesign(dclus2, "meals", "api00")
scatterPlotDesign(dclus2, "meals", "api00", "stype")
scatterPlotDesign(dclus2, "meals", "api00", "stype", splitMethod = "group")
scatterPlotDesign(dclus2, "meals", "api00", c("stype", "awards"), splitMethod = "facet",
                  minAlpha = .5, maxAlpha = .7)
scatterPlotDesign(dclus2, "meals", "api00", c("stype", "awards"), splitMethod = "group",
                  minAlpha = .5, maxAlpha = .7)
scatterPlotDesign(dclus2, "meals", "api00", c("stype", "awards"), splitMethod = "facet",
                  minAlpha = .25, maxAlpha = 1.0, mapWeightsToSize = TRUE)



# setup for a customizable scatter plot
df <- model.frame(dclus2)
xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$meals)
yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$api00)
pScatter <- scatterPlotDesign(dclus2, "meals", "api00", "stype", splitMethod = "group", xBreaks = xBreaks, yBreaks = yBreaks)
pHistTop   <- ggSvyHist(dclus2, "meals", xBreaks = xBreaks, addRangeFrame = FALSE) + ggplot2::theme_void()
pHistRight <- ggSvyHist(dclus2, "api00", xBreaks = yBreaks, addRangeFrame = FALSE) + ggplot2::theme_void() + ggplot2::coord_flip()

# debug_x_thm <- ggplot2::theme(
#   axis.line.x = ggplot2::element_line(),
#   axis.ticks.x = ggplot2::element_line(),
#   axis.ticks.length.x = ggplot2::unit(3, "points"),
#   axis.text.x = ggplot2::element_text()
# )
# debug_y_thm <- ggplot2::theme(
#   axis.line.y = ggplot2::element_line(),
#   axis.ticks.y = ggplot2::element_line(),
#   axis.ticks.length.y = ggplot2::unit(10, "pt"),
#   axis.text.y = ggplot2::element_text()
# )
# pHistTop <- pHistTop + debug_x_thm
# pHistRight <- pHistRight + debug_y_thm

# layout <- "AAAA#
#            BBBBC
#            BBBBC
#            BBBBC
#            BBBBC"
layout <- c(
  patchwork::area(1, 1, 1, 4),
  patchwork::area(2, 1, 5, 4),
  patchwork::area(2, 5, 5, 5)
)
# plot(layout)
pHistTop + pScatter + pHistRight +
  patchwork::plot_layout(
    axes    = "collect",
    guides  = "collect",
    design  = layout
  )
