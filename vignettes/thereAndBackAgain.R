## ----setup, include=FALSE, warning=FALSE, message=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE,
                      message = FALSE,
                      fig.align = "center", 
                      fig.width = 6, 
                      fig.height = 5,
                      out.width = "40%", 
                      collapse = TRUE,
                      comment = "#>",
                      tidy.opts = list(width.cutoff = 65),
                      tidy = FALSE)
library(knitr)
set.seed(12314159)
imageDirectory <- "./l_ggplot"
dataDirectory <- "./l_ggplot"

library(grid, quietly = TRUE)
library(gridExtra, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(loon, quietly = TRUE)
library(loon.ggplot, quietly = TRUE)

## ----fake data----------------------------------------------------------------
data <- data.frame(A = c(19, 19, 25, 62, 34, 
                         58, 62, 40, 24, 60, 
                         70, 40, 40, 34, 26),
                   B = c(68, 63, 63, 13, 55,
                         78, 14, 14, NA, 28,
                         NA, 55, 57, 40, 78) )

## ----ggplot-------------------------------------------------------------------
ggp <- ggplot(data, 
               mapping = aes(x = A, y = B)) +
  ggtitle("Some title") +
  geom_point(color = "grey", size = 5) +
  linking(linkingGroup = "my plots")
plot(ggp)

## ----ggp states---------------------------------------------------------------
# get the ggplot data corresponding to loon "states"
ggp_states <- ggplot_build(ggp)$data[[1]]
ggp_states

## ----loon plot----------------------------------------------------------------
lp <- loon.ggplot(ggp)
plot(lp)

## ----loon plot states---------------------------------------------------------
names(lp)
# and accessed with [] as in 
lp["title"]

## ----loon n-------------------------------------------------------------------
lp["n"]

## ----glyphs-------------------------------------------------------------------
ggp_states$shape
lp["glyph"]

## ----sizes--------------------------------------------------------------------
ggp_states$size
lp["size"]

## ----colours------------------------------------------------------------------
ggp_states$colour
lp["color"]

## ----tkcolors, echo = FALSE, out.width="60%"----------------------------------
tohex <- function(x) {
    sapply(x, function(xi) {
        crgb <- as.vector(col2rgb(xi))
        rgb(crgb[1], crgb[2], crgb[3], maxColorValue = 255)
    })}

df <- data.frame(
    R_col = tohex(colors()),
    Tcl_col = hex12tohex6(l_hexcolor(colors())),
    row.names = colors(),
    stringsAsFactors = FALSE
)

df_diff <- df[df$R_col != df$Tcl_col,]

if (requireNamespace("grid", quietly = TRUE)) {
  grid::grid.newpage()
  grid::pushViewport(grid::plotViewport())

  x_col <- grid::unit(0, "npc")
  x_R <- grid::unit(6, "lines")
  x_Tcl <- grid::unit(10, "lines")

  grid::grid.text('color', x=x_col, y=grid::unit(1, "npc"),
                  just='left', gp=grid::gpar(fontface='bold'))
  grid::grid.text('R', x=x_R, y=grid::unit(1, "npc"), just='center',
                   gp=grid::gpar(fontface='bold'))
  grid::grid.text('Tcl', x=x_Tcl, y=grid::unit(1, "npc"), just='center',
                   gp=grid::gpar(fontface='bold'))
  for (i in 1:nrow(df_diff)) {
      y <- grid::unit(1, "npc") - grid::unit(i*1.2, "lines")
      grid::grid.text(rownames(df_diff)[i], x=x_col, y=y, just='left')
      grid::grid.rect(x=x_R, y=y, width=grid::unit(3, "line"),
                height=grid::unit(1, "line"), gp=grid::gpar(fill=df_diff[i,1]))
      grid::grid.rect(x=x_Tcl, y=y, width=grid::unit(3, "line"),
                height=grid::unit(1, "line"), gp=grid::gpar(fill=df_diff[i,2]))
  }
}

## ----tohex show, eval = FALSE-------------------------------------------------
#  tohex <- function(x) {
#      sapply(x, function(xi) {
#          crgb <- as.vector(col2rgb(xi))
#          rgb(crgb[1], crgb[2], crgb[3], maxColorValue = 255)
#      })}

## ----missing data-------------------------------------------------------------
nrow(ggp_states) == lp["n"]
# Compare
ggp_states$y
lp["y"]

## ----missing linking keys-----------------------------------------------------
lp["linkingKey"]

## ----ggp from lp--------------------------------------------------------------
ggp_lp_1 <- loon.ggplot(lp)
ggp_lp_1

## ----ggp_lp_1 states----------------------------------------------------------
ggp_lp_1_states <- ggplot_build(ggp_lp_1)$data[[1]]
ggp_lp_1_states

lp_ggp_lp_1 <- loon.ggplot(ggp_lp_1)

## ----change loon plot---------------------------------------------------------
selection <- lp["x"] > 50 &lp["y"] > 13
lp["selected"] <- selection
colorMeRed <- lp["x"] == 34
lp["color"][colorMeRed] <- "red"

## -----------------------------------------------------------------------------
# Get a ggplot from the loon plot
ggp_lp <- loon.ggplot(lp)

## ----loon to ggplot, echo = FALSE, fig.width = 8, fig.height = 4.5, out.width = "80%", warning = FALSE----
# need grid for text
library(grid)
grid.arrange(plot(lp, draw = FALSE),               # the interactive plot
             ggp_lp,                               # the ggplot fromlp 
             grid.text("lp"),
             grid.text("ggp_lp"),               
             ncol = 2,
             nrow = 2,
             widths = c(0.45, 0.55),
             heights = c(0.5, 0.1))

## -----------------------------------------------------------------------------
# The loon plot from the resulting ggplot
lp_ggp_lp <- loon.ggplot(ggp_lp)

## -----------------------------------------------------------------------------
# The original loon plot has linking keys
lp["linkingKey"]
# And the loon plot from the derived ggplot
lp_ggp_lp["linkingKey"]

## -----------------------------------------------------------------------------
# The original point order
lp["x"]
# The new plot's order
lp_ggp_lp["x"]

## -----------------------------------------------------------------------------
# Original selected
lp["selected"]
# the new plot has nothing selected
lp_ggp_lp["selected"]

## -----------------------------------------------------------------------------
# Original selected
lp["color"]
# the new plot has nothing selected
lp_ggp_lp["color"]

## -----------------------------------------------------------------------------
# Add the linking information when creating the interactive plot
lp_ggp_l1_lk <- loon.ggplot(ggp_lp, 
                            linkingKey =lp["linkingKey"], 
                            linkingGroup = "NA example")
# Now compare
lp_ggp_l1_lk["linkingKey"]
# to the original loon plot
lp["linkingKey"]

## -----------------------------------------------------------------------------
# Add the linking information when creating the interactive plot
lp_ggp_l1_ggk <- loon.ggplot(ggp_lp + 
                               linking(linkingGroup = "NA example",
                                       linkingKey =lp["linkingKey"]))
# Again compare
lp_ggp_l1_ggk["linkingKey"]
# to the original loon plot
lp["linkingKey"]

