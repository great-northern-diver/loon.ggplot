# loon.ggplot 1.2.0

* Class name modification

  - In 1.1.0 or earlier version, if a `ggplot` object with facets is transformed to a `loon` widget, the class of a returned object is c("l_ggplot", "l_compound", "loon"). However, for an `l_ggplot()` function, the class of a returned object is c("lggplot", "gg", "ggplot") which brings much confusion. 
  
  - To eliminate the confustion, in 1.2.0 version, if a `ggplot` object with facets is transformed to a `loon` widget, the class of a returned object is **c("l_facet_ggplot", "l_facet", "l_compound", "loon")**; for an `l_ggplot()` function, the class of a returned object is **c("l_ggplot", "gg", "ggplot")**.

* For an `l_facet_ggplot` object (transformed from the ggplot with multiple `facet`s), the returned object is changed. 

  - In 1.1.0 or earlier version, the returned object is a list of three components: 
  
    + `plots`: a list of loon widgets;
    
    + `facet`: a list of four logical components, `FacetWrap`, `FacetGrid`, `byCOLS` and `byROWS`
    
    + `titles`: a list of three components, title of the whole widget, column subtitles and row substitles.
    
  - In 1.2.0 version, the returned object is just a list of `loon` widgets, like other `l_compound` objects, e.g. a `l_facet` object, a `l_pairs` object, etc.
  
* Two changes for the transformation from a `ggplot` histogram to a `loon` `l_hist` widget: 

  + If the ggplot histogram "y shows" is density, the area of each category (grouped by color) is 1; however, for an `l_hist` widget, the whole area is one and the area of each category is proportional to the counts. Thus, when the transformation occurs, the y limits are identical but the display is very different. To fix it, the y limits are released and a message is given to users to explain what happens.
  
  + Suppose the transformed `loon` histograms are joined into a linking group, the color of each bin could be different from the original `ggplot`. Thus, the state `colorStackingOrder` (queried from the `ggplot` object) will be reset to the default settings (it is meaningless to set the `colorStackingOrder` to some color that never appears). 
  
* New interactive component 

  + `scaleTo`: used to change the region (i.e. to selected points, active points or a specific geometric layer) of the `loon` plot.
  
  + `active`: determine which geom layer could be interactive or which points could be activated. 
  
* From `loon` to `ggplot`, a logical argument `asAes` is given (default is `TRUE`). If `TRUE`, the color, size, other aesthetics will be taken as variables and set in the function `aes()`; else they are the general aesthetics attributes.
  
* Fix a bug: the NA of some n dimensional states should be checked before passing through. 

* Multiple active layers are not suggested. So a warning will be given. 

* Remove `tidyverse` dependency

# loon.ggplot 1.1.0

* When a `loon` widget --> a `ggplot` object, several things are changed here

  1. To meet the changes (or new features) of the loon version 1.3.2 (or higher version), i.e. 'Andrews curves'.
  
  2. For layer `geom_point`, if `z` is provided in mapping aesthetics, an `l_plot3D` object could be created.

  3. If the points are non-primitive glyphs, (i.e. `text` glyph, `pointrange` glyph, `image` glyph, `polygon` glyph, and `serialaxes` glyph), during the tranformation, function `geom_imageGlyph`, `geom_polygonGlyph`, `geom_pointrangeGlyph`, `geom_serialaxesGlyph` and `geom_textGlyph` are deprecated and not maintained any more. Instead, we encourage people to use package `ggmulti` which provides `geom_image_glyph`, `geom_polygon_glyph` and `geom_serialaxes_glyph` to draw such glyphs. For layer `geom_text` and `geom_pointrange`, they can be interactive automatically. However, the argument `activeGeomLayers` can be set as 0, if users do not expect any of them to be interactive.
  
  4. For a `l_serialaxes` object, `ggSerialaxes` is deprecated and `ggmulti::coord_serialaxes()` is suggested.
  
  5. For some `ggplot2` extension packages (providing new `geom` layers), one can edit the function `loonLayer` to realize the transformation.
  
# loon.ggplot 1.0.0

* 2020-06-17: Welcome to the world 
