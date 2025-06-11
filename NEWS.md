# loon.ggplot 1.3.5

1. update external doc \link{bar} to include package names \link{foo:bar}

2. Updated DESCRIPTION to requiere ggplot2 >= 3.5.0 and made necessary changes
   - argument changes (trans -> transform, scale_name -> deprecated, palette is new) 
     for calls to ggplot2::continuous_scale()
   - is.ggplot() -> is_ggplot()
   
# loon.ggplot 1.3.4

1. The recently released ggplot2 version 3.5.0 introduces coord_radial as a new function, 
   which conflicts with the function we utilized, leading to the package check failure.

2. Review all examples to ensure they execute flawlessly.

# loon.ggplot 1.3.3

Fix bugs resulted from the new released version of `ggplot2` 3.4.0

* 1. in `ggplot2` 3.4.0, the aesthetic attribute `size` is no longer recommended to set the line width for all geometric objects. Most errors and warnings are caused by this.

* 2. in the new release of `ggplot2`, `GeomCol` (geometric layer `geom_col`) is now inherited from `GeomBar` (its supper class). Therefore, in the transformation, a `ggplot` with  `geom_col()` layer will be converted to an interactive `l_hist` plot, which should not be (note that, only `geom_bar` and `geom_histogram` could be turned an interactive `l_hist` plot).

# loon.ggplot 1.3.2

* No functionality changes. A minor change is made on the website html.

# loon.ggplot 1.3.1

* **Important Update**: to transform a `loon` `l_facet` object to a `ggplot` object, instead of relying on the package `patchwork` to combine plots, the function `facet_grid()` or `facet_wrap()` is used.

* A `patchwork` object can be transformed to a `loon` `l_compound` object.

* In the previous versions of `ggplot2` (< 3.3.5), both `none` and `FALSE` work in `guides()`. In the latest version (e.g., >= 3.3.5), `FALSE` is deprecated. Therefore, to set `guides()` in the package `loon.ggplot`, we use `none`.

* Fix bugs: 

  + To transform a `ggplot` object to a `loon` plot, if the points shapes are between 21 to 25, the variable "fill" is used as the `loon` points color; if the variable "fill" is not found or set as NA, then the variable "colour" is used.
  
  + To transform a `ggplot` barplot to a `loon` `l_hist` widget, the binwidth is set as 1 by default.

# loon.ggplot 1.3.0

* **Important Changes**: when we turn a `loon` compound object to a `ggplot` object, package `patchwork` is used (not `GGally`). The benefit is that the size of each plot can be different.

* Fix a bug: a error is encountered when we transform a swapped `loon` histogram to a `ggplot` object.

* A `loon` `zenplot` can be turned to a `ggplot` object via the function `loon.ggplot()` (or `loon2ggplot()`).

# loon.ggplot 1.2.1

* Fix a bug: in the previous versions, to transform a `ggplot` object to a `loon` plot, `showItemLabels` and `itemLabels` are failed to be passed into the `l_serialaxes` widget. 

* Set `ggmulti` as "Imports" rather than the "Depends", and replace `coord_serialaxes` to `geom_serialaxes` in order to avoid potential failures (in `coord_serialaxes`).

* Executing the function `print.l_ggplot()`, a message will be given to show the `loon` plot path name.

# loon.ggplot 1.2.0

* Class name modification: in the earlier versions, to transform a `ggplot` object with facets to a `loon` plot, the class of the returned object is c("l_ggplot", "l_compound", "loon"). However, for an `l_ggplot()` function, the class of the returned object is c("lggplot", "gg", "ggplot") which is confusing. In this version, the class of the former one is **c("l_facet_ggplot", "l_facet", "l_compound", "loon")**; and the class of latter one is **c("l_ggplot", "gg", "ggplot")**.

* For an `l_facet_ggplot` object (transformed from the ggplot with multiple `facet`s), the returned object is changed. 

  - In the earlier versions, the returned object is a list of three components: 
  
    + `plots`: a list of loon widgets;
    
    + `facet`: a list of four logical components, `FacetWrap`, `FacetGrid`, `byCOLS` and `byROWS`
    
    + `titles`: a list of three components, title of the whole widget, column subtitles and row subtitles.
    
  - In this version, the returned object is just a list of `loon` widgets, like other `l_compound` objects (e.g., `l_facet`, `l_pairs`).
  
* Two changes are made when we transform a `ggplot` histogram to a `loon` `l_hist` widget: 

  + If the `ggplot` histogram shows "density", the area of each category (grouped by color) is 1; however, in an `l_hist` widget, the whole area is 1 and the area of each category is proportional to the counts. Thus, after transformation, the y limits are identical but the display could be very different. To get a better display, the y limits are released (may result the different visual displays) and a message is given.
  
  + Suppose the transformed `loon` histograms are joined into a linking group, the colors of each bin could be different from the original `ggplot` object. Therefore, the state `colorStackingOrder` will be reset. 
  
* New interactive component 

  + `scaleTo`: used to change the region (i.e., scale to selected points, active points, or a specific geometric layer) of the `loon` plot.
  
  + `active`: determine which geom layer could be interactive or which points could be activated. 
  
* From `loon` to `ggplot`, a logical argument `asAes` is given (default is `TRUE`). If `TRUE`, the color, size, other aesthetics will be taken as variables and set in the function `aes()`; else they are the general aesthetics attributes.
  
* Fix a bug: the NA of some n dimensional states should be checked before passing through. 

* A warning is given once multiple active layers are not set. 

* Remove the `tidyverse` dependency

# loon.ggplot 1.1.0

* When a `loon` widget --> a `ggplot` object, several things are changed here:

  1. Add new features (e.g., 'Andrews curves');
  
  2. If `z` is provided in the mapping aesthetics of `geom_point`, an `l_plot3D` object would be created.

  3. In `loon`, if points have non-primitive glyphs (viz., `text` glyph, `pointrange` glyph, `image` glyph, `polygon` glyph, and `serialaxes` glyph), in the transformation, functions `geom_imageGlyph()`, `geom_polygonGlyph()`, `geom_pointrangeGlyph()`, `geom_serialaxesGlyph()` and `geom_textGlyph()` are deprecated and not maintained any more. Instead, we use the package `ggmulti` who provides functions `geom_image_glyph()`, `geom_polygon_glyph()` and `geom_serialaxes_glyph()` to draw the non-primitive glyphs (for the layer `geom_text` and `geom_pointrange`, they would be turned into an interactive layer automatically. To turn them as static, one can set the argument `activeGeomLayers` as 0).
  
  4. To transform an `l_serialaxes` object, the function `ggSerialAxes()` is deprecated and `ggmulti::coord_serialaxes()` is used.
  
  5. Some `ggplot2` extensions provide new `geom` layers (e.g., `geom_textpath`). To transform these layers in `loon`, one can customize the function `loonLayer()`.
  
# loon.ggplot 1.0.0

* 2020-06-17: Welcome to the world 
