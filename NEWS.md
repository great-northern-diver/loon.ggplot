# loon.ggplot 1.1.0

* When a `loon` widget --> a `ggplot` object, several things are changed here

  1. To meet the changes (or new features) of the loon version 1.3.2 (or higher version), i.e. 'Andrews curves'.
  
  2. For layer `geom_point`, if `z` is provided in mapping aesthetics, an `l_plot3D` object could be created.

  3. If the points are non-primitive glyphs, (i.e. `text` glyph, `pointrange` glyph, `image` glyph, `polygon` glyph, and `serialaxes` glyph), during the tranformation, function `geom_imageGlyph`, `geom_polygonGlyph`, `geom_pointrangeGlyph`, `geom_serialaxesGlyph` and `geom_textGlyph` are deprecated and not maintained any more. Instead, we encourage people to use package `ggmulti` which provides `geom_image_glyph`, `geom_polygon_glyph` and `geom_serialaxes_glyph` to draw such glyphs. For layer `geom_text` and `geom_pointrange`, they can be interactive automatically. However, if users do not expect any of them to be interactive, the argument `activeGeomLayers` can be set as 0.
  
  4. For a `l_serialaxes` object, `ggSerialaxes` is deprecated and `ggmulti::coord_serialaxes()` is suggested.
  
  5. For some `ggplot2` extension packages (providing new `geom` layers), one can edit the function `loonLayer` to realize the transformation.
