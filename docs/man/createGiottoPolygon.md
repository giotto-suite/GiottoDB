
# Create GiottoPolygon object using dbSpatial

## Description

Create a <code>giottoPolygon</code> object that wraps a dbSpatial
polygons object to support larger-than-memory spatial polygon data. This
implementation extends the standard GiottoClass implementation by
providing specific methods for dbSpatial objects.

## Usage

<pre><code class='language-R'>## S4 method for signature 'dbSpatial'
createGiottoPolygon(
  x,
  name = "polygons",
  verbose = TRUE,
  split_keyword = NULL,
  unique_IDs = NULL,
  calc_centroids = FALSE
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
dbSpatial object, SpatVector, or data.frame-like object with polygon
coordinate information (must include poly_ID column)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="name">name</code>
</td>
<td>
character. Name for the polygon object
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
be verbose
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="split_keyword">split_keyword</code>
</td>
<td>
list of character vectors of keywords to split the giottoPolygon object
based on their poly_ID. Keywords will be <code>grepl()</code> matched
against the polygon IDs information.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="unique_IDs">unique_IDs</code>
</td>
<td>
(optional) character vector of unique IDs present within the spatVector
data. Provided for cacheing purposes
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="calc_centroids">calc_centroids</code>
</td>
<td>
logical. Whether to calculate centroids for the polygons
</td>
</tr>
</table>

## Value

giottoPolygon object wrapping a dbSpatial object
