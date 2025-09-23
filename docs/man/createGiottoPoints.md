
# Create GiottoPoints object using dbSpatial

## Description

Create a <code>giottoPoints</code> object that wraps a dbSpatial points
object to support larger-than-memory spatial point data. This
implementation extends the standard GiottoClass implementation by
providing specific methods for dbSpatial objects.

## Usage

<pre><code class='language-R'>## S4 method for signature 'dbSpatial'
createGiottoPoints(
  x,
  feat_type = "rna",
  verbose = TRUE,
  split_keyword = NULL,
  unique_IDs = NULL
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
dbSpatial object, SpatVector, or data.frame-like object with points
coordinate information (x, y, feat_ID)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="feat_type">feat_type</code>
</td>
<td>
character. feature type. Provide more than one value if using the
<code>split_keyword</code> param. For each set of keywords to split by,
an additional feat_type should be provided in the same order.
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
list of character vectors of keywords to split the giottoPoints object
based on their feat_ID. Keywords will be <code>grepl()</code> matched
against the feature IDs information.
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
</table>

## Value

giottoPoints object wrapping a dbSpatial object
