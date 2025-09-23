
# GiottoDB Class

## Description

S4 class that extends the giotto class to provide a database-backed
implementation of Giotto objects using dbMatrix and dbSpatial.

## Details

The GiottoDB class extends the standard giotto class, replacing
in-memory objects with database-backed alternatives where appropriate:

<ul>
<li>

Expression matrices (matrix, Matrix) are replaced with dbMatrix objects

</li>
<li>

Spatial objects (points, polygons) are replaced with dbSpatial objects

</li>
</ul>

This allows Giotto to scale to larger-than-memory datasets while
maintaining API compatibility with existing Giotto workflows.

## Value

A GiottoDB object

## Slots

<dl>
<dt>
<code>conn</code>
</dt>
<dd>
A DBI connection to a duckdb database
</dd>
</dl>
