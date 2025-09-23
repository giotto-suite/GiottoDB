
# Create a new GiottoDB object

[**Source code**](/tree/main/R/GiottoDB-class.R#L94)

## Description

Create a new GiottoDB object, which is a database-backed implementation
of the Giotto object.

## Usage

<pre><code class='language-R'>GiottoDB(con, ...)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="con">con</code>
</td>
<td>
A DBI connection to a duckdb database
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
Additional arguments passed to the giotto constructor
</td>
</tr>
</table>

## Value

A GiottoDB object

## Examples

``` r
library(GiottoDB)

# Create a new GiottoDB object with a new database connection
library(GiottoDB)
library(duckdb)
library(DBI)

# Create a connection to a DuckDB database
con <- dbConnect(duckdb(), dbdir = ":memory:")
dbSpatial::loadSpatial(con) # Load spatial extension

# Create a new GiottoDB object
gobj_db <- GiottoDB(con = con)

# Don't forget to close the connection when done
dbDisconnect(con, shutdown = TRUE)
```
