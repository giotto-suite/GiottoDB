
# Convert giotto Object to GiottoDB

[**Source code**](/tree/main/R/as_giottodb.R#L34)

## Description

A convenience function that coerces a giotto object to a GiottoDB object
with a database backend. Expression matrices are converted to dbMatrix
objects and spatial data are converted to dbSpatial objects.

## Usage

<pre><code class='language-R'>as_giottodb(
  x,
  con = NULL,
  db_path = NULL,
  prefix = "gdb_",
  overwrite = FALSE,
  verbose = TRUE,
  compute = FALSE
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
A giotto object
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="con">con</code>
</td>
<td>
A DBI connection from a duckdb connection object
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="db_path">db_path</code>
</td>
<td>
Path to the database file if creating a new persistent connection
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="prefix">prefix</code>
</td>
<td>
A string prefix to add to database table names
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="overwrite">overwrite</code>
</td>
<td>
Whether to overwrite existing tables
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="verbose">verbose</code>
</td>
<td>
Whether to print progress messages
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="compute">compute</code>
</td>
<td>
Whether to persist dbMatrix and dbSpatial tables using dplyr::compute
</td>
</tr>
</table>

## Value

A GiottoDB object

## Examples

``` r
library(GiottoDB)

library(GiottoDB)
library(dbSpatial)

# Create connection
con <- dbConnect(duckdb(), dbdir = ":memory:")
loadSpatial(con)

# Convert Giotto object
my_giotto_db <- as_giottodb(my_giotto, con = con)

# Don't forget to close the connection when done
dbDisconnect(con, shutdown = TRUE)
```
