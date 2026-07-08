# Package index

## Object creation

- [`GiottoDB-class`](https://giotto-suite.github.io/GiottoDB/reference/GiottoDB-class.md)
  [`GiottoDB`](https://giotto-suite.github.io/GiottoDB/reference/GiottoDB-class.md)
  : GiottoDB Class
- [`GiottoDB()`](https://giotto-suite.github.io/GiottoDB/reference/GiottoDB.md)
  : Create a new GiottoDB object
- [`as_giotto()`](https://giotto-suite.github.io/GiottoDB/reference/as_giotto.md)
  : Convert GiottoDB Object to giotto
- [`as_giottodb()`](https://giotto-suite.github.io/GiottoDB/reference/as_giottodb.md)
  : Convert giotto Object to GiottoDB

## Saving and loading

- [`loadGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/loadGiotto.md)
  : Load Giotto object
- [`loadGiottoDB()`](https://giotto-suite.github.io/GiottoDB/reference/loadGiottoDB.md)
  : Load GiottoDB object with explicit connection
- [`saveGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/saveGiotto.md)
  : Save Giotto object

## Connection management

- [`dbReconnect(`*`<GiottoDB>`*`)`](https://giotto-suite.github.io/GiottoDB/reference/dbReconnect-GiottoDB-method.md)
  : Reconnect a GiottoDB object

## Configuration

- [`GiottoDB-options`](https://giotto-suite.github.io/GiottoDB/reference/GiottoDB-options.md)
  : GiottoDB Global Options

## Expression processing

- [`addStatistics()`](https://giotto-suite.github.io/GiottoDB/reference/addStatistics.md)
  : Add statistics for Giotto / GiottoDB
- [`calculateHVF()`](https://giotto-suite.github.io/GiottoDB/reference/calculateHVF.md)
  : Calculate Highly Variable Features for GiottoDB
- [`normalizeGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/normalizeGiotto.md)
  : Normalize Expression for GiottoDB with dbMatrix support
- [`processData()`](https://giotto-suite.github.io/GiottoDB/reference/processData-dbMatrix-methods.md)
  : Count Matrix Processing for GiottoDB Objects

## Dimension reduction

- [`runPCA()`](https://giotto-suite.github.io/GiottoDB/reference/runPCA.md)
  :

  Run PCA on GiottoDB with
  [`dbMatrix::dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.html)
  support

## Differential expression

- [`findMarkers_one_vs_all()`](https://giotto-suite.github.io/GiottoDB/reference/findMarkers_one_vs_all.md)
  : Find marker features one-vs-all with GiottoDB support

## Subcellular workflow

- [`calculateOverlap(`*`<dbSpatial>`*`,`*`<dbSpatial>`*`)`](https://giotto-suite.github.io/GiottoDB/reference/calculateOverlap.md)
  : Calculate overlaps with dbSpatial-backed objects

- [`createGiottoPoints(`*`<dbSpatial>`*`)`](https://giotto-suite.github.io/GiottoDB/reference/createGiottoPoints.md)
  : Create GiottoPoints object using dbSpatial

- [`createGiottoPolygon(`*`<dbSpatial>`*`)`](https://giotto-suite.github.io/GiottoDB/reference/createGiottoPolygon.md)
  : Create GiottoPolygon object using dbSpatial

- [`overlapToMatrix(`*`<dbSpatial>`*`)`](https://giotto-suite.github.io/GiottoDB/reference/overlapToMatrix.md)
  : Convert dbSpatial overlaps to a matrix

- [`tessellate()`](https://giotto-suite.github.io/GiottoDB/reference/tessellate.md)
  :

  Tessellate a `dbSpatial` object

## Visualization

- [`spatInSituPlotPoints()`](https://giotto-suite.github.io/GiottoDB/reference/spatInSituPlotPoints.md)
  : Plot in situ points with GiottoDB support
