# Overview

## Overview

`GiottoDB` is a module in the [Giotto Suite
ecosystem](https://drieslab.github.io/Giotto_website/articles/ecosystem.html)
that provides database support for core `Giotto` functionality through
the [dbverse](https://github.com/drieslab).

[overview.png](https://giotto-suite.github.io/path/to/overview.png)

## GiottoDB function support

The following non-exhaustive list of functions show which core `Giotto`
functions are supported by `GiottoDB` objects:

#### Constructors

[`GiottoDB()`](https://rdrr.io/pkg/GiottoDB/man/GiottoDB-class.html) -
Create database-backed Giotto object

[`as_giottodb()`](https://giotto-suite.github.io/GiottoDB/reference/as_giottodb.md) -
Convert giotto to GiottoDB

#### Saving and Loading

[`saveGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/saveGiotto.md) -
Save GiottoDB with database files

[`loadGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/loadGiotto.md) -
Load saved GiottoDB object

#### Subcellular

[createGiottoPoints()](https://drieslab.github.io/GiottoClass/reference/createGiottoPoints.html)

[createGiottoPolygon()](https://drieslab.github.io/GiottoClass/reference/createGiottoPolygon.html)

[calculateOverlap()](https://drieslab.github.io/GiottoClass/reference/calculateOverlap.html)

[overlapToMatrix()](https://drieslab.github.io/GiottoClass/reference/overlapToMatrix.html)

[tessellate()](https://drieslab.github.io/GiottoClass/reference/tessellate.html) -
Generate hex/square tessellations

#### Expression

[createExprObj()](https://drieslab.github.io/GiottoClass/reference/createExprObj.html)

[getExpression()](https://drieslab.github.io/GiottoClass/reference/getExpression.html)

[filterGiotto()](https://drieslab.github.io/Giotto_website/reference/filterGiotto.html)

[processExpression()](https://drieslab.github.io/Giotto_website/reference/processExpression.html)

[normalizeGiotto()](https://drieslab.github.io/Giotto_website/reference/normalizeGiotto.html)

[addStatistics()](https://drieslab.github.io/Giotto_website/reference/addStatistics.html)

[calculateHVF()](https://drieslab.github.io/Giotto_website/reference/calculateHVF.html)

#### Dimension Reduction

[`runPCA()`](https://giotto-suite.github.io/GiottoDB/reference/runPCA.md) -
PCA via streaming SVD and support for BPCells::svds()

[`runUMAP()`](https://rdrr.io/pkg/Giotto/man/runUMAP.html) - UMAP
embedding (inherited)

#### Clustering

[doLeidenCluster()](https://giottosuite.com/reference/doLeidenCluster.html)

[doLeidenClusterIgraph()](https://giottosuite.com/reference/doLeidenClusterIgraph.html)

#### Visualization

[`spatPlot2D()`](https://giotto-suite.github.io/GiottoVisuals/reference/spatPlot.html) -
Spatial 2D plots

[`spatInSituPlotPoints()`](https://giotto-suite.github.io/GiottoVisuals/reference/spatInSituPlotPoints.html) -
In situ points visualization

[`plotPCA()`](https://giotto-suite.github.io/GiottoVisuals/reference/plotPCA.html) -
PCA scatter plots

[`plotUMAP()`](https://giotto-suite.github.io/GiottoVisuals/reference/plotUMAP.html) -
UMAP scatter plots

## Dynamic Method Dispatch

While `GiottoDB` provides scalable database-backed methods of core
`Giotto` functions, it does not natively support all of Giotto’s
functionality.

## Performance

Benchmark can be found at the [benchmarks
vignette](https://giotto-suite.github.io/GiottoDB/articles/path/to/benchmarks).
