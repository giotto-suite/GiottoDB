# Overview

## Overview

`GiottoDB` is a module in the [Giotto Suite
ecosystem](https://drieslab.github.io/Giotto_website/articles/ecosystem.html)
that provides database support for core `Giotto` functionality through
[dbverse](https://github.com/dbverse-org).

`GiottoDB` stores expression matrices and spatial geometries as
database-backed `dbMatrix` and `dbSpatial` objects while preserving
familiar Giotto workflows for filtering, normalization, feature
selection, dimension reduction and visualization.

## GiottoDB function support

The table below summarizes GiottoDB-native methods and Giotto functions
that are supported for GiottoDB objects.

| Category | Function | Description |
|----|----|----|
| Object creation | [`GiottoDB()`](https://giotto-suite.github.io/GiottoDB/reference/GiottoDB.md) | Create a database-backed GiottoDB object. |
| Object creation | [`as_giottodb()`](https://giotto-suite.github.io/GiottoDB/reference/as_giottodb.md) | Convert a Giotto object to a GiottoDB object. |
| Object creation | [`as_giotto()`](https://giotto-suite.github.io/GiottoDB/reference/as_giotto.md) | Convert a GiottoDB object back to an in-memory Giotto object. |
| Saving and loading | [`saveGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/saveGiotto.md) | Save a GiottoDB object with its database-backed data. |
| Saving and loading | [`loadGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/loadGiotto.md) | Load a saved GiottoDB object. |
| Saving and loading | [`loadGiottoDB()`](https://giotto-suite.github.io/GiottoDB/reference/loadGiottoDB.md) | Load a GiottoDB object from a saved GiottoDB project. |
| Connection management | [`dbReconnect()`](https://giotto-suite.github.io/GiottoDB/reference/dbReconnect-GiottoDB-method.md) | Reconnect stale database-backed slots after loading. |
| Configuration | [`GiottoDB-options`](https://giotto-suite.github.io/GiottoDB/reference/GiottoDB-options.md) | Review global options used by GiottoDB-backed workflows. |
| Expression objects | [`createExprObj()`](https://drieslab.github.io/GiottoClass/reference/createExprObj.html) | Create Giotto expression objects that can contain database-backed matrices. |
| Expression access | [`getExpression()`](https://drieslab.github.io/GiottoClass/reference/getExpression.html) | Retrieve expression objects or matrices from GiottoDB objects. |
| Expression processing | [`filterGiotto()`](https://drieslab.github.io/Giotto_website/reference/filterGiotto.html) | Filter cells and features while preserving database-backed expression data. |
| Expression processing | [`processExpression()`](https://drieslab.github.io/Giotto_website/reference/processExpression.html) | Run supported expression-processing workflows on dbMatrix-backed expression data. |
| Expression processing | [`processData()`](https://giotto-suite.github.io/GiottoDB/reference/processData-dbMatrix-methods.md) | Run dbMatrix-backed expression processing methods used by Giotto workflows. |
| Expression processing | [`normalizeGiotto()`](https://giotto-suite.github.io/GiottoDB/reference/normalizeGiotto.md) | Normalize database-backed expression matrices. |
| Expression processing | [`addStatistics()`](https://giotto-suite.github.io/GiottoDB/reference/addStatistics.md) | Add cell and feature statistics from database-backed matrices. |
| Expression processing | [`calculateHVF()`](https://giotto-suite.github.io/GiottoDB/reference/calculateHVF.md) | Identify highly variable features from database-backed matrices. |
| Dimension reduction | [`runPCA()`](https://giotto-suite.github.io/GiottoDB/reference/runPCA.md) | Run PCA using database-backed matrix operations. |
| Dimension reduction | `runUMAP()` | Run UMAP from supported reduced dimensions. |
| Clustering | [`doLeidenCluster()`](https://giottosuite.com/reference/doLeidenCluster.html) | Cluster cells from supported nearest-neighbor graphs or reduced dimensions. |
| Clustering | [`doLeidenClusterIgraph()`](https://giottosuite.com/reference/doLeidenClusterIgraph.html) | Run Leiden clustering through Giotto’s igraph workflow. |
| Differential expression | [`findMarkers_one_vs_all()`](https://giotto-suite.github.io/GiottoDB/reference/findMarkers_one_vs_all.md) | Run one-versus-all marker testing on supported GiottoDB objects. |
| Subcellular workflow | [`createGiottoPoints()`](https://giotto-suite.github.io/GiottoDB/reference/createGiottoPoints.md) | Create Giotto point objects backed by dbSpatial where applicable. |
| Subcellular workflow | [`createGiottoPolygon()`](https://giotto-suite.github.io/GiottoDB/reference/createGiottoPolygon.md) | Create Giotto polygon objects backed by dbSpatial where applicable. |
| Subcellular workflow | [`calculateOverlap()`](https://giotto-suite.github.io/GiottoDB/reference/calculateOverlap.md) | Compute overlaps between spatial features. |
| Subcellular workflow | [`overlapToMatrix()`](https://giotto-suite.github.io/GiottoDB/reference/overlapToMatrix.md) | Convert spatial overlaps to matrix-like outputs. |
| Subcellular workflow | [`tessellate()`](https://giotto-suite.github.io/GiottoDB/reference/tessellate.md) | Generate square or hexagonal tessellations. |
| Visualization | `spatPlot2D()` | Create standard Giotto spatial plots for supported GiottoDB objects. |
| Visualization | [`spatInSituPlotPoints()`](https://giotto-suite.github.io/GiottoDB/reference/spatInSituPlotPoints.md) | Visualize in situ point features from GiottoDB objects. |
| Visualization | `plotPCA()` | Plot PCA embeddings from GiottoDB objects. |
| Visualization | `plotUMAP()` | Plot UMAP embeddings from GiottoDB objects. |

## Dynamic Method Dispatch

GiottoDB implements database-backed methods for selected Giotto
workflows. Unsupported Giotto functions may require conversion back to
an in-memory Giotto object with
[`as_giotto()`](https://giotto-suite.github.io/GiottoDB/reference/as_giotto.md).

## Performance

Benchmark summaries are available in the [benchmarks
vignette](https://giotto-suite.github.io/GiottoDB/articles/Benchmarks.md).
