
# mvtview

Provides a development Mapbox Vector Tile server and viewer for R, powered by {plumber} and {rdeck}.

## Installation

You can install the development version of {mvtview} from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MilesMcBain/mvtview")
```

## Usage

Serving an .mbtiles file for map making:

```r
library(mvtview)

serve_mvt("my_tiles.mbtiles")
```

Or alternatively, serving and then viewing tile data in an .mbtiles database on an interactive map:

```r
view_mvt("my_tiles.mbtiles)
```

Clean up running servers:

```r
clean_mvt()
```

## Prior art

* [mbview](https://github.com/mapbox/mbview) a NodeJS vector tile viewer by Mapbox

## Vector tile stuff

* [Awesome vector tiles](https://github.com/mapbox/awesome-vector-tiles) vector tile generators, servers and more.