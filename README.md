---
title: "Poking around pokies"
author: "David Hood"
date: "3/13/2019"
output: 
  html_document: 
    keep_md: yes
---



This is not complete - this is a work in progress.

## Data Acknowledgement

In exploring Pokie distribution and deprivation in NZ, I am heavily drawing on Hamish Campbell's (@polemic on Twitter) identification and geocoding of data sources.

https://twitter.com/polemic/status/1105631444519383040

So the data is

* Pokie locations geocoded by Hamish https://gist.github.com/hamishcampbell/7d7810a9ead83147b39e2bbe969ff188

* Deprevation Index https://www.otago.ac.nz/wellington/departments/publichealth/research/hirp/otago020194.html

* 2013 Stats NZ Area Units https://datafinder.stats.govt.nz/layer/25743-area-unit-2013/

And some R libraries I will be using include

* sf (spatial feature), my current favourite for spatial analysis
* dplyr, for the grammar of data analysis
* ggplot2 for the graphs
* ggbeeswarm for the geom_quasirandom graph
* purrr to make some commands act in sequence rather than vectorised
* geosphere for distance calculations


```r
library(sf)
```

```
## Linking to GEOS 3.6.1, GDAL 2.1.3, proj.4 4.9.3
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(ggbeeswarm)
library(purrr)
library(geosphere)

dep <- read.table("otago069931.txt", header=TRUE, sep="\t",
                  stringsAsFactors = FALSE) %>% select(-CAU_name_2013) %>%
  mutate(CAU_2013 = as.character(CAU_2013))
statmap <- st_read("statsnzarea-unit-2013-SHP/area-unit-2013.shp", stringsAsFactors = FALSE)
```

```
## Reading layer `area-unit-2013' from data source `/Users/hooda84p/Syncplicity Folders/work_and_home/depindex/statsnzarea-unit-2013-SHP/area-unit-2013.shp' using driver `ESRI Shapefile'
## Simple feature collection with 2004 features and 5 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: 1067061 ymin: 4701317 xmax: 2523320 ymax: 6242140
## epsg (SRID):    NA
## proj4string:    +proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs
```

```r
depmap <- inner_join(dep, statmap, by=c("CAU_2013" = "AU2013_V1_"))
```

Lets do a general check


```r
depmap %>% count(CAU_average_NZDep2013)
```

```
## # A tibble: 10 x 2
##    CAU_average_NZDep2013     n
##                    <int> <int>
##  1                     1   186
##  2                     2   187
##  3                     3   187
##  4                     4   187
##  5                     5   186
##  6                     6   187
##  7                     7   187
##  8                     8   187
##  9                     9   187
## 10                    10   186
```

Yup, the deciles are sound.


```r
ggplot(depmap, aes(x=factor(CAU_average_NZDep2013), y=log10(LAND_AREA_),
                   colour=factor(CAU_average_NZDep2013))) + 
  theme_minimal() + ggtitle("Land Area by Deprivation Index") +
  xlab("Deprivation Index of Unit") + geom_quasirandom() + 
  geom_boxplot(alpha=0.5) +
  theme(legend.position = "none") 
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Working out AU centroids. This is one place I am diverting a little from others approach, as I am labelling the pokies with the AU of the closest centroid (which might not be the one the Pokie is in). These are the "true" centroids, which may be out of the AU if the AU is curved.


```r
pokie <- read.csv("nz-pokies-geocoded.csv", stringsAsFactors = FALSE)
depmap <- depmap  %>% 
  mutate(
    geom2 = st_transform(geometry, "+proj=longlat +ellps=WGS84 +datum=WGS84"),
    cenlon = map_dbl(geom2, ~st_centroid(.x)[[1]]),
     cenlat = map_dbl(geom2, ~st_centroid(.x)[[2]]))
```

Now assign the AU with the closest centroid to each pokie location.


```r
closest <- function(x,y, vsSet=depmap[,c("cenlon", "cenlat")], AUref=depmap$CAU_2013){
  if (!is.na(x) & !is.na(y)){
    pokie_place <- c(x,y)
    vsSet_dist <- as.vector(distm(pokie_place,vsSet))
    return(AUref[which.min(vsSet_dist)])
  } else {
    return(NA_character_)
  }
}

pokie$closest <- combine(map2(pokie$x, pokie$y, closest))
```

So, now lets take that earlier graph and split by Pokie & non-Pokie AUs.


```r
AU_w._pokies <-  pokie %>% 
  group_by(closest) %>% summarise(pokies = sum(count)) %>% 
  right_join(depmap, by=c("closest" = "CAU_2013")) %>%
  mutate(pokies = ifelse(is.na(pokies), 0, pokies),
         has_slots = ifelse(pokies == 0, "no pokies", "some pokies"))

ggplot(AU_w._pokies, aes(x=factor(CAU_average_NZDep2013), y=log10(LAND_AREA_),
                   colour=factor(CAU_average_NZDep2013))) + 
  theme_minimal() + ggtitle("Land Area by Deprivation Index") +
  xlab("Deprivation Index of Unit") + geom_quasirandom(size=0.5) + 
  geom_boxplot(alpha=0.5) + facet_wrap(~ has_slots, ncol=1) +
  theme(legend.position = "none") 
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

By eye, that looks like the differences in pokie presence is not just based on the density of AU, but it could do with a more formal statistical test.


