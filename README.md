Running server.R and ui.R

### Prerequisites

The R Packages stated in the libraries should be installed before running it.

Please uncomment these lines to install the required packages

```r
# install.packages("RColorBrewer")
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("ggplot2")
# install.packages("sf")
# install.packages('tidyverse')
# install.packages('ggcorrplot')
# install.packages("plotly")
# install.packages("igraph")
# install.packages("ggraph")
# install.packages("scrollrevealR")
```

### Directory
```bash
.
├── README.md
├── app.Rproj
├── data
│   ├── adjacency_high.csv
│   ├── adjacency_low.csv
│   ├── calendar_cleaned.csv
│   ├── choropleth_data.geojson
│   ├── listings_fixed.csv
│   ├── token_high.csv
│   └── token_low.csv
├── server.R
├── ui.R
└── www
    └── app.css
```


### Debug
Occasionally the correlation heat map is not rendered correctly, please refresh
if this situation happens.