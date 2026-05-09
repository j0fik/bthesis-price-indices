# Bachelor's thesis code for computing various price indices
Simple R script that is used for computing price indices and visualising their behaviour in time.

Thesis can be found [here](https://opac.crzp.sk/?fn=detailBiblioForm&sid=363BD46AAC15F7F453735E6E2E5E).
Code is designed to process [Dominick's data](https://www.chicagobooth.edu/research/kilts/research-data/dominicks) and compute certain price indices: Jevons, Carli, Young-Averaged(newly defined index), Törnqvist and chained-Jevons.

## Packages
You need to install first these packages [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html), [`PriceIndices`](https://cran.r-project.org/web/packages/PriceIndices/index.html), [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html).

## Folder Structure

```text
parent_folder/
├── script.R              (script)
├── data/                 (folder with input data)
│   ├── wana.csv
│   └── wcso.csv
└── graphs/               (folder for graphical outputs)
    ├── ana/              (graphs for analgesics will be saved here)
    └── cso/              (graphs for canned soups will be saved here)
```

### Instructions

The script must be run within an R Project, or you need to set the working directory (`setwd()`) to the folder containing the script (`parent_folder`) in the structure above.

```r
setwd("path/to/parent_folder")
```


## Functions
I'll add this section later.

