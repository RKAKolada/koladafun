# koladafun

Ett R-paket med funktioner för att enkelt hämta data från Kolada.

## Installation

Installera först paketet `remotes` om du inte redan har det:

```r
install.packages("remotes")
```

Installera sedan `koladafun` från GitHub:

```r
remotes::install_github(
  "RKAkolada/koladafun",
  build = FALSE
)
```

## Användning

Ladda paketet:

```r
library(koladafun)
```

### Hämta data från Kolada

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = "0180",
  ar = 2025
)
```

Flera nyckeltal, kommuner och år kan anges samtidigt:

```r
data <- hamta_fran_kolada(
  nyckeltal = c("N01926", "N17454"),
  kommun = c("0180", "0136"),
  ar = 2024:2025
)
```

Det går även att filtrera på kön:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = "0180",
  ar = 2025,
  kon = c("K", "M")
)
```

`kon` kan anges som `"T"` för total, `"K"` för kvinnor och `"M"` för män.
