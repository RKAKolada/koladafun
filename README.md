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

Det enda obligatoriska argumentet är `nyckeltal`.

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926"
)
```

Om kommun och år inte anges hämtas data för alla tillgängliga områden och år.

### Välj kommun med namn eller kommunkod

Kommuner kan anges antingen med kommunkod:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = "0136",
  ar = 2025
)
```

eller med kommunnamn:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = "Haninge",
  ar = 2025
)
```

Det går även att ange flera kommuner samtidigt och att blanda kommunnamn och kommunkoder:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = c("Haninge", "0180", "Mjölby"),
  ar = 2025
)
```

### Välj ett eller flera år

Ett enskilt år kan anges:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = "Haninge",
  ar = 2025
)
```

Flera år kan anges som ett intervall:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = "Haninge",
  ar = 2020:2025
)
```

Om något av de efterfrågade åren saknar data hämtas data för de år som finns tillgängliga och ett meddelande visas om vilka år som saknas.

Om `ar` inte anges hämtas alla tillgängliga år.

### Flera nyckeltal

Flera nyckeltal kan anges samtidigt:

```r
data <- hamta_fran_kolada(
  nyckeltal = c("N01926", "N17454"),
  kommun = "Haninge",
  ar = 2020:2025
)
```

### Filtrera på kön

Det går att filtrera resultatet på kön:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = "Haninge",
  ar = 2025,
  kon = "K"
)
```

`kon` kan anges som:

- `"T"` = total
- `"K"` = kvinnor
- `"M"` = män
- `c("K", "M")` = kvinnor och män

Om `kon` inte anges hämtas alla tillgängliga kön.

### Filtrera på kommuner eller regioner

Argumentet `kommuntyp` kan användas för att begränsa hämtningen till kommuner eller regioner.

För att endast hämta kommuner:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommuntyp = "K",
  ar = 2025
)
```

För att endast hämta regioner:

```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommuntyp = "R",
  ar = 2025
)
```

`kommuntyp` kan anges som:

- `"K"` = kommun
- `"R"` = region

### Kombinera flera val

Argumenten kan kombineras. Exempelvis kan data för flera nyckeltal, kommuner och år hämtas och samtidigt filtreras på kön:

```r
data <- hamta_fran_kolada(
  nyckeltal = c("N01926", "N17454"),
  kommun = c("Haninge", "0180", "Mjölby"),
  ar = 2020:2025,
  kon = c("K", "M")
)
```

## Argument

De viktigaste argumenten i `hamta_fran_kolada()` är:

| Argument | Beskrivning |
|---|---|
| `nyckeltal` | Ett eller flera nyckeltals-ID. Obligatoriskt. |
| `kommun` | Kommun-/regionnamn eller kod. Om det utelämnas hämtas alla områden. |
| `ar` | Ett eller flera år. Om det utelämnas hämtas alla tillgängliga år. |
| `kon` | `"T"`, `"K"` eller `"M"`. Kan även anges som en kombination. |
| `kommuntyp` | `"K"` för kommun eller `"R"` för region. |

Mer information om funktionen finns även i R:

```r
?hamta_fran_kolada
```
