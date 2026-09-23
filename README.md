# koladafun

Ett R-paket med funktioner för att söka, utforska och hämta data från Kolada.

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
```r
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommuntyp = "L",
  ar = 2025
)
```

`kommuntyp` kan anges som:

- `"K"` = kommun
- `"R"` eller `"L"` = region

Koladas API använder `"L"` för region, men både `"R"` och `"L"` kan användas i `koladafun`.

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
| `kommuntyp` | `"K"` för kommun eller `"R"`/`"L"` för region. |

Mer information om funktionen finns även i R:

```r
?hamta_fran_kolada
```

## Fler funktioner

Utöver `hamta_fran_kolada()` finns flera hjälpfunktioner för att hitta nyckeltal, läsa metadata, lista kommuner och regioner, identifiera tillgängliga år, hämta senaste värden, beräkna förändringar och jämföra kommuner utifrån olika jämförelsegrupper.

### Sök efter nyckeltal

Om du inte känner till ett nyckeltals-ID kan du söka efter nyckeltal med `sok_nyckeltal()`.

```r
sok_nyckeltal("förskola")
```

Det går även att använda flera sökord:

```r
sok_nyckeltal("kostnad förskola")
```

Sökningen görs bland annat i nyckeltalets ID, namn och beskrivning.

Antalet träffar kan begränsas med `max_resultat`:

```r
sok_nyckeltal(
  "förskola",
  max_resultat = 20
)
```

Det går också att söka direkt på ett nyckeltals-ID:

```r
sok_nyckeltal("N01926")
```

Mer information:

```r
?sok_nyckeltal
```

### Visa information om ett nyckeltal

Med `info_nyckeltal()` kan metadata för ett eller flera nyckeltal hämtas.

```r
info_nyckeltal("N01926")
```

Flera nyckeltal kan anges samtidigt:

```r
info_nyckeltal(
  c("N01926", "N17454")
)
```

Funktionen returnerar den metadata som finns tillgänglig för nyckeltalen i Kolada, exempelvis namn och beskrivning.

Mer information:

```r
?info_nyckeltal
```

### Hämta kommuner och regioner

Med `hamta_kommuner()` kan en lista över kommuner och regioner hämtas.

Alla områden:

```r
hamta_kommuner()
```

Endast kommuner:

```r
hamta_kommuner(
  typ = "K"
)
```

Endast regioner:

```r
hamta_kommuner(
  typ = "R"
)
```

Det går även att söka efter ett namn eller en kod:

```r
hamta_kommuner(
  sok = "Han"
)
```

Filtrering och sökning kan kombineras:

```r
hamta_kommuner(
  typ = "K",
  sok = "Han"
)
```

`typ` kan anges som:

- `"K"` = kommun
- `"R"` eller `"L"` = region

Mer information:

```r
?hamta_kommuner
```

### Visa tillgängliga år

Med `tillgangliga_ar()` kan du kontrollera vilka år som har data för ett visst nyckeltal.

```r
tillgangliga_ar("N01926")
```

Det går också att begränsa sökningen till en viss kommun eller region:

```r
tillgangliga_ar(
  "N01926",
  kommun = "Haninge"
)
```

Kommun kan anges med namn eller kod:

```r
tillgangliga_ar(
  "N01926",
  kommun = "0136"
)
```

Det går även att begränsa resultatet till kommuner eller regioner:

```r
tillgangliga_ar(
  "N01926",
  kommuntyp = "K"
)
```

Mer information:

```r
?tillgangliga_ar
```

### Hämta senaste tillgängliga värde

Med `senaste_varde()` kan det senaste tillgängliga värdet för ett nyckeltal hämtas utan att användaren själv behöver veta vilket det senaste publicerade året är.

```r
senaste_varde(
  nyckeltal = "N01926",
  kommun = "Haninge"
)
```

Det går också att filtrera på kön:

```r
senaste_varde(
  nyckeltal = "N01926",
  kommun = "Haninge",
  kon = "T"
)
```

För flera kommuner:

```r
senaste_varde(
  nyckeltal = "N01926",
  kommun = c(
    "Haninge",
    "0180",
    "Mjölby"
  )
)
```

Det går även att hämta senaste värdet för samtliga kommuner:

```r
senaste_varde(
  nyckeltal = "N01926",
  kommuntyp = "K"
)
```

Det senaste året bestäms separat för varje kombination av nyckeltal, område och kön.

Som standard används den senaste observationen där ett faktiskt värde finns. Om den senaste observationen är bortfall, sekretess eller av annan anledning saknar värde används istället det senaste året där ett värde finns.

Om även observationer med bortfall eller sekretess ska accepteras kan `bortfall = TRUE` anges:

```r
senaste_varde(
  nyckeltal = "N01926",
  kommun = "Haninge",
  bortfall = TRUE
)
```

Med `bortfall = TRUE` returneras den senaste observationen även om `value` saknas. Information om exempelvis bortfall eller sekretess finns då kvar i kolumnen `status`.

Mer information:

```r
?senaste_varde
```

### Beräkna förändring mellan två år

Med `forandring()` kan förändringen för ett eller flera nyckeltal mellan två år beräknas. Funktionen hämtar värdena för de två valda åren och beräknar både absolut och procentuell förändring.

Exempel för en kommun:

```r
forandring(
  nyckeltal = "N01926",
  kommun = "Haninge",
  fran = 2020,
  till = 2025,
  kon = "T"
)
```

Resultatet innehåller bland annat:

- värdet för startåret
- värdet för slutåret
- absolut förändring
- procentuell förändring

Funktionen kan även användas för flera kommuner och nyckeltal.

Exempel för flera kommuner:

```r
forandring(
  nyckeltal = "N01926",
  kommun = c("Haninge", "Huddinge", "0180"),
  fran = 2020,
  till = 2025,
  kon = "T"
)
```

Det går också att beräkna förändringen för samtliga kommuner:

```r
forandring(
  nyckeltal = "N01926",
  kommuntyp = "K",
  fran = 2020,
  till = 2025,
  kon = "T"
)
```

Om ett värde saknas för något av jämförelseåren kan förändringen inte beräknas för den observationen. Procentuell förändring beräknas inte heller när startvärdet är 0.

Mer information:

```r
?forandring
```

### Jämför en kommun med andra kommuner

Med `hamta_jamforelse()` kan en vald kommun jämföras med andra kommuner utifrån olika jämförelsegrupper i Kolada.

Det går att välja mellan:

- kommuner i samma län
- SKR:s kommungruppsindelning
- liknande kommuner inom olika verksamhetsområden

Som standard används kommunerna i samma län.

### Jämför med kommunerna i samma län

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025,
  kon = "T",
  jamforelse = "lan"
)
```

Funktionen identifierar automatiskt vilket län kommunen tillhör och hämtar:

- den valda kommunen
- övriga kommuner i samma län
- länets kommungrupp (ovägt medel)

Eftersom `"lan"` är standard kan samma anrop även skrivas:

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025,
  kon = "T"
)
```

### Jämför med SKR:s kommungrupp

Med `jamforelse = "kommungrupp"` jämförs kommunen istället med kommunerna i samma kommungrupp enligt SKR:s kommungruppsindelning.

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025,
  kon = "T",
  jamforelse = "kommungrupp"
)
```

Funktionen identifierar automatiskt vilken kommungrupp den valda kommunen tillhör och hämtar den valda kommunen, övriga kommuner i kommungruppen samt gruppvärdet.

### Jämför med liknande kommuner

Med `jamforelse = "liknande"` kan kommunen jämföras med liknande kommuner inom olika verksamhetsområden.

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025,
  kon = "T",
  jamforelse = "liknande"
)
```

När funktionen körs visas de tillgängliga grupperna för den valda kommunen i Console, exempelvis:

```text
Välj grupp för liknande kommuner för Ludvika:

1: arbetsmarknad (2024)
2: ekonomiskt bistånd (2023)
3: fritidshem (2024)
4: förskola (2024)
5: grundskola (2024)
6: gymnasieskola (2024)
7: IFO (2024)
8: LSS (2024)
9: räddningstjänst (2024)
10: socioekonomi (2024)
11: äldreomsorg (2024)
12: övergripande (2025)

Selection:
```

Ange numret för den jämförelsegrupp som ska användas. Om exempelvis `4` anges används gruppen för liknande kommuner inom förskola.

De grupper som visas hämtas från Kolada och kan därför skilja sig mellan kommuner och förändras när gruppindelningarna uppdateras.

Det går också att ange verksamheten direkt och därmed hoppa över valet i Console:

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025,
  kon = "T",
  jamforelse = "liknande",
  verksamhet = "förskola"
)
```

Detta är särskilt användbart när funktionen används i ett script eller annat automatiserat arbetsflöde.

### Inkludera eller exkludera gruppvärdet

Som standard är:

```r
inkludera_grupp = TRUE
```

vilket innebär att jämförelsegruppens gruppvärde inkluderas tillsammans med de enskilda kommunerna.

Om endast kommunernas egna värden ska hämtas används:

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025,
  jamforelse = "lan",
  inkludera_grupp = FALSE
)
```

### Resultatet

Resultatet innehåller bland annat kolumnerna `jamforelsetyp` och `jamforelsegrupp`.

`jamforelsetyp` visar om observationen avser den valda kommunen, en annan kommun i jämförelsegruppen eller gruppvärdet.

`jamforelsegrupp` visar vilken grupp i Kolada som har använts för jämförelsen.

Flera år och flera nyckeltal kan anges på samma sätt som i `hamta_fran_kolada()`:

```r
hamta_jamforelse(
  nyckeltal = c("N01926", "N17454"),
  kommun = "Ludvika",
  ar = 2020:2025,
  kon = "T",
  jamforelse = "lan"
)
```

Observera att gruppvärden avser Koladas jämförelsegrupper och inte regionorganisationens eget värde.

Mer information:

```r
?hamta_jamforelse
```

## Exempel på arbetsflöde

Ett vanligt arbetsflöde kan vara att först söka efter ett nyckeltal, läsa dess metadata och därefter hämta data.

```r
# 1. Sök efter ett nyckeltal
sok_nyckeltal("kostnad förskola")

# 2. Läs mer om nyckeltalet
info_nyckeltal("N11005")

# 3. Kontrollera vilka år som finns
tillgangliga_ar(
  "N11005",
  kommun = "Haninge"
)

# 4. Hämta data
data <- hamta_fran_kolada(
  nyckeltal = "N11005",
  kommun = "Haninge",
  ar = 2020:2025
)
```

Alternativt kan det senaste publicerade värdet hämtas direkt:

```r
data <- senaste_varde(
  nyckeltal = "N11005",
  kommun = "Haninge"
)
```

## Funktioner i paketet

Alla huvudfunktioner finns med både svenska och engelska funktionsnamn.

| Svenska | English | Beskrivning |
|---|---|---|
| `hamta_fran_kolada()` | `get_from_kolada()` | Hämtar data från Kolada för valda nyckeltal, områden och år. |
| `sok_nyckeltal()` | `search_kpi()` | Söker efter nyckeltal utifrån ord, namn, beskrivning eller ID. |
| `info_nyckeltal()` | `kpi_info()` | Hämtar metadata för ett eller flera nyckeltal. |
| `hamta_kommuner()` | `get_municipalities()` | Hämtar och söker bland kommuner och regioner. |
| `tillgangliga_ar()` | `available_years()` | Visar vilka år som har tillgängliga data för ett nyckeltal. |
| `senaste_varde()` | `latest_value()` | Hämtar den senaste tillgängliga observationen. |
| `forandring()` | `change()` | Beräknar absolut och procentuell förändring mellan två år. |
| `hamta_jamforelse()` | `get_comparison()` | Jämför en kommun med kommuner i samma län, SKR-kommungrupp eller grupper av liknande kommuner. |

Dokumentation för de svenska funktionerna finns även direkt i R:

```r
# Svenska
?hamta_fran_kolada
?sok_nyckeltal
?info_nyckeltal
?hamta_kommuner
?tillgangliga_ar
?senaste_varde
?forandring
?hamta_jamforelse
```

## English users

`koladafun` can be used with both Swedish and English function names.

The English functions provide the same functionality as the Swedish functions, but use English function names and argument names.

| Swedish function | English function |
|---|---|
| `hamta_fran_kolada()` | `get_from_kolada()` |
| `sok_nyckeltal()` | `search_kpi()` |
| `info_nyckeltal()` | `kpi_info()` |
| `hamta_kommuner()` | `get_municipalities()` |
| `tillgangliga_ar()` | `available_years()` |
| `senaste_varde()` | `latest_value()` |
| `forandring()` | `change()` |
| `hamta_jamforelse()` | `get_comparison()` |

### Get data from Kolada

The only required argument is `kpi`.

```r
data <- get_from_kolada(
  kpi = "N01926"
)
```

If no municipality or year is specified, data for all available areas and years are returned.

A municipality can be specified by name or code:

```r
data <- get_from_kolada(
  kpi = "N01926",
  municipality = "Haninge",
  year = 2025
)
```

Multiple municipalities, years and KPIs can also be requested:

```r
data <- get_from_kolada(
  kpi = c("N01926", "N17454"),
  municipality = c("Haninge", "0180", "Mjölby"),
  year = 2020:2025,
  gender = c("K", "M")
)
```

`gender` can be:

- `"T"` = total
- `"K"` = women
- `"M"` = men
- `c("K", "M")` = women and men

The `municipality_type` argument can be used to limit the request to municipalities or regions:

- `"K"` = municipality
- `"R"` or `"L"` = region

Kolada's API uses `"L"` for regions, but both `"R"` and `"L"` are accepted by `koladafun`.

More information:

```r
?get_from_kolada
```

### Search for KPIs

Use `search_kpi()` to search for KPIs in Kolada:

```r
search_kpi("preschool")
```

The maximum number of results can be specified with `max_results`:

```r
search_kpi(
  "preschool",
  max_results = 20
)
```

More information:

```r
?search_kpi
```

### Get KPI information

Use `kpi_info()` to retrieve metadata for one or more KPIs:

```r
kpi_info("N01926")
```

Multiple KPIs can be specified:

```r
kpi_info(
  c("N01926", "N17454")
)
```

More information:

```r
?kpi_info
```

### Get municipalities and regions

Use `get_municipalities()` to retrieve municipalities and regions:

```r
get_municipalities()
```

Only municipalities:

```r
get_municipalities(
  type = "K"
)
```

Only regions:

```r
get_municipalities(
  type = "R"
)
```

Both `"R"` and `"L"` can be used to select regions.

It is also possible to search by name or code:

```r
get_municipalities(
  search = "Han"
)
```

More information:

```r
?get_municipalities
```

### Get available years

Use `available_years()` to check which years contain data for a KPI:

```r
available_years(
  kpi = "N01926"
)
```

The search can be limited to a municipality or region:

```r
available_years(
  kpi = "N01926",
  municipality = "Haninge"
)
```

More information:

```r
?available_years
```

### Get the latest available value

Use `latest_value()` to retrieve the latest available value without having to know the latest published year:

```r
latest_value(
  kpi = "N01926",
  municipality = "Haninge"
)
```

By default, the latest observation containing an actual value is returned.

If observations with missing values, for example due to missing data or confidentiality, should also be accepted, use:

```r
latest_value(
  kpi = "N01926",
  municipality = "Haninge",
  include_missing = TRUE
)
```

The latest year is determined separately for each combination of KPI, area and gender.

More information:

```r
?latest_value
```

### Calculate change between two years

Use `change()` to calculate absolute and percentage change between two years:

```r
change(
  kpi = "N01926",
  municipality = "Haninge",
  start_year = 2020,
  end_year = 2025,
  gender = "T"
)
```

The result contains:

- `start_value`
- `end_value`
- `change`
- `percentage_change`

If a value is missing for either comparison year, the change cannot be calculated. Percentage change is not calculated when the start value is 0.

More information:

```r
?change
```

### Compare a municipality with other municipalities

Use `get_comparison()` to compare a municipality with different comparison groups in Kolada.

Three types of comparisons are available:

- `"county"` = municipalities in the same county
- `"municipality_group"` = municipalities in the same SKR municipality group
- `"similar"` = similar municipalities

The default is `"county"`.

#### Compare with municipalities in the same county

```r
get_comparison(
  kpi = "N01926",
  municipality = "Ludvika",
  year = 2025,
  gender = "T",
  comparison = "county"
)
```

#### Compare with an SKR municipality group

```r
get_comparison(
  kpi = "N01926",
  municipality = "Ludvika",
  year = 2025,
  gender = "T",
  comparison = "municipality_group"
)
```

#### Compare with similar municipalities

```r
get_comparison(
  kpi = "N01926",
  municipality = "Ludvika",
  year = 2025,
  gender = "T",
  comparison = "similar"
)
```

When `comparison = "similar"` and `area` is not specified, an interactive menu is shown with the available comparison groups for the selected municipality.

A service area can also be specified directly:

```r
get_comparison(
  kpi = "N01926",
  municipality = "Ludvika",
  year = 2025,
  gender = "T",
  comparison = "similar",
  area = "preschool"
)
```

Common values for `area` include:

| `area` | Comparison group |
|---|---|
| `"labour_market"` | Labour market |
| `"economic_assistance"` | Economic assistance |
| `"leisure_time_centre"` | Leisure-time centre |
| `"preschool"` | Preschool |
| `"compulsory_school"` | Compulsory school |
| `"upper_secondary_school"` | Upper secondary school |
| `"individual_and_family_care"` | Individual and family care (IFO) |
| `"lss"` | LSS |
| `"rescue_service"` | Rescue service |
| `"socioeconomic"` | Socioeconomic |
| `"elderly_care"` | Elderly care |
| `"overall"` | Overall |

By default, `include_group = TRUE`. Set `include_group = FALSE` to return only the individual municipalities.

The result includes `comparison_type` and `comparison_group`.

More information:

```r
?get_comparison
```

## Functions

| Function | Description |
|---|---|
| `get_from_kolada()` | Retrieves data from Kolada for selected KPIs, areas and years. |
| `search_kpi()` | Searches for KPIs by keyword, name, description or ID. |
| `kpi_info()` | Retrieves metadata for one or more KPIs. |
| `get_municipalities()` | Retrieves and searches municipalities and regions. |
| `available_years()` | Shows which years have available data for a KPI. |
| `latest_value()` | Retrieves the latest available observation. |
| `change()` | Calculates absolute and percentage change between two years. |
| `get_comparison()` | Compares a municipality with municipalities in the same county, SKR municipality group or groups of similar municipalities. |

Documentation for the English functions is also available directly in R:

```r
# English
?get_from_kolada
?search_kpi
?kpi_info
?get_municipalities
?available_years
?latest_value
?change
?get_comparison
```

