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

## Fler funktioner

Utöver `hamta_fran_kolada()` finns flera hjälpfunktioner för att hitta nyckeltal, läsa metadata, lista kommuner och regioner, identifiera tillgängliga år, hämta senaste värden, beräkna förändringar och göra jämförelser mellan kommuner inom samma län.

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
- `"R"` = region

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

### Jämför en kommun med övriga kommuner i länet

Med `hamta_jamforelse()` kan en vald kommun jämföras med övriga kommuner i samma län.

Som standard hämtas:

- den valda kommunen
- övriga kommuner i samma län
- länets kommungrupp, uttryckt som ovägt medel

Exempel:

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025,
  kon = "T"
)
```

Kommunen kan anges med namn eller kommunkod.

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "2085",
  ar = 2025,
  kon = "T"
)
```

Funktionen identifierar automatiskt vilken länsgrupp kommunen tillhör i Kolada och hämtar samtliga kommuner som ingår i samma grupp.

Resultatet innehåller kolumnen `jamforelsetyp`, som visar vilken typ av observation raden avser:

- `"Vald kommun"` = den kommun som angavs i funktionen
- `"Övrig kommun i länet"` = övriga kommuner i samma län
- `"Länets kommuner"` = länets kommungrupp, ovägt medel

Exempelvis ger:

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025
)
```

värden för Ludvika, övriga kommuner i Dalarnas län samt Dalarnas läns kommuner (ovägt medel).

### Exkludera länets ovägda medel

Om endast den valda kommunen och övriga kommuner i länet ska hämtas kan `inkludera_grupp = FALSE` användas:

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2025,
  inkludera_grupp = FALSE
)
```

Standard är:

```r
inkludera_grupp = TRUE
```

vilket innebär att länets ovägda medel inkluderas.

### Flera år

Flera år kan anges samtidigt:

```r
hamta_jamforelse(
  nyckeltal = "N01926",
  kommun = "Ludvika",
  ar = 2020:2025,
  kon = "T"
)
```

### Flera nyckeltal

Det går även att ange flera nyckeltal:

```r
hamta_jamforelse(
  nyckeltal = c("N01926", "N17454"),
  kommun = "Ludvika",
  ar = 2025,
  kon = "T"
)
```

Observera att `"Länets kommuner"` avser Koladas kommungrupp för länets kommuner och dess ovägda medel. Det är inte samma sak som regionorganisationens eget värde.

Mer information:

```r
?hamta_jamforelse
```

## Exempel på arbetsflöde

Ett vanligt arbetsflöde kan vara att först söka efter ett nyckeltal, läsa dess metadata och därefter hämta data.

```r
# 1. Sök efter ett nyckeltal
sok_nyckeltal("förskola")

# 2. Läs mer om nyckeltalet
info_nyckeltal("N01926")

# 3. Kontrollera vilka år som finns
tillgangliga_ar(
  "N01926",
  kommun = "Haninge"
)

# 4. Hämta data
data <- hamta_fran_kolada(
  nyckeltal = "N01926",
  kommun = "Haninge",
  ar = 2020:2025
)
```

Alternativt kan det senaste publicerade värdet hämtas direkt:

```r
data <- senaste_varde(
  nyckeltal = "N01926",
  kommun = "Haninge"
)
```

## Funktioner i paketet

| Funktion | Beskrivning |
|---|---|
| `hamta_fran_kolada()` | Hämtar data från Kolada för valda nyckeltal, områden och år. |
| `sok_nyckeltal()` | Söker efter nyckeltal utifrån ord, namn, beskrivning eller ID. |
| `info_nyckeltal()` | Hämtar metadata för ett eller flera nyckeltal. |
| `hamta_kommuner()` | Hämtar och söker bland kommuner och regioner. |
| `tillgangliga_ar()` | Visar vilka år som har tillgängliga data för ett nyckeltal. |
| `senaste_varde()` | Hämtar den senaste tillgängliga observationen. |
| `forandring()` | Beräknar absolut och procentuell förändring mellan två år. |
| `hamta_jamforelse()` | Jämför en vald kommun med övriga kommuner i samma län och, valfritt, länets ovägda medel. |

Dokumentation för samtliga funktioner finns även direkt i R:

```r
?hamta_fran_kolada
?sok_nyckeltal
?info_nyckeltal
?hamta_kommuner
?tillgangliga_ar
?senaste_varde
?forandring
?hamta_jamforelse
```
