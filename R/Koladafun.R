# =========================================================
# Intern cache och säkra API-anrop
# =========================================================

# Cache gäller under den aktuella R-sessionen.
# Själva nyckeltalsvärdena cachelagras inte, endast metadata.
.kolada_cache <- new.env(parent = emptyenv())


.cache_get <- function(namn) {

  if (exists(
    namn,
    envir = .kolada_cache,
    inherits = FALSE
  )) {
    return(
      get(
        namn,
        envir = .kolada_cache,
        inherits = FALSE
      )
    )
  }

  NULL
}


.cache_set <- function(
    namn,
    varde
) {

  assign(
    namn,
    varde,
    envir = .kolada_cache
  )

  invisible(varde)
}


# Säkert API-anrop med retry.
# Väntetiden ökar mellan försöken: 1, 2, 3 sekunder som standard.
.kolada_fromJSON <- function(
    url,
    retries = 3,
    wait = 1
) {

  senaste_fel <- NULL

  for (forsok in seq_len(retries)) {

    resultat <- tryCatch(
      jsonlite::fromJSON(url),
      error = function(e) {
        senaste_fel <<- e
        NULL
      }
    )

    if (!is.null(resultat)) {
      return(resultat)
    }

    if (forsok < retries) {
      Sys.sleep(
        wait * forsok
      )
    }
  }

  stop(
    "Kunde inte hämta data från Koladas API efter ",
    retries,
    " försök.\n",
    "URL: ",
    url,
    if (!is.null(senaste_fel)) {
      paste0(
        "\nFel: ",
        conditionMessage(senaste_fel)
      )
    } else {
      ""
    },
    call. = FALSE
  )
}


# Metadata för kommuner och regioner.
.hamta_kommunmetadata <- function() {

  cache <- .cache_get(
    "kommunmetadata"
  )

  if (!is.null(cache)) {
    return(cache)
  }

  svar <- .kolada_fromJSON(
    "https://api.kolada.se/v3/municipality?page=1&per_page=5000"
  )

  kommuner <- as.data.frame(
    svar$values
  )

  kommuner$id <- as.character(
    kommuner$id
  )

  kommuner$title <- as.character(
    kommuner$title
  )

  kommuner$type <- as.character(
    kommuner$type
  )

  .cache_set(
    "kommunmetadata",
    kommuner
  )

  kommuner
}


# Metadata för ett enskilt nyckeltal.
.hamta_kpi_metadata <- function(kpi_id) {

  kpi_id <- as.character(kpi_id)

  cache_namn <- paste0(
    "kpi_",
    kpi_id
  )

  cache <- .cache_get(
    cache_namn
  )

  if (!is.null(cache)) {
    return(cache)
  }

  svar <- .kolada_fromJSON(
    paste0(
      "https://api.kolada.se/v3/kpi/",
      kpi_id
    )
  )

  if (
    is.null(svar$values) ||
    nrow(as.data.frame(svar$values)) == 0
  ) {
    stop(
      "Nyckeltalet ",
      kpi_id,
      " kunde inte hittas."
    )
  }

  resultat <- as.data.frame(
    svar$values
  )

  .cache_set(
    cache_namn,
    resultat
  )

  resultat
}


# Kommungrupper används bland annat av hamta_jamforelse().
.hamta_kommungrupper <- function() {

  cache <- .cache_get(
    "kommungrupper"
  )

  if (!is.null(cache)) {
    return(cache)
  }

  svar <- .kolada_fromJSON(
    "https://api.kolada.se/v3/municipality_groups/?page=1&per_page=5000"
  )

  grupper <- svar$values |>
    tidyr::unnest(members)

  grupper$id <- as.character(
    grupper$id
  )

  grupper$member_id <- as.character(
    grupper$member_id
  )

  grupper$title <- as.character(
    grupper$title
  )

  .cache_set(
    "kommungrupper",
    grupper
  )

  grupper
}


#' Hämta data från Kolada
#'
#' Hämtar data från Koladas API för ett eller flera nyckeltal.
#' Kommun/region, år, kön och områdestyp kan filtreras valfritt.
#'
#' Om kommun inte anges hämtas samtliga tillgängliga områden.
#' Om år inte anges hämtas samtliga tillgängliga år.
#'
#' Kommun kan anges antingen med kommunkod eller kommunnamn,
#' exempelvis `"0136"` eller `"Haninge"`.
#'
#' @param nyckeltal Ett eller flera nyckeltals-ID från Kolada,
#'   exempelvis `"N01926"` eller `c("N01926", "N17454")`.
#'
#' @param kommun Valfritt. En eller flera kommunkoder eller kommun-/regionnamn,
#'   exempelvis `"0136"`, `"Haninge"` eller
#'   `c("Haninge", "0180")`.
#'   Om NULL hämtas samtliga områden.
#'
#' @param ar Valfritt. Ett eller flera år,
#'   exempelvis `2025` eller `2020:2025`.
#'   Om NULL hämtas samtliga tillgängliga år.
#'
#' @param kon Valfritt filter för kön.
#'   `"T"` = total, `"K"` = kvinnor och `"M"` = män.
#'   Flera värden kan anges, exempelvis `c("K", "M")`.
#'   Om NULL returneras alla tillgängliga kön.
#'
#' @param kommuntyp Valfritt filter för områdestyp.
#'   `"K"` = kommun och `"R"` eller `"L"` = region.
#'   Koladas API använder `"L"` för region, men både `"R"` och `"L"`
#'   accepteras av funktionen.
#'   Flera kan anges med exempelvis `c("K", "R")`.
#'
#' @param per_page Antal observationer per sida i API-anropet.
#'   Standard är 5000.
#'
#' @param batch_size Antal kommuner/regioner som hämtas per API-anrop.
#'   Standard är 50.
#'
#' @return En data.frame med områdeskod, områdesnamn,
#'   nyckeltals-ID, nyckeltalsnamn, år, värde, kön och status.
#'
#' @examples
#' \dontrun{
#'
#' hamta_fran_kolada(
#'   nyckeltal = "N01926"
#' )
#'
#' hamta_fran_kolada(
#'   nyckeltal = "N01926",
#'   kommun = "Haninge"
#' )
#'
#' hamta_fran_kolada(
#'   nyckeltal = "N01926",
#'   kommuntyp = "K"
#' )
#'
#' hamta_fran_kolada(
#'   nyckeltal = "N01926",
#'   kommun = "Haninge",
#'   ar = 2020:2025,
#'   kon = "K"
#' )
#'
#' }
#'
#' @export
hamta_fran_kolada <- function(
    nyckeltal,
    kommun = NULL,
    ar = NULL,
    kon = NULL,
    kommuntyp = NULL,
    per_page = 5000,
    batch_size = 25
) {

  if (missing(nyckeltal) || length(nyckeltal) == 0) {
    stop("Du måste ange minst ett nyckeltal.")
  }

  nyckeltal <- as.character(nyckeltal)

  # Hämta metadata för kommuner och regioner
  # Metadata cachelagras under den aktuella R-sessionen.
  kommuner <- .hamta_kommunmetadata()

  # Filtrera på kommuntyp
  if (!is.null(kommuntyp)) {

    kommuntyp <- toupper(as.character(kommuntyp))

    ogiltiga_typer <- setdiff(
      kommuntyp,
      c("K", "R", "L")
    )

    if (length(ogiltiga_typer) > 0) {
      stop(
        "Ogiltig kommuntyp: ",
        paste(ogiltiga_typer, collapse = ", "),
        ". Använd K för kommun eller R/L för region."
      )
    }

    # Koladas API använder L för region.
    # Både R och L accepteras därför som region av användaren.
    api_typ <- ifelse(
      kommuntyp %in% c("R", "L"),
      "L",
      kommuntyp
    )

    kommuner <- kommuner[
      kommuner$type %in% unique(api_typ),
      ,
      drop = FALSE
    ]
  }

  # Matcha kommunnamn eller kommunkod
  if (!is.null(kommun)) {

    kommun <- as.character(kommun)

    valda_id <- character(0)

    for (x in kommun) {

      traff_kod <- kommuner[
        kommuner$id == x,
        ,
        drop = FALSE
      ]

      if (nrow(traff_kod) > 0) {

        valda_id <- c(
          valda_id,
          traff_kod$id
        )

        next
      }

      traff_namn <- kommuner[
        tolower(kommuner$title) == tolower(x),
        ,
        drop = FALSE
      ]

      if (nrow(traff_namn) == 0) {
        stop(
          "Hittar ingen kommun eller region med namn/kod: ",
          x
        )
      }

      valda_id <- c(
        valda_id,
        traff_namn$id
      )
    }

    kommuner <- kommuner[
      kommuner$id %in% unique(valda_id),
      ,
      drop = FALSE
    ]
  }

  # Kontrollera kön tidigt
  if (!is.null(kon)) {

    kon <- toupper(as.character(kon))

    giltiga_kon <- c(
      "T",
      "K",
      "M"
    )

    ogiltiga_kon <- setdiff(
      kon,
      giltiga_kon
    )

    if (length(ogiltiga_kon) > 0) {
      stop(
        "Ogiltigt värde för kön: ",
        paste(ogiltiga_kon, collapse = ", "),
        ". Använd T, K eller M."
      )
    }
  }

  # Hjälpfunktion för pagination
  hamta_sidor <- function(url) {

    alla <- list()
    sida <- 1

    repeat {

      separator <- if (grepl("\\?", url)) "&" else "?"

      aktuell_url <- paste0(
        url,
        separator,
        "page=",
        sida,
        "&per_page=",
        per_page
      )

      svar <- .kolada_fromJSON(
        aktuell_url
      )

      if (
        is.null(svar$values) ||
        length(svar$values) == 0
      ) {
        break
      }

      alla[[sida]] <- svar$values

      if (nrow(as.data.frame(svar$values)) < per_page) {
        break
      }

      sida <- sida + 1
    }

    alla
  }

  # Hämta KPI-namn
  kpi_metadata <- lapply(
    nyckeltal,
    function(kpi_id) {

      metadata <- .hamta_kpi_metadata(
        kpi_id
      )

      data.frame(
        kpi = kpi_id,
        kpi_name = metadata$title[1],
        stringsAsFactors = FALSE
      )
    }
  )

  kpi_metadata <- dplyr::bind_rows(
    kpi_metadata
  )

  # Dela områden i batchar
  kommun_batchar <- split(
    kommuner$id,
    ceiling(
      seq_along(kommuner$id) / batch_size
    )
  )

  # Dela år i mindre batchar om användaren angett år
  if (is.null(ar)) {

    ar_batchar <- list(NULL)

  } else {

    ar <- as.character(ar)

    ar_batchar <- split(
      ar,
      ceiling(
        seq_along(ar) / 20
      )
    )
  }

  # Hämta data
  resultat <- list()
  index <- 1

  for (kpi_id in nyckeltal) {

    for (kommun_batch in kommun_batchar) {

      kommun_del <- paste(
        kommun_batch,
        collapse = ","
      )

      for (ar_batch in ar_batchar) {

        url <- paste0(
          "https://api.kolada.se/v3/data/kpi/",
          kpi_id,
          "/municipality/",
          kommun_del
        )

        if (!is.null(ar_batch)) {

          url <- paste0(
            url,
            "/year/",
            paste(
              ar_batch,
              collapse = ","
            )
          )
        }

        svar_lista <- tryCatch(
          hamta_sidor(url),
          error = function(e) {
            NULL
          }
        )

        if (
          is.null(svar_lista) ||
          length(svar_lista) == 0
        ) {
          next
        }

        data_lista <- lapply(
          svar_lista,
          function(x) {

            data.frame(x) |>
              tidyr::unnest(values)

          }
        )

        resultat[[index]] <- dplyr::bind_rows(
          data_lista
        )

        index <- index + 1
      }
    }
  }

  # Slå ihop resultat
  data <- dplyr::bind_rows(
    resultat
  )

  if (nrow(data) == 0) {

    message(
      "Ingen data hittades för de valda kriterierna."
    )

    return(data.frame())
  }

  # Kontrollera vilka efterfrågade år som faktiskt finns
  if (!is.null(ar)) {

    begarda_ar <- unique(
      as.character(ar)
    )

    tillgangliga_ar <- unique(
      as.character(data$period)
    )

    saknade_ar <- setdiff(
      begarda_ar,
      tillgangliga_ar
    )

    if (length(saknade_ar) > 0) {

      message(
        "Data saknas för följande år: ",
        paste(
          saknade_ar,
          collapse = ", "
        ),
        ". Tillgängliga år har hämtats."
      )
    }
  }

  # Lägg till kommunnamn
  data <- data |>
    dplyr::left_join(
      kommuner |>
        dplyr::select(
          municipality = id,
          municipality_name = title
        ),
      by = "municipality"
    )

  # Lägg till KPI-namn
  data <- data |>
    dplyr::left_join(
      kpi_metadata,
      by = "kpi"
    )

  # Filtrera kön
  if (!is.null(kon)) {

    data <- data |>
      dplyr::filter(
        gender %in% kon
      )
  }

  # Välj och sortera kolumner
  data <- data |>
    dplyr::select(
      municipality,
      municipality_name,
      kpi,
      kpi_name,
      period,
      value,
      gender,
      status
    ) |>
    dplyr::arrange(
      kpi,
      municipality,
      period,
      gender
    )

  rownames(data) <- NULL

  return(
    as.data.frame(data)
  )
}

# Intern hjälpfunktion för att hämta alla sidor från Koladas API
.hamta_alla_sidor <- function(url, per_page = 5000) {

  resultat <- list()
  sida <- 1

  repeat {

    separator <- if (grepl("\\?", url)) "&" else "?"

    aktuell_url <- paste0(
      url,
      separator,
      "page=",
      sida,
      "&per_page=",
      per_page
    )

    svar <- .kolada_fromJSON(aktuell_url)

    if (
      is.null(svar$values) ||
      length(svar$values) == 0
    ) {
      break
    }

    resultat[[sida]] <- as.data.frame(
      svar$values
    )

    if (
      nrow(as.data.frame(svar$values)) < per_page
    ) {
      break
    }

    sida <- sida + 1
  }

  dplyr::bind_rows(resultat)
}

#' Sök efter nyckeltal i Kolada
#'
#' Söker bland Koladas nyckeltal efter ett eller flera sökord.
#' Sökningen görs i nyckeltals-ID, namn och beskrivning.
#'
#' Om flera sökord anges måste samtliga sökord förekomma
#' i nyckeltalets metadata.
#'
#' @param sok Ett eller flera sökord, exempelvis `"förskola"`
#'   eller `"kostnad förskola"`.
#' @param max_resultat Maximalt antal träffar som returneras.
#'   Standard är 50.
#'
#' @return En data.frame med matchande nyckeltal.
#'
#' @examples
#' \dontrun{
#' sok_nyckeltal("förskola")
#' sok_nyckeltal("kostnad förskola")
#' sok_nyckeltal("N01926")
#' }
#'
#' @export
sok_nyckeltal <- function(
    sok,
    max_resultat = 50
) {

  if (
    missing(sok) ||
    length(sok) == 0 ||
    !nzchar(trimws(sok))
  ) {
    stop("Du måste ange ett sökord.")
  }

  nyckeltal <- .cache_get(
    "alla_nyckeltal"
  )

  if (is.null(nyckeltal)) {

    nyckeltal <- .hamta_alla_sidor(
      "https://api.kolada.se/v3/kpi"
    )

    .cache_set(
      "alla_nyckeltal",
      nyckeltal
    )
  }

  sokkolumner <- intersect(
    c(
      "id",
      "title",
      "description"
    ),
    names(nyckeltal)
  )

  soktext <- apply(
    nyckeltal[, sokkolumner, drop = FALSE],
    1,
    function(x) {
      paste(
        ifelse(is.na(x), "", x),
        collapse = " "
      )
    }
  )

  soktext <- tolower(soktext)

  sokord <- strsplit(
    tolower(trimws(sok)),
    "\\s+"
  )[[1]]

  traff <- Reduce(
    `&`,
    lapply(
      sokord,
      function(ord) {
        grepl(
          ord,
          soktext,
          fixed = TRUE
        )
      }
    )
  )

  resultat <- nyckeltal[
    traff,
    ,
    drop = FALSE
  ]

  # Lägg de viktigaste kolumnerna först
  forst <- intersect(
    c(
      "id",
      "title",
      "description"
    ),
    names(resultat)
  )

  resultat <- resultat[
    c(
      forst,
      setdiff(names(resultat), forst)
    )
  ]

  resultat <- head(
    resultat,
    max_resultat
  )

  rownames(resultat) <- NULL

  resultat
}

#' Visa information om nyckeltal
#'
#' Hämtar metadata för ett eller flera nyckeltal från Kolada.
#'
#' @param nyckeltal Ett eller flera nyckeltals-ID,
#'   exempelvis `"N01926"` eller
#'   `c("N01926", "N17454")`.
#'
#' @return En data.frame med metadata om nyckeltalen.
#'
#' @examples
#' \dontrun{
#' info_nyckeltal("N01926")
#'
#' info_nyckeltal(
#'   c("N01926", "N17454")
#' )
#' }
#'
#' @export
info_nyckeltal <- function(nyckeltal) {

  if (
    missing(nyckeltal) ||
    length(nyckeltal) == 0
  ) {
    stop("Du måste ange minst ett nyckeltal.")
  }

  nyckeltal <- as.character(
    nyckeltal
  )

  resultat <- lapply(
    nyckeltal,
    function(kpi_id) {

      .hamta_kpi_metadata(
        kpi_id
      )
    }
  )

  resultat <- dplyr::bind_rows(
    resultat
  )

  # Lägg viktig information först
  forst <- intersect(
    c(
      "id",
      "title",
      "description"
    ),
    names(resultat)
  )

  resultat <- resultat[
    c(
      forst,
      setdiff(names(resultat), forst)
    )
  ]

  rownames(resultat) <- NULL

  resultat
}

#' Hämta kommuner och regioner från Kolada
#'
#' Hämtar en lista över kommuner och regioner från Kolada.
#' Resultatet kan filtreras på områdestyp eller namn/kod.
#'
#' @param typ Valfri områdestyp.
#'   `"K"` = kommun och `"R"` eller `"L"` = region.
#' @param sok Valfri söktext för namn eller kod.
#'
#' @return En data.frame med områdeskod, namn och typ.
#'
#' @examples
#' \dontrun{
#' hamta_kommuner()
#' hamta_kommuner(typ = "K")
#' hamta_kommuner(typ = "R")
#' hamta_kommuner(sok = "Han")
#' }
#'
#' @export
hamta_kommuner <- function(
    typ = NULL,
    sok = NULL
) {

  kommuner <- .hamta_kommunmetadata()

  # Gör regiontypen enklare för användaren
  kommuner$type[
    kommuner$type == "L"
  ] <- "R"

  if (!is.null(typ)) {

    typ <- toupper(
      as.character(typ)
    )

    ogiltiga <- setdiff(
      typ,
      c("K", "R", "L")
    )


    if (length(ogiltiga) > 0) {
      stop(
        "Ogiltig typ: ",
        paste(
          ogiltiga,
          collapse = ", "
        ),
        ". Använd K för kommun eller R/L för region."
      )
    }

    # Både R och L betyder region
    typ[typ == "L"] <- "R"

    kommuner <- kommuner[
      kommuner$type %in% typ,
      ,
      drop = FALSE
    ]
  }

  if (!is.null(sok)) {

    sok <- tolower(
      as.character(sok)
    )

    traff <- grepl(
      sok,
      tolower(kommuner$title),
      fixed = TRUE
    ) |
      grepl(
        sok,
        tolower(kommuner$id),
        fixed = TRUE
      )

    kommuner <- kommuner[
      traff,
      ,
      drop = FALSE
    ]
  }

  kommuner <- kommuner |>
    dplyr::select(
      municipality = id,
      municipality_name = title,
      type
    ) |>
    dplyr::arrange(
      type,
      municipality_name
    )

  rownames(kommuner) <- NULL

  as.data.frame(kommuner)
}

#' Visa tillgängliga år för nyckeltal
#'
#' Hämtar vilka år som har data för ett eller flera
#' nyckeltal i Kolada.
#'
#' @param nyckeltal Ett eller flera nyckeltals-ID.
#' @param kommun Valfri kommun eller region, angiven som
#'   namn eller kod.
#' @param kommuntyp Valfritt filter för områdestyp.
#'   `"K"` = kommun och `"R"` eller `"L"` = region.
#'   Koladas API använder `"L"` för region, men både `"R"` och `"L"`
#'   accepteras av funktionen.
#'   Flera kan anges med exempelvis `c("K", "R")`.
#'
#' @return En data.frame med nyckeltals-ID och tillgängliga år.
#'
#' @examples
#' \dontrun{
#' tillgangliga_ar("N01926")
#'
#' tillgangliga_ar(
#'   "N01926",
#'   kommun = "Haninge"
#' )
#' }
#'
#' @export
tillgangliga_ar <- function(
    nyckeltal,
    kommun = NULL,
    kommuntyp = NULL
) {

  data <- hamta_fran_kolada(
    nyckeltal = nyckeltal,
    kommun = kommun,
    kommuntyp = kommuntyp
  )

  if (nrow(data) == 0) {
    return(
      data.frame()
    )
  }

  resultat <- data |>
    dplyr::distinct(
      kpi,
      period
    ) |>
    dplyr::arrange(
      kpi,
      period
    )

  rownames(resultat) <- NULL

  as.data.frame(resultat)
}

#' Hämta senaste tillgängliga värde
#'
#' Hämtar det senast tillgängliga värdet för ett eller flera
#' nyckeltal och områden.
#'
#' Det senaste året bestäms separat för varje kombination av
#' nyckeltal, område och kön.
#'
#' @param nyckeltal Ett eller flera nyckeltals-ID.
#' @param kommun Valfri kommun eller region.
#' @param kon Valfritt kön: `"T"`, `"K"` eller `"M"`.
#' @param kommuntyp Valfri områdestyp:
#'   `"K"` = kommun och `"R"` eller `"L"` = region.
#' @param bortfall Logiskt värde. Om `FALSE` hämtas senaste
#'   observationen där ett faktiskt värde finns. Om `TRUE`
#'   accepteras även observationer där värdet saknas, exempelvis
#'   på grund av bortfall eller sekretess. Standard är `FALSE`.
#'
#' @return En data.frame med senaste tillgängliga observation.
#'
#' @examples
#' \dontrun{
#' senaste_varde(
#'   nyckeltal = "N01926",
#'   kommun = "Haninge"
#' )
#'
#' senaste_varde(
#'   nyckeltal = "N01926",
#'   kommun = "Haninge",
#'   bortfall = TRUE
#' )
#' }
#'
#' @export
senaste_varde <- function(
    nyckeltal,
    kommun = NULL,
    kon = NULL,
    kommuntyp = NULL,
    bortfall = FALSE
) {

  if (
    length(bortfall) != 1 ||
    !is.logical(bortfall) ||
    is.na(bortfall)
  ) {
    stop("'bortfall' måste vara TRUE eller FALSE.")
  }

  data <- hamta_fran_kolada(
    nyckeltal = nyckeltal,
    kommun = kommun,
    kon = kon,
    kommuntyp = kommuntyp
  )

  if (nrow(data) == 0) {
    return(
      data.frame()
    )
  }

  # Om bortfall inte accepteras:
  # behåll endast observationer där ett faktiskt värde finns
  if (!bortfall) {

    data <- data |>
      dplyr::filter(
        !is.na(value)
      )

    if (nrow(data) == 0) {
      message(
        "Inga observationer med ett faktiskt värde hittades."
      )

      return(
        data.frame()
      )
    }
  }

  data <- data |>
    dplyr::mutate(
      period_num = suppressWarnings(
        as.numeric(period)
      )
    ) |>
    dplyr::group_by(
      kpi,
      municipality,
      gender
    ) |>
    dplyr::filter(
      period_num == max(
        period_num,
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -period_num
    ) |>
    dplyr::arrange(
      kpi,
      municipality,
      gender
    )

  rownames(data) <- NULL

  as.data.frame(data)
}

#' Beräkna förändring mellan två år
#'
#' Hämtar värden från Kolada för två valda år och beräknar
#' både absolut och procentuell förändring.
#'
#' Funktionen kan användas för en eller flera kommuner,
#' regioner och nyckeltal.
#'
#' @param nyckeltal Ett eller flera nyckeltals-ID från Kolada,
#'   exempelvis `"N01926"` eller `c("N01926", "N17454")`.
#'
#' @param kommun Valfritt. En eller flera kommunkoder eller
#'   kommun-/regionnamn, exempelvis `"0136"` eller `"Haninge"`.
#'   Om NULL används alla områden.
#'
#' @param fran Det första året i jämförelsen.
#'
#' @param till Det sista året i jämförelsen.
#'
#' @param kon Valfritt filter för kön.
#'   `"T"` = total, `"K"` = kvinnor och `"M"` = män.
#'   Om NULL används alla tillgängliga kön.
#'
#' @param kommuntyp Valfritt filter för områdestyp.
#'   `"K"` = kommun och `"R"` = region.
#'
#' @return En data.frame med värde för startår och slutår,
#'   absolut förändring och procentuell förändring.
#'
#' @examples
#' \dontrun{
#'
#' forandring(
#'   nyckeltal = "N01926",
#'   kommun = "Haninge",
#'   fran = 2020,
#'   till = 2025,
#'   kon = "T"
#' )
#'
#' forandring(
#'   nyckeltal = "N01926",
#'   kommuntyp = "K",
#'   fran = 2020,
#'   till = 2025,
#'   kon = "T"
#' )
#'
#' }
#'
#' @export
forandring <- function(
    nyckeltal,
    kommun = NULL,
    fran,
    till,
    kon = NULL,
    kommuntyp = NULL
) {

  # Kontrollera år
  if (missing(fran) || missing(till)) {
    stop("Du måste ange både 'fran' och 'till'.")
  }

  if (length(fran) != 1 || length(till) != 1) {
    stop("'fran' och 'till' måste vara ett år vardera.")
  }

  if (fran == till) {
    stop("'fran' och 'till' måste vara olika år.")
  }

  # Hämta endast de två år som behövs
  data <- hamta_fran_kolada(
    nyckeltal = nyckeltal,
    kommun = kommun,
    ar = c(fran, till),
    kon = kon,
    kommuntyp = kommuntyp
  )

  if (nrow(data) == 0) {
    message("Ingen data hittades för de valda kriterierna.")
    return(data.frame())
  }

  # Säkerställ numeriska värden
  data <- data |>
    dplyr::mutate(
      period = as.character(period),
      value = as.numeric(value)
    )

  fran_chr <- as.character(fran)
  till_chr <- as.character(till)

  # Separera startår
  data_fran <- data |>
    dplyr::filter(
      period == fran_chr
    ) |>
    dplyr::select(
      municipality,
      municipality_name,
      kpi,
      kpi_name,
      gender,
      varde_fran = value
    )

  # Separera slutår
  data_till <- data |>
    dplyr::filter(
      period == till_chr
    ) |>
    dplyr::select(
      municipality,
      municipality_name,
      kpi,
      kpi_name,
      gender,
      varde_till = value
    )

  # Slå ihop
  resultat <- dplyr::full_join(
    data_fran,
    data_till,
    by = c(
      "municipality",
      "municipality_name",
      "kpi",
      "kpi_name",
      "gender"
    )
  )

  # Lägg till år och förändring
  resultat <- resultat |>
    dplyr::mutate(
      fran = fran,
      till = till,

      forandring = varde_till - varde_fran,

      forandring_procent = dplyr::if_else(
        is.na(varde_fran) |
          varde_fran == 0 |
          is.na(varde_till),
        NA_real_,
        ((varde_till - varde_fran) / abs(varde_fran)) * 100
      )
    ) |>
    dplyr::select(
      municipality,
      municipality_name,
      kpi,
      kpi_name,
      gender,
      fran,
      till,
      varde_fran,
      varde_till,
      forandring,
      forandring_procent
    ) |>
    dplyr::arrange(
      kpi,
      municipality,
      gender
    )

  # Meddela om någon observation saknar ett av åren
  saknade <- resultat |>
    dplyr::filter(
      is.na(varde_fran) |
        is.na(varde_till)
    )

  if (nrow(saknade) > 0) {
    message(
      nrow(saknade),
      " observation(er) saknar värde för ett av jämförelseåren."
    )
  }

  rownames(resultat) <- NULL

  as.data.frame(resultat)
}

#' Hämta jämförelse för en kommun
#'
#' Hämtar värden för en vald kommun och andra kommuner i en vald
#' jämförelsegrupp.
#'
#' Jämförelsen kan göras mot kommunerna i samma län, kommunens
#' SKR-kommungrupp eller någon av Koladas grupper för liknande kommuner.
#'
#' @param nyckeltal Ett eller flera nyckeltals-ID från Kolada.
#'
#' @param kommun En kommun angiven med kommunkod eller kommunnamn,
#'   exempelvis `"2085"` eller `"Ludvika"`.
#'
#' @param ar Ett eller flera år för de nyckeltal som ska hämtas.
#'
#' @param kon Valfritt filter för kön.
#'   `"T"` = total, `"K"` = kvinnor och `"M"` = män.
#'
#' @param jamforelse Typ av jämförelse.
#'   `"lan"` = kommuner i samma län,
#'   `"kommungrupp"` = kommunens SKR-kommungrupp,
#'   `"liknande"` = liknande kommuner.
#'   Standard är `"lan"`.
#'
#' @param verksamhet Valfritt. Används när `jamforelse = "liknande"`.
#'   Exempelvis `"förskola"`, `"grundskola"` eller `"äldreomsorg"`.
#'   Om NULL visas en meny med tillgängliga grupper i Console.
#'
#' @param inkludera_grupp Logiskt värde. Om `TRUE` inkluderas även
#'   jämförelsegruppens ovägda medel. Standard är `TRUE`.
#'
#' @return En data.frame med den valda kommunen, övriga kommuner
#'   i jämförelsegruppen och, om valt, gruppens ovägda medel.
#'
#' @examples
#' \dontrun{
#'
#' hamta_jamforelse(
#'   nyckeltal = "N01926",
#'   kommun = "Ludvika",
#'   ar = 2025
#' )
#'
#' hamta_jamforelse(
#'   nyckeltal = "N01926",
#'   kommun = "Ludvika",
#'   ar = 2025,
#'   jamforelse = "kommungrupp"
#' )
#'
#' hamta_jamforelse(
#'   nyckeltal = "N01926",
#'   kommun = "Ludvika",
#'   ar = 2025,
#'   jamforelse = "liknande"
#' )
#'
#' hamta_jamforelse(
#'   nyckeltal = "N01926",
#'   kommun = "Ludvika",
#'   ar = 2025,
#'   jamforelse = "liknande",
#'   verksamhet = "förskola"
#' )
#'
#' }
#'
#' @export
hamta_jamforelse <- function(
    nyckeltal,
    kommun,
    ar,
    kon = NULL,
    jamforelse = "lan",
    verksamhet = NULL,
    inkludera_grupp = TRUE
) {

  # ---------------------------------------------------------
  # Kontrollera argument
  # ---------------------------------------------------------

  if (missing(kommun)) {
    stop("Du måste ange en kommun.")
  }

  if (length(kommun) != 1) {
    stop("hamta_jamforelse() hanterar en kommun åt gången.")
  }

  if (missing(ar)) {
    stop("Du måste ange minst ett år.")
  }

  if (
    length(inkludera_grupp) != 1 ||
    !is.logical(inkludera_grupp) ||
    is.na(inkludera_grupp)
  ) {
    stop("'inkludera_grupp' måste vara TRUE eller FALSE.")
  }

  jamforelse <- tolower(
    as.character(jamforelse)
  )

  giltiga_jamforelser <- c(
    "lan",
    "kommungrupp",
    "liknande"
  )

  if (!jamforelse %in% giltiga_jamforelser) {
    stop(
      "Ogiltig jämförelse. Använd 'lan', 'kommungrupp' eller 'liknande'."
    )
  }


  # ---------------------------------------------------------
  # Hitta vald kommun
  # ---------------------------------------------------------

  kommuner <- hamta_kommuner(
    typ = "K"
  )

  kommun_chr <- as.character(
    kommun
  )

  traff <- kommuner[
    kommuner$municipality == kommun_chr |
      tolower(kommuner$municipality_name) == tolower(kommun_chr),
    ,
    drop = FALSE
  ]

  if (nrow(traff) == 0) {
    stop(
      "Hittar ingen kommun med namn eller kod: ",
      kommun
    )
  }

  if (nrow(traff) > 1) {
    stop(
      "Flera kommuner matchade: ",
      kommun,
      ". Ange kommunkod."
    )
  }

  kommun_id <- traff$municipality[1]
  kommun_namn <- traff$municipality_name[1]


  # ---------------------------------------------------------
  # Hämta kommungrupper
  # ---------------------------------------------------------

  grupper <- .hamta_kommungrupper()


  # ---------------------------------------------------------
  # Alla grupper där vald kommun ingår
  # ---------------------------------------------------------

  kommun_grupper <- grupper[
    grupper$member_id == kommun_id,
    ,
    drop = FALSE
  ]


  # ---------------------------------------------------------
  # Välj jämförelsegrupp
  # ---------------------------------------------------------

  if (jamforelse == "lan") {

    vald_grupp <- kommun_grupper[
      grepl(
        "läns kommuner.*ovägt medel",
        kommun_grupper$title,
        ignore.case = TRUE
      ),
      ,
      drop = FALSE
    ]

    if (nrow(vald_grupp) == 0) {
      stop(
        "Kunde inte hitta någon länsgrupp för ",
        kommun_namn,
        "."
      )
    }

    vald_grupp <- vald_grupp[
      1,
      ,
      drop = FALSE
    ]

    jamforelse_namn <- "Övrig kommun i länet"
    grupp_typ_namn <- "Länets kommuner"
  }


  # ---------------------------------------------------------
  # SKR:s kommungruppsindelning
  # ---------------------------------------------------------

  if (jamforelse == "kommungrupp") {

    skr_grupper <- c(
      "Storstäder",
      "Pendlingskommun nära storstad",
      "Större stad",
      "Pendlingskommun nära större stad",
      "Lågpendlingskommun nära större stad",
      "Mindre stad/tätort",
      "Pendlingskommun nära mindre stad/tätort",
      "Landsbygdskommun",
      "Landsbygdskommun med besöksnäring"
    )

    skr_pattern <- paste(
      skr_grupper,
      collapse = "|"
    )

    vald_grupp <- kommun_grupper[
      grepl(
        skr_pattern,
        kommun_grupper$title,
        ignore.case = TRUE
      ),
      ,
      drop = FALSE
    ]

    # Ta bort eventuella liknande-kommun-grupper
    vald_grupp <- vald_grupp[
      !grepl(
        "^Liknande kommuner",
        vald_grupp$title,
        ignore.case = TRUE
      ),
      ,
      drop = FALSE
    ]

    if (nrow(vald_grupp) == 0) {
      stop(
        "Kunde inte identifiera någon SKR-kommungrupp för ",
        kommun_namn,
        "."
      )
    }

    if (nrow(vald_grupp) > 1) {

      # Föredra grupp med ovägt medel
      ovagt <- vald_grupp[
        grepl(
          "ovägt medel",
          vald_grupp$title,
          ignore.case = TRUE
        ),
        ,
        drop = FALSE
      ]

      if (nrow(ovagt) > 0) {
        vald_grupp <- ovagt
      }
    }

    vald_grupp <- vald_grupp[
      1,
      ,
      drop = FALSE
    ]

    jamforelse_namn <- "Övrig kommun i kommungruppen"
    grupp_typ_namn <- "Kommungruppen"
  }


  # ---------------------------------------------------------
  # Liknande kommuner
  # ---------------------------------------------------------

  if (jamforelse == "liknande") {

    # Utgå från samtliga unika grupper, inte bara grupper
    # där den valda kommunen finns som medlem
    alla_grupper <- grupper[
      !duplicated(grupper$id),
      ,
      drop = FALSE
    ]

    # Behåll grupper för "Liknande kommuner"
    liknande_grupper <- alla_grupper[
      grepl(
        "^Liknande kommuner",
        alla_grupper$title,
        ignore.case = TRUE
      ),
      ,
      drop = FALSE
    ]

    # -------------------------------------------------------
    # Ta reda på vilken kommun gruppen avser
    #
    # Exempel:
    # "Liknande kommuner förskola, Ludvika, 2024"
    #
    # Delarna blir:
    # 1 = Liknande kommuner förskola
    # 2 = Ludvika
    # 3 = 2024
    # -------------------------------------------------------

    delar <- strsplit(
      liknande_grupper$title,
      ",\\s*"
    )

    fokuskommun <- vapply(
      delar,
      function(x) {

        if (length(x) < 3) {
          return(NA_character_)
        }

        trimws(x[length(x) - 1])
      },
      character(1)
    )

    # Behåll endast grupper som är skapade för vald kommun
    liknande_grupper <- liknande_grupper[
      tolower(fokuskommun) == tolower(kommun_namn),
      ,
      drop = FALSE
    ]

    if (nrow(liknande_grupper) == 0) {
      stop(
        "Inga grupper för liknande kommuner hittades för ",
        kommun_namn,
        "."
      )
    }

    # -------------------------------------------------------
    # Plocka ut verksamhetsnamnet
    # -------------------------------------------------------

    verksamhetsnamn <- sub(
      ",.*$",
      "",
      liknande_grupper$title
    )

    verksamhetsnamn <- sub(
      "^Liknande kommuner\\s*",
      "",
      verksamhetsnamn,
      ignore.case = TRUE
    )

    verksamhetsnamn <- trimws(
      verksamhetsnamn
    )

    # Gruppen "Liknande kommuner, övergripande, ..."
    # ger annars ett tomt verksamhetsnamn
    verksamhetsnamn[
      verksamhetsnamn == ""
    ] <- "övergripande"

    liknande_grupper$verksamhet <- verksamhetsnamn

    # -------------------------------------------------------
    # Plocka ut året för gruppindelningen
    # -------------------------------------------------------

    grupp_ar <- vapply(
      strsplit(
        liknande_grupper$title,
        ",\\s*"
      ),
      function(x) {

        if (length(x) < 2) {
          return(NA_real_)
        }

        suppressWarnings(
          as.numeric(
            trimws(x[length(x)])
          )
        )
      },
      numeric(1)
    )

    liknande_grupper$grupp_ar <- grupp_ar

    # -------------------------------------------------------
    # Om samma verksamhet finns för flera år:
    # behåll senaste gruppindelningen
    # -------------------------------------------------------

    liknande_grupper <- liknande_grupper |>
      dplyr::arrange(
        verksamhet,
        dplyr::desc(grupp_ar)
      ) |>
      dplyr::group_by(
        verksamhet
      ) |>
      dplyr::slice_head(
        n = 1
      ) |>
      dplyr::ungroup() |>
      dplyr::arrange(
        tolower(verksamhet)
      )

    # -------------------------------------------------------
    # Om verksamheten anges direkt
    # -------------------------------------------------------

    if (!is.null(verksamhet)) {

      matchning <- liknande_grupper[
        tolower(liknande_grupper$verksamhet) ==
          tolower(verksamhet),
        ,
        drop = FALSE
      ]

      # Försök delmatchning om exakt match saknas
      if (nrow(matchning) == 0) {

        matchning <- liknande_grupper[
          grepl(
            verksamhet,
            liknande_grupper$verksamhet,
            ignore.case = TRUE,
            fixed = TRUE
          ),
          ,
          drop = FALSE
        ]
      }

      if (nrow(matchning) == 0) {
        stop(
          "Ingen grupp för liknande kommuner hittades för verksamheten: ",
          verksamhet
        )
      }

      if (nrow(matchning) > 1) {
        stop(
          "Flera verksamheter matchade '",
          verksamhet,
          "'. Ange ett mer exakt namn."
        )
      }

      vald_grupp <- matchning

    } else {

      # -----------------------------------------------------
      # Visa tillgängliga grupper i Console
      # -----------------------------------------------------

      meny_text <- paste0(
        liknande_grupper$verksamhet,
        " (",
        liknande_grupper$grupp_ar,
        ")"
      )

      cat(
        "\nVälj grupp för liknande kommuner för ",
        kommun_namn,
        ":\n\n",
        sep = ""
      )

      for (i in seq_along(meny_text)) {
        cat(
          i,
          ": ",
          meny_text[i],
          "\n",
          sep = ""
        )
      }

      cat("\n")

      val <- suppressWarnings(
        as.integer(
          readline("Selection: ")
        )
      )

      if (
        is.na(val) ||
        val < 1 ||
        val > length(meny_text)
      ) {
        stop("Ogiltigt val.")
      }

      vald_grupp <- liknande_grupper[
        val,
        ,
        drop = FALSE
      ]
    }

    jamforelse_namn <- "Liknande kommun"
    grupp_typ_namn <- "Liknande kommuner"
  }

  # ---------------------------------------------------------
  # Grupp-ID och gruppnamn
  # ---------------------------------------------------------

  grupp_id <- as.character(
    vald_grupp$id[1]
  )

  grupp_namn <- as.character(
    vald_grupp$title[1]
  )


  # ---------------------------------------------------------
  # Hitta alla kommuner som ingår i vald grupp
  # ---------------------------------------------------------

  kommuner_i_gruppen <- grupper[
    grupper$id == grupp_id,
    ,
    drop = FALSE
  ]

  kommun_idn <- unique(
    c(
      kommun_id,
      as.character(
        kommuner_i_gruppen$member_id
      )
    )
  )


  # ---------------------------------------------------------
  # Hämta värden för alla kommuner i gruppen
  # ---------------------------------------------------------

  data_kommuner <- hamta_fran_kolada(
    nyckeltal = nyckeltal,
    kommun = kommun_idn,
    ar = ar,
    kon = kon
  )

  if (nrow(data_kommuner) > 0) {

    data_kommuner <- data_kommuner |>
      dplyr::mutate(
        jamforelsetyp = dplyr::if_else(
          municipality == kommun_id,
          "Vald kommun",
          jamforelse_namn
        ),
        jamforelsegrupp = grupp_namn
      )
  }


  # ---------------------------------------------------------
  # Hämta gruppens ovägda medel
  # ---------------------------------------------------------

  if (inkludera_grupp) {

    data_grupp <- lapply(
      nyckeltal,
      function(kpi_id) {

        # Hämta ett år i taget för att undvika långa URL:er
        data_ar <- lapply(
          ar,
          function(ar_id) {

            url <- paste0(
              "https://api.kolada.se/v3/data/kpi/",
              kpi_id,
              "/municipality/",
              grupp_id,
              "/year/",
              ar_id,
              "?page=1&per_page=5000"
            )

            svar <- tryCatch(
              .kolada_fromJSON(url),
              error = function(e) NULL
            )

            if (
              is.null(svar) ||
              is.null(svar$values) ||
              length(svar$values) == 0
            ) {
              return(NULL)
            }

            data.frame(svar$values) |>
              tidyr::unnest(values)
          }
        )

        dplyr::bind_rows(
          data_ar
        )
      }
    )

    data_grupp <- dplyr::bind_rows(
      data_grupp
    )

    if (nrow(data_grupp) > 0) {

      if (!is.null(kon)) {

        data_grupp <- data_grupp |>
          dplyr::filter(
            gender %in% toupper(kon)
          )
      }

      # KPI-namn
      kpi_namn <- lapply(
        unique(data_grupp$kpi),
        function(kpi_id) {

          metadata <- .hamta_kpi_metadata(
            kpi_id
          )

          data.frame(
            kpi = kpi_id,
            kpi_name = metadata$title[1],
            stringsAsFactors = FALSE
          )
        }
      )

      kpi_namn <- dplyr::bind_rows(
        kpi_namn
      )

      data_grupp <- data_grupp |>
        dplyr::left_join(
          kpi_namn,
          by = "kpi"
        ) |>
        dplyr::mutate(
          municipality_name = grupp_namn,
          jamforelsetyp = grupp_typ_namn,
          jamforelsegrupp = grupp_namn
        ) |>
        dplyr::select(
          municipality,
          municipality_name,
          kpi,
          kpi_name,
          period,
          value,
          gender,
          status,
          jamforelsetyp,
          jamforelsegrupp
        )
    }

  } else {

    data_grupp <- data.frame()
  }


  # ---------------------------------------------------------
  # Slå ihop resultat
  # ---------------------------------------------------------

  resultat <- dplyr::bind_rows(
    data_kommuner,
    data_grupp
  )

  if (nrow(resultat) == 0) {
    message("Ingen data hittades för jämförelsen.")
    return(data.frame())
  }

  resultat <- resultat |>
    dplyr::select(
      jamforelsetyp,
      jamforelsegrupp,
      municipality,
      municipality_name,
      kpi,
      kpi_name,
      period,
      value,
      gender,
      status
    ) |>
    dplyr::mutate(
      sortering = dplyr::case_when(
        jamforelsetyp == "Vald kommun" ~ 1,
        jamforelsetyp == grupp_typ_namn ~ 3,
        TRUE ~ 2
      )
    ) |>
    dplyr::arrange(
      kpi,
      period,
      gender,
      sortering,
      municipality_name
    ) |>
    dplyr::select(
      -sortering
    )

  rownames(resultat) <- NULL

  as.data.frame(resultat)
}

#' Hämta enheter från Kolada
#'
#' Hämtar organisatoriska enheter och, valfritt,
#' nyckeltalsdata på enhetsnivå.
#'
#' @param verksamhet Verksamhet eller V-kod, exempelvis `"Förskola"` eller `"V11"`.
#' @param kommun Valfri kommun angiven med namn eller kod.
#' @param nyckeltal Valfritt nyckeltals-ID.
#' @param ar Valfritt år eller flera år.
#' @param kon Valfritt kön: `"T"`, `"K"` eller `"M"`.
#' @param sok Valfri söktext för enhetsnamn.
#' @param batch_size Antal enheter per API-anrop. Standard är 25.
#'
#' @details
#' Följande verksamheter kan anges med namn eller V-kod:
#'
#' \tabular{ll}{
#' \strong{Verksamhet} \tab \strong{V-kod} \cr
#' Förskola \tab V11 \cr
#' Grundskola F-9 \tab V15 \cr
#' Gymnasieskola \tab V17 \cr
#' Hemtjänst, äldre \tab V21 \cr
#' Särskilt boende, äldre \tab V23 \cr
#' LSS boende med särskild service \tab V25 \cr
#' LSS daglig verksamhet \tab V26 \cr
#' Gruppbostad LSS \tab V29 \cr
#' Servicebostad LSS \tab V30 \cr
#' SoL boendestöd \tab V31 \cr
#' SoL boende med särskild service \tab V32 \cr
#' SoL sysselsättning \tab V34 \cr
#' Våld i nära relationer \tab V45 \cr
#' Fastigheter \tab V60
#' }
#'
#' Verksamhet kan anges antingen med namn, exempelvis
#' `verksamhet = "Förskola"`, eller direkt med V-kod,
#' exempelvis `verksamhet = "V11"`.
#'
#' Exakta värden som kan anges i `verksamhet` är:
#'
#' \tabular{ll}{
#' \strong{verksamhet} \tab \strong{V-kod} \cr
#' förskola \tab V11 \cr
#' grundskola \tab V15 \cr
#' gymnasieskola \tab V17 \cr
#' hemtjänst \tab V21 \cr
#' särskilt boende \tab V23 \cr
#' lss boende med särskild service \tab V25 \cr
#' lss daglig verksamhet \tab V26 \cr
#' gruppbostad lss \tab V29 \cr
#' servicebostad lss \tab V30 \cr
#' sol boendestöd \tab V31 \cr
#' sol boende med särskild service \tab V32 \cr
#' sol sysselsättning \tab V34 \cr
#' våld i nära relationer \tab V45 \cr
#' fastigheter \tab V60
#' }
#'
#' @return En data.frame med enheter och, om nyckeltal anges,
#' nyckeltalsvärden på enhetsnivå.
#'
#' @examples
#' \dontrun{
#' # Hämta alla förskolor i Haninge
#' hamta_enheter(
#'   verksamhet = "Förskola",
#'   kommun = "Haninge"
#' )
#'
#' # Samma sak med V-kod och kommunkod
#' hamta_enheter(
#'   verksamhet = "V11",
#'   kommun = "0136"
#' )
#'
#' # Hämta även nyckeltalsvärden
#' hamta_enheter(
#'   verksamhet = "V11",
#'   kommun = "0136",
#'   nyckeltal = "N11808",
#'   ar = 2025
#' )
#'
#' #' # Hämta nyckelord
#' hamta_enheter(
#'   verksamhet = "V11",
#'   nyckeltal = "N11808",
#'   ar = 2025,
#'   kommun = "Montessori"
#' )
#' }
#'
#' @export
hamta_enheter <- function(
    verksamhet = NULL,
    kommun = NULL,
    nyckeltal = NULL,
    ar = NULL,
    kon = NULL,
    sok = NULL,
    batch_size = 25
) {

  verksamhetskoder <- c(
    "förskola" = "V11",
    "grundskola" = "V15",
    "gymnasieskola" = "V17",
    "hemtjänst" = "V21",
    "särskilt boende" = "V23",
    "lss boende med särskild service" = "V25",
    "lss daglig verksamhet" = "V26",
    "gruppbostad lss" = "V29",
    "servicebostad lss" = "V30",
    "sol boendestöd" = "V31",
    "sol boende med särskild service" = "V32",
    "sol sysselsättning" = "V34",
    "våld i nära relationer" = "V45",
    "fastigheter" = "V60"
  )

  # ---------------------------------------------------------
  # Matcha verksamhet
  # ---------------------------------------------------------

  verksamhetskod <- NULL

  if (!is.null(verksamhet)) {

    verksamhet_chr <- as.character(
      verksamhet
    )

    if (
      toupper(verksamhet_chr) %in%
      unname(verksamhetskoder)
    ) {

      verksamhetskod <- toupper(
        verksamhet_chr
      )

    } else {

      verksamhet_lower <- tolower(
        verksamhet_chr
      )

      if (
        !verksamhet_lower %in%
        names(verksamhetskoder)
      ) {
        stop(
          "Okänd verksamhet: ",
          verksamhet,
          "."
        )
      }

      verksamhetskod <- unname(
        verksamhetskoder[
          verksamhet_lower
        ]
      )
    }
  }


  # ---------------------------------------------------------
  # Matcha kommun
  # ---------------------------------------------------------

  kommun_id <- NULL

  if (!is.null(kommun)) {

    kommuner <- hamta_kommuner(
      typ = "K"
    )

    kommun_chr <- as.character(
      kommun
    )

    traff <- kommuner[
      kommuner$municipality == kommun_chr |
        tolower(
          kommuner$municipality_name
        ) == tolower(kommun_chr),
      ,
      drop = FALSE
    ]

    if (nrow(traff) == 0) {
      stop(
        "Hittar ingen kommun med namn eller kod: ",
        kommun
      )
    }

    if (nrow(traff) > 1) {
      stop(
        "Flera kommuner matchade: ",
        kommun,
        ". Ange kommunkod."
      )
    }

    kommun_id <- traff$municipality[1]
  }


  # ---------------------------------------------------------
  # Bygg URL för enheter
  # ---------------------------------------------------------

  url <- "https://api.kolada.se/v3/ou"

  parametrar <- character(0)

  if (!is.null(kommun_id)) {

    parametrar <- c(
      parametrar,
      paste0(
        "municipality=",
        kommun_id
      )
    )
  }

  if (!is.null(sok)) {

    parametrar <- c(
      parametrar,
      paste0(
        "title=",
        utils::URLencode(
          sok,
          reserved = TRUE
        )
      )
    )
  }

  if (length(parametrar) > 0) {

    url <- paste0(
      url,
      "?",
      paste(
        parametrar,
        collapse = "&"
      )
    )
  }


  # ---------------------------------------------------------
  # Hämta enheter
  # ---------------------------------------------------------

  enheter <- .hamta_alla_sidor(
    url
  )

  if (nrow(enheter) == 0) {

    message(
      "Inga enheter hittades."
    )

    return(
      data.frame()
    )
  }

  enheter$id <- as.character(
    enheter$id
  )

  enheter$municipality <- as.character(
    enheter$municipality
  )


  # ---------------------------------------------------------
  # Filtrera på verksamhetskod
  # ---------------------------------------------------------

  if (!is.null(verksamhetskod)) {

    enheter <- enheter[
      startsWith(
        enheter$id,
        verksamhetskod
      ),
      ,
      drop = FALSE
    ]
  }

  if (nrow(enheter) == 0) {

    message(
      "Inga enheter hittades för den valda verksamheten."
    )

    return(
      data.frame()
    )
  }


  # ---------------------------------------------------------
  # Lägg till verksamhetsnamn
  # ---------------------------------------------------------

  kod_till_namn <- setNames(
    names(verksamhetskoder),
    verksamhetskoder
  )

  enheter$unit_type <- substr(
    enheter$id,
    1,
    3
  )

  enheter$unit_type_name <- unname(
    kod_till_namn[
      enheter$unit_type
    ]
  )


  # ---------------------------------------------------------
  # Lägg till kommunnamn
  # ---------------------------------------------------------

  kommunmetadata <- .hamta_kommunmetadata()

  enheter <- enheter |>
    dplyr::left_join(
      kommunmetadata |>
        dplyr::select(
          municipality = id,
          municipality_name = title
        ),
      by = "municipality"
    ) |>
    dplyr::rename(
      unit_id = id,
      unit_name = title
    ) |>
    dplyr::select(
      unit_id,
      unit_name,
      unit_type,
      unit_type_name,
      municipality,
      municipality_name
    ) |>
    dplyr::arrange(
      municipality_name,
      unit_name
    )


  # ---------------------------------------------------------
  # Om nyckeltal inte anges:
  # returnera endast enhetslistan
  # ---------------------------------------------------------

  if (is.null(nyckeltal)) {

    rownames(enheter) <- NULL

    return(
      as.data.frame(enheter)
    )
  }


  # ---------------------------------------------------------
  # Kontrollera nyckeltal och kön
  # ---------------------------------------------------------

  nyckeltal <- as.character(
    nyckeltal
  )

  if (!is.null(kon)) {

    kon <- toupper(
      as.character(kon)
    )

    ogiltiga_kon <- setdiff(
      kon,
      c(
        "T",
        "K",
        "M"
      )
    )

    if (length(ogiltiga_kon) > 0) {

      stop(
        "Ogiltigt värde för kön: ",
        paste(
          ogiltiga_kon,
          collapse = ", "
        ),
        ". Använd T, K eller M."
      )
    }
  }


  # ---------------------------------------------------------
  # Dela enheter i batchar
  # ---------------------------------------------------------

  enhets_batchar <- split(
    enheter$unit_id,
    ceiling(
      seq_along(
        enheter$unit_id
      ) / batch_size
    )
  )


  # ---------------------------------------------------------
  # Dela år i batchar
  # ---------------------------------------------------------

  if (is.null(ar)) {

    ar_batchar <- list(
      NULL
    )

  } else {

    ar <- as.character(
      ar
    )

    ar_batchar <- split(
      ar,
      ceiling(
        seq_along(ar) / 20
      )
    )
  }


  # ---------------------------------------------------------
  # Hämta nyckeltalsdata på enhetsnivå
  # ---------------------------------------------------------

  resultat <- list()
  index <- 1

  for (kpi_id in nyckeltal) {

    for (enhets_batch in enhets_batchar) {

      enhets_del <- paste(
        enhets_batch,
        collapse = ","
      )

      for (ar_batch in ar_batchar) {

        url_data <- paste0(
          "https://api.kolada.se/v3/oudata/kpi/",
          kpi_id,
          "/ou/",
          enhets_del
        )

        if (!is.null(ar_batch)) {

          url_data <- paste0(
            url_data,
            "/year/",
            paste(
              ar_batch,
              collapse = ","
            )
          )
        }

        svar <- tryCatch(
          .hamta_alla_sidor(
            url_data
          ),
          error = function(e) {
            NULL
          }
        )

        if (
          is.null(svar) ||
          nrow(svar) == 0
        ) {
          next
        }

        # API-resultatet kan innehålla nästlade values
        if ("values" %in% names(svar)) {

          svar <- svar |>
            tidyr::unnest(values)
        }

        resultat[[index]] <- svar

        index <- index + 1
      }
    }
  }


  # ---------------------------------------------------------
  # Slå ihop enhetsdata
  # ---------------------------------------------------------

  enhetsdata <- dplyr::bind_rows(
    resultat
  )

  if (nrow(enhetsdata) == 0) {

    message(
      "Inga nyckeltalsvärden hittades för de valda enheterna."
    )

    rownames(enheter) <- NULL

    return(
      as.data.frame(enheter)
    )
  }


  # ---------------------------------------------------------
  # Filtrera kön
  # ---------------------------------------------------------

  if (!is.null(kon)) {

    enhetsdata <- enhetsdata |>
      dplyr::filter(
        gender %in% kon
      )
  }


  # ---------------------------------------------------------
  # Identifiera kolumnen med enhets-ID
  # ---------------------------------------------------------

  ou_kolumn <- intersect(
    c(
      "ou",
      "unit",
      "organizationalunit",
      "organizational_unit"
    ),
    names(enhetsdata)
  )

  if (length(ou_kolumn) == 0) {

    stop(
      "Kunde inte identifiera enhets-ID i API-resultatet."
    )
  }

  ou_kolumn <- ou_kolumn[1]

  enhetsdata <- enhetsdata |>
    dplyr::rename(
      unit_id = dplyr::all_of(
        ou_kolumn
      )
    )

  enhetsdata$unit_id <- as.character(
    enhetsdata$unit_id
  )


  # ---------------------------------------------------------
  # Lägg till KPI-namn
  # ---------------------------------------------------------

  kpi_metadata <- lapply(
    unique(
      enhetsdata$kpi
    ),
    function(kpi_id) {

      metadata <- .hamta_kpi_metadata(
        kpi_id
      )

      data.frame(
        kpi = kpi_id,
        kpi_name = metadata$title[1],
        stringsAsFactors = FALSE
      )
    }
  )

  kpi_metadata <- dplyr::bind_rows(
    kpi_metadata
  )

  enhetsdata <- enhetsdata |>
    dplyr::left_join(
      kpi_metadata,
      by = "kpi"
    )


  # ---------------------------------------------------------
  # Lägg ihop enhetsinformation och nyckeltalsvärden
  # ---------------------------------------------------------

  enheter <- enheter |>
    dplyr::left_join(
      enhetsdata |>
        dplyr::select(
          unit_id,
          kpi,
          kpi_name,
          period,
          value,
          gender,
          status
        ),
      by = "unit_id"
    ) |>
    dplyr::arrange(
      municipality_name,
      unit_name,
      kpi,
      period,
      gender
    )


  # ---------------------------------------------------------
  # Returnera resultat
  # ---------------------------------------------------------

  rownames(enheter) <- NULL

  as.data.frame(
    enheter
  )
}

# =========================================================
# English wrappers
# =========================================================


#' Get data from Kolada
#'
#' English wrapper for [hamta_fran_kolada()].
#'
#' @param kpi One or more Kolada KPI IDs.
#' @param municipality Optional municipality/region name or code.
#' @param year Optional year or vector of years.
#' @param gender Optional gender filter:
#'   `"T"` = total, `"K"` = women, `"M"` = men.
#' @param municipality_type Optional area type:
#'   `"K"` = municipality, `"R"` or `"L"` = region.
#'   Kolada's API uses `"L"` for regions, but both values are accepted.
#' @param per_page Number of observations per API page.
#' @param batch_size Number of municipalities/regions per API request.
#'
#' @return A data.frame with data from Kolada.
#'
#' @examples
#' \dontrun{
#' get_from_kolada(
#'   kpi = "N01926",
#'   municipality = "Haninge",
#'   year = 2025
#' )
#' }
#'
#' @export
get_from_kolada <- function(
    kpi,
    municipality = NULL,
    year = NULL,
    gender = NULL,
    municipality_type = NULL,
    per_page = 5000,
    batch_size = 25
) {

  hamta_fran_kolada(
    nyckeltal = kpi,
    kommun = municipality,
    ar = year,
    kon = gender,
    kommuntyp = municipality_type,
    per_page = per_page,
    batch_size = batch_size
  )
}


#' Search for KPIs in Kolada
#'
#' English wrapper for [sok_nyckeltal()].
#'
#' @param query Search term or words.
#' @param max_results Maximum number of results. Default is 50.
#'
#' @return A data.frame with matching KPIs.
#'
#' @examples
#' \dontrun{
#' search_kpi("preschool")
#' search_kpi("N01926")
#' }
#'
#' @export
search_kpi <- function(
    query,
    max_results = 50
) {

  sok_nyckeltal(
    sok = query,
    max_resultat = max_results
  )
}


#' Get information about KPIs
#'
#' English wrapper for [info_nyckeltal()].
#'
#' @param kpi One or more Kolada KPI IDs.
#'
#' @return A data.frame with KPI metadata.
#'
#' @examples
#' \dontrun{
#' kpi_info("N01926")
#' kpi_info(c("N01926", "N17454"))
#' }
#'
#' @export
kpi_info <- function(kpi) {

  info_nyckeltal(
    nyckeltal = kpi
  )
}


#' Get municipalities and regions
#'
#' English wrapper for [hamta_kommuner()].
#'
#' @param type Optional area type:
#'   `"K"` = municipality, `"R"` or `"L"` = region.
#' @param search Optional search text for name or code.
#'
#' @return A data.frame with municipalities and regions.
#'
#' @examples
#' \dontrun{
#' get_municipalities()
#' get_municipalities(type = "K")
#' get_municipalities(search = "Han")
#' }
#'
#' @export
get_municipalities <- function(
    type = NULL,
    search = NULL
) {

  hamta_kommuner(
    typ = type,
    sok = search
  )
}


#' Get available years
#'
#' English wrapper for [tillgangliga_ar()].
#'
#' @param kpi One or more Kolada KPI IDs.
#' @param municipality Optional municipality or region name/code.
#' @param municipality_type Optional area type:
#'   `"K"` = municipality, `"R"` or `"L"` = region.
#'
#' @return A data.frame with available years.
#'
#' @examples
#' \dontrun{
#' available_years(
#'   kpi = "N01926",
#'   municipality = "Haninge"
#' )
#' }
#'
#' @export
available_years <- function(
    kpi,
    municipality = NULL,
    municipality_type = NULL
) {

  tillgangliga_ar(
    nyckeltal = kpi,
    kommun = municipality,
    kommuntyp = municipality_type
  )
}


#' Get latest available value
#'
#' English wrapper for [senaste_varde()].
#'
#' @param kpi One or more Kolada KPI IDs.
#' @param municipality Optional municipality or region.
#' @param gender Optional gender filter.
#' @param municipality_type Optional area type:
#'   `"K"` = municipality, `"R"` or `"L"` = region.
#' @param include_missing Logical. If `FALSE`, the latest observation
#'   with an actual value is returned. If `TRUE`, observations with
#'   missing values due to e.g. confidentiality or missing data are
#'   also accepted. Default is `FALSE`.
#'
#' @return A data.frame with the latest available observation.
#'
#' @examples
#' \dontrun{
#' latest_value(
#'   kpi = "N01926",
#'   municipality = "Haninge"
#' )
#' }
#'
#' @export
latest_value <- function(
    kpi,
    municipality = NULL,
    gender = NULL,
    municipality_type = NULL,
    include_missing = FALSE
) {

  senaste_varde(
    nyckeltal = kpi,
    kommun = municipality,
    kon = gender,
    kommuntyp = municipality_type,
    bortfall = include_missing
  )
}


#' Calculate change between two years
#'
#' English wrapper for [forandring()].
#'
#' @param kpi One or more Kolada KPI IDs.
#' @param municipality Optional municipality or region name/code.
#' @param start_year First year in the comparison.
#' @param end_year Last year in the comparison.
#' @param gender Optional gender filter.
#' @param municipality_type Optional area type:
#'   `"K"` = municipality, `"R"` or `"L"` = region.
#'
#' @return A data.frame with values for both years and calculated change.
#'
#' @export
change <- function(
    kpi,
    municipality = NULL,
    start_year,
    end_year,
    gender = NULL,
    municipality_type = NULL
) {

  resultat <- forandring(
    nyckeltal = kpi,
    kommun = municipality,
    fran = start_year,
    till = end_year,
    kon = gender,
    kommuntyp = municipality_type
  )

  if (nrow(resultat) == 0) {
    return(resultat)
  }

  resultat <- resultat |>
    dplyr::rename(
      start_year = fran,
      end_year = till,
      start_value = varde_fran,
      end_value = varde_till,
      change = forandring,
      percentage_change = forandring_procent
    )

  as.data.frame(resultat)
}


#' Get comparison data for a municipality
#'
#' English wrapper for [hamta_jamforelse()].
#'
#' A municipality can be compared with municipalities in the same
#' county, the same SKR municipality group, or a group of similar
#' municipalities.
#'
#' @param kpi One or more Kolada KPI IDs.
#' @param municipality Municipality name or code.
#' @param year One or more years.
#' @param gender Optional gender filter:
#'   `"T"` = total, `"K"` = women and `"M"` = men.
#' @param comparison Comparison type:
#'   `"county"`, `"municipality_group"` or `"similar"`.
#'   Default is `"county"`.
#' @param area Optional service area when `comparison = "similar"`.
#'   Examples include `"preschool"`, `"compulsory_school"` and
#'   `"elderly_care"`. If omitted, an interactive menu is shown.
#' @param include_group Logical. If `TRUE`, the group value is included.
#'   Default is `TRUE`.
#'
#' @return A data.frame with comparison data. The result includes
#'   `comparison_type` and `comparison_group`, indicating the type
#'   of observation and the Kolada group used for the comparison.
#'
#' @examples
#' \dontrun{
#'
#' # Compare with municipalities in the same county
#' get_comparison(
#'   kpi = "N01926",
#'   municipality = "Ludvika",
#'   year = 2025,
#'   comparison = "county"
#' )
#'
#' # Compare with the same SKR municipality group
#' get_comparison(
#'   kpi = "N01926",
#'   municipality = "Ludvika",
#'   year = 2025,
#'   comparison = "municipality_group"
#' )
#'
#' # Compare with similar municipalities using the interactive menu
#' get_comparison(
#'   kpi = "N01926",
#'   municipality = "Ludvika",
#'   year = 2025,
#'   comparison = "similar"
#' )
#'
#' # Specify the service area directly
#' get_comparison(
#'   kpi = "N01926",
#'   municipality = "Ludvika",
#'   year = 2025,
#'   comparison = "similar",
#'   area = "preschool"
#' )
#'
#' }
#'
#' @export
get_comparison <- function(
    kpi,
    municipality,
    year,
    gender = NULL,
    comparison = "county",
    area = NULL,
    include_group = TRUE
) {

  # ---------------------------------------------------------
  # Translate comparison type
  # ---------------------------------------------------------

  comparison_sv <- switch(
    tolower(comparison),

    "county" = "lan",
    "lan" = "lan",

    "municipality_group" = "kommungrupp",
    "municipality group" = "kommungrupp",
    "kommungrupp" = "kommungrupp",

    "similar" = "liknande",
    "liknande" = "liknande",

    stop(
      "Invalid comparison. Use 'county', ",
      "'municipality_group' or 'similar'."
    )
  )


  # ---------------------------------------------------------
  # Translate common service areas
  # ---------------------------------------------------------

  if (!is.null(area)) {

    area_lower <- tolower(area)

    area_map <- c(
      "labour_market" = "arbetsmarknad",
      "labor_market" = "arbetsmarknad",
      "economic_assistance" = "ekonomiskt bistånd",
      "leisure_time_centre" = "fritidshem",
      "leisure_time_center" = "fritidshem",
      "preschool" = "förskola",
      "compulsory_school" = "grundskola",
      "upper_secondary_school" = "gymnasieskola",
      "individual_and_family_care" = "IFO",
      "ifo" = "IFO",
      "lss" = "LSS",
      "rescue_service" = "räddningstjänst",
      "socioeconomic" = "socioekonomi",
      "elderly_care" = "äldreomsorg",
      "overall" = "övergripande"
    )

    if (area_lower %in% names(area_map)) {

      area <- unname(
        area_map[area_lower]
      )
    }
  }


  # ---------------------------------------------------------
  # Run Swedish function
  # ---------------------------------------------------------

  resultat <- hamta_jamforelse(
    nyckeltal = kpi,
    kommun = municipality,
    ar = year,
    kon = gender,
    jamforelse = comparison_sv,
    verksamhet = area,
    inkludera_grupp = include_group
  )


  # ---------------------------------------------------------
  # Return empty result if no data was found
  # ---------------------------------------------------------

  if (nrow(resultat) == 0) {
    return(resultat)
  }


  # ---------------------------------------------------------
  # Rename result columns to English
  # ---------------------------------------------------------

  resultat <- resultat |>
    dplyr::rename(
      comparison_type = jamforelsetyp,
      comparison_group = jamforelsegrupp
    )


  # ---------------------------------------------------------
  # Translate comparison type values
  # ---------------------------------------------------------

  resultat <- resultat |>
    dplyr::mutate(
      comparison_type = dplyr::case_when(
        comparison_type == "Vald kommun" ~ "Selected municipality",
        comparison_type == "Övrig kommun i länet" ~ "Other municipality in county",
        comparison_type == "Länets kommuner" ~ "County municipalities",
        comparison_type == "Övrig kommun i kommungruppen" ~
          "Other municipality in municipality group",
        comparison_type == "Kommungruppen" ~ "Municipality group",
        comparison_type == "Liknande kommun" ~ "Similar municipality",
        comparison_type == "Liknande kommuner" ~ "Similar municipalities",
        TRUE ~ as.character(comparison_type)
      )
    )


  # ---------------------------------------------------------
  # Return data.frame
  # ---------------------------------------------------------

  rownames(resultat) <- NULL

  as.data.frame(resultat)
}

#' Get organizational units from Kolada
#'
#' English wrapper for [hamta_enheter()].
#'
#' Retrieves organizational units from Kolada and, optionally,
#' KPI data at unit level.
#'
#' @param activity Optional activity type. Can be specified using an
#'   English activity name or a Kolada V-code, for example
#'   `"preschool"` or `"V11"`.
#' @param municipality Optional municipality name or code,
#'   for example `"Haninge"` or `"0136"`.
#' @param kpi Optional one or more Kolada KPI IDs.
#' @param year Optional year or vector of years.
#' @param gender Optional gender filter:
#'   `"T"` = total, `"K"` = women, `"M"` = men.
#' @param search Optional search text for unit name.
#' @param batch_size Number of units included in each API request.
#'   Default is 25.
#'
#' @details
#' The following activity types can be specified using an English
#' activity name or a Kolada V-code:
#'
#' \tabular{ll}{
#' \strong{Activity} \tab \strong{V-code} \cr
#' Preschool \tab V11 \cr
#' Compulsory school F-9 \tab V15 \cr
#' Upper secondary school \tab V17 \cr
#' Home care, elderly \tab V21 \cr
#' Special housing, elderly \tab V23 \cr
#' LSS housing with special services \tab V25 \cr
#' LSS daily activity \tab V26 \cr
#' LSS group home \tab V29 \cr
#' LSS service home \tab V30 \cr
#' Social Services housing support \tab V31 \cr
#' Social Services housing with special services \tab V32 \cr
#' Social Services employment \tab V34 \cr
#' Domestic violence \tab V45 \cr
#' Properties \tab V60
#' }
#'
#' The `activity` argument can use either the English activity name,
#' for example `activity = "preschool"`, or the V-code directly,
#' for example `activity = "V11"`.
#'
#' English activity names accepted by the function include:
#'
#' \tabular{ll}{
#' \strong{activity} \tab \strong{V-code} \cr
#' preschool \tab V11 \cr
#' compulsory_school \tab V15 \cr
#' upper_secondary_school \tab V17 \cr
#' home_care \tab V21 \cr
#' special_housing_elderly \tab V23 \cr
#' lss_special_service_housing \tab V25 \cr
#' lss_daily_activity \tab V26 \cr
#' lss_group_home \tab V29 \cr
#' lss_service_home \tab V30 \cr
#' social_services_housing_support \tab V31 \cr
#' social_services_special_housing \tab V32 \cr
#' social_services_employment \tab V34 \cr
#' domestic_violence \tab V45 \cr
#' properties \tab V60
#' }
#'
#' @return A data.frame with organizational units and, if `kpi`
#'   is specified, KPI values at unit level.
#'
#' @examples
#' \dontrun{
#' # Get all preschools in Haninge
#' get_units(
#'   activity = "preschool",
#'   municipality = "Haninge"
#' )
#'
#' # The same query using a V-code and municipality code
#' get_units(
#'   activity = "V11",
#'   municipality = "0136"
#' )
#'
#' # Get KPI values for the units
#' get_units(
#'   activity = "preschool",
#'   municipality = "Haninge",
#'   kpi = "N11808",
#'   year = 2025
#' )
#' }
#'
#' @export
get_units <- function(
    activity = NULL,
    municipality = NULL,
    kpi = NULL,
    year = NULL,
    gender = NULL,
    search = NULL,
    batch_size = 25
) {

  # ---------------------------------------------------------
  # Translate activity names
  # ---------------------------------------------------------

  if (!is.null(activity)) {

    activity_chr <- as.character(
      activity
    )

    # V-codes can be passed directly
    if (!grepl(
      "^V[0-9]{2}$",
      toupper(activity_chr)
    )) {

      activity_lower <- tolower(
        activity_chr
      )

      activity_map <- c(
        "preschool" =
          "förskola",

        "compulsory_school" =
          "grundskola",

        "compulsory school" =
          "grundskola",

        "upper_secondary_school" =
          "gymnasieskola",

        "upper secondary school" =
          "gymnasieskola",

        "home_care" =
          "hemtjänst",

        "home care" =
          "hemtjänst",

        "special_housing_elderly" =
          "särskilt boende",

        "special housing elderly" =
          "särskilt boende",

        "lss_special_service_housing" =
          "lss boende med särskild service",

        "lss special service housing" =
          "lss boende med särskild service",

        "lss_daily_activity" =
          "lss daglig verksamhet",

        "lss daily activity" =
          "lss daglig verksamhet",

        "lss_group_home" =
          "gruppbostad lss",

        "lss group home" =
          "gruppbostad lss",

        "lss_service_home" =
          "servicebostad lss",

        "lss service home" =
          "servicebostad lss",

        "social_services_housing_support" =
          "sol boendestöd",

        "social services housing support" =
          "sol boendestöd",

        "social_services_special_housing" =
          "sol boende med särskild service",

        "social services special housing" =
          "sol boende med särskild service",

        "social_services_employment" =
          "sol sysselsättning",

        "social services employment" =
          "sol sysselsättning",

        "domestic_violence" =
          "våld i nära relationer",

        "domestic violence" =
          "våld i nära relationer",

        "properties" =
          "fastigheter",

        "property" =
          "fastigheter"
      )

      if (activity_lower %in% names(activity_map)) {

        activity <- unname(
          activity_map[
            activity_lower
          ]
        )
      }
    }
  }


  # ---------------------------------------------------------
  # Run Swedish function
  # ---------------------------------------------------------

  resultat <- hamta_enheter(
    verksamhet = activity,
    kommun = municipality,
    nyckeltal = kpi,
    ar = year,
    kon = gender,
    sok = search,
    batch_size = batch_size
  )


  # ---------------------------------------------------------
  # Return empty result if no units were found
  # ---------------------------------------------------------

  if (nrow(resultat) == 0) {
    return(resultat)
  }


  # ---------------------------------------------------------
  # Translate unit type names to English
  # ---------------------------------------------------------

  if ("unit_type_name" %in% names(resultat)) {

    resultat <- resultat |>
      dplyr::mutate(
        unit_type_name = dplyr::case_when(
          unit_type_name == "förskola" ~
            "preschool",

          unit_type_name == "grundskola" ~
            "compulsory school",

          unit_type_name == "gymnasieskola" ~
            "upper secondary school",

          unit_type_name == "hemtjänst" ~
            "home care",

          unit_type_name == "särskilt boende" ~
            "special housing, elderly",

          unit_type_name ==
            "lss boende med särskild service" ~
            "LSS housing with special services",

          unit_type_name ==
            "lss daglig verksamhet" ~
            "LSS daily activity",

          unit_type_name ==
            "gruppbostad lss" ~
            "LSS group home",

          unit_type_name ==
            "servicebostad lss" ~
            "LSS service home",

          unit_type_name ==
            "sol boendestöd" ~
            "Social Services housing support",

          unit_type_name ==
            "sol boende med särskild service" ~
            "Social Services housing with special services",

          unit_type_name ==
            "sol sysselsättning" ~
            "Social Services employment",

          unit_type_name ==
            "våld i nära relationer" ~
            "domestic violence",

          unit_type_name ==
            "fastigheter" ~
            "properties",

          TRUE ~ as.character(
            unit_type_name
          )
        )
      )
  }


  rownames(resultat) <- NULL

  as.data.frame(
    resultat
  )
}

#' koladafun: Functions for Kolada
#'
#' `koladafun` is an R package with functions for searching,
#' exploring and retrieving data from Kolada.
#'
#' Paketet kan användas med både svenska och engelska funktionsnamn.
#'
#' @section Svenska funktioner:
#'
#' * [hamta_fran_kolada()] - Hämta data från Kolada.
#' * [sok_nyckeltal()] - Sök efter nyckeltal.
#' * [info_nyckeltal()] - Visa information om nyckeltal.
#' * [hamta_kommuner()] - Hämta kommuner och regioner.
#' * [tillgangliga_ar()] - Visa tillgängliga år.
#' * [senaste_varde()] - Hämta senaste tillgängliga värde.
#' * [forandring()] - Beräkna förändring mellan två år.
#' * [hamta_jamforelse()] - Jämför en kommun med andra kommuner.
#' * [hamta_enheter()] - Hämta enheter och enhetsdata.
#'
#' @section English functions:
#'
#' * [get_from_kolada()] - Get data from Kolada.
#' * [search_kpi()] - Search for KPIs.
#' * [kpi_info()] - Get KPI metadata.
#' * [get_municipalities()] - Get municipalities and regions.
#' * [available_years()] - Get available years.
#' * [latest_value()] - Get the latest available value.
#' * [change()] - Calculate change between two years.
#' * [get_comparison()] - Compare a municipality with other municipalities.
#' * [get_units()] - Get organizational units and unit-level data.
#'
#' @section More information:
#'
#' README and source code are available on GitHub:
#' \url{https://github.com/RKAkolada/koladafun}
#'
"_PACKAGE"
