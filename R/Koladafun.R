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
#'   `"K"` = kommun och `"R"` = region.
#'   Flera kan anges med `c("K", "R")`.
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
  kommun_svar <- jsonlite::fromJSON(
    "https://api.kolada.se/v3/municipality?page=1&per_page=5000"
  )

  kommuner <- as.data.frame(kommun_svar$values)

  kommuner$id <- as.character(kommuner$id)
  kommuner$title <- as.character(kommuner$title)
  kommuner$type <- as.character(kommuner$type)

  # Filtrera på kommuntyp
  if (!is.null(kommuntyp)) {

    kommuntyp <- toupper(as.character(kommuntyp))

    ogiltiga_typer <- setdiff(
      kommuntyp,
      c("K", "R")
    )

    if (length(ogiltiga_typer) > 0) {
      stop(
        "Ogiltig kommuntyp: ",
        paste(ogiltiga_typer, collapse = ", "),
        ". Använd K för kommun eller R för region."
      )
    }

    api_typ <- ifelse(
      kommuntyp == "R",
      "L",
      kommuntyp
    )

    kommuner <- kommuner[
      kommuner$type %in% api_typ,
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

      svar <- jsonlite::fromJSON(
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

      url <- paste0(
        "https://api.kolada.se/v3/kpi/",
        kpi_id
      )

      svar <- jsonlite::fromJSON(url)

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

      data.frame(
        kpi = kpi_id,
        kpi_name = svar$values$title[1],
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
