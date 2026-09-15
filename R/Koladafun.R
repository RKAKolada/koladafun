#' Hämta data från Kolada
#'
#' Hämtar data från Koladas API för ett eller flera nyckeltal,
#' kommuner och år. Det går även att filtrera resultatet efter kön.
#'
#' @param nyckeltal Ett eller flera nyckeltals-ID från Kolada,
#'   exempelvis `"N01926"` eller `c("N01926", "N17454")`.
#' @param kommun En eller flera kommunkoder,
#'   exempelvis `"0180"` eller `c("0180", "0136")`.
#' @param ar Ett eller flera år,
#'   exempelvis `2025` eller `2020:2025`.
#' @param kon Valfritt filter för kön. Ange `"T"` för total,
#'   `"K"` för kvinnor, `"M"` för män eller exempelvis
#'   `c("K", "M")` för båda könen. Standard är `NULL`,
#'   vilket innebär att alla tillgängliga kön returneras.
#' @param page Sidnummer i Koladas API. Standard är 1.
#' @param per_page Antal observationer per sida. Standard är 5000.
#'
#' @return En data.frame med kommun, kommunnamn, nyckeltals-ID,
#'   nyckeltalsnamn, år, värde, kön och status.
#'
#' @examples
#' \dontrun{
#' hamta_fran_kolada(
#'   nyckeltal = "N01926",
#'   kommun = "0180",
#'   ar = 2025,
#'   kon = "T"
#' )
#'
#' hamta_fran_kolada(
#'   nyckeltal = c("N01926", "N17454"),
#'   kommun = c("0180", "0136"),
#'   ar = 2024:2025,
#'   kon = c("K", "M")
#' )
#' }
#'
#' @export
hamta_fran_kolada <- function(
    nyckeltal,
    kommun,
    ar,
    kon = NULL,
    page = 1,
    per_page = 5000
) {

  kombinationer <- expand.grid(
    nyckeltal = nyckeltal,
    kommun = kommun,
    ar = ar,
    stringsAsFactors = FALSE
  )

  resultat <- lapply(seq_len(nrow(kombinationer)), function(i) {

    kpi_id <- kombinationer$nyckeltal[i]
    kommun_id <- kombinationer$kommun[i]
    ar_id <- kombinationer$ar[i]

    # Hämta själva datan
    url <- paste0(
      "https://api.kolada.se/v3/data/kpi/",
      kpi_id,
      "/municipality/",
      kommun_id,
      "/year/",
      ar_id,
      "?page=",
      page,
      "&per_page=",
      per_page
    )

    svar <- jsonlite::fromJSON(url)

    # Hämta nyckeltalsnamn
    kpi_url <- paste0(
      "https://api.kolada.se/v3/kpi/",
      kpi_id
    )

    kpi_svar <- jsonlite::fromJSON(kpi_url)
    kpi_name <- kpi_svar$values$title

    # Hämta kommunnamn
    kommun_url <- paste0(
      "https://api.kolada.se/v3/municipality/",
      kommun_id
    )

    kommun_svar <- jsonlite::fromJSON(kommun_url)
    municipality_name <- kommun_svar$values$title

    # Skapa data.frame
    data <- data.frame(svar$values) |>
      tidyr::unnest(values) |>
      dplyr::mutate(
        municipality_name = municipality_name,
        kpi_name = kpi_name
      ) |>
      dplyr::select(
        municipality,
        municipality_name,
        kpi,
        kpi_name,
        period,
        value,
        gender,
        status
      )

    # Filtrera på kön
    if (!is.null(kon)) {
      data <- data |>
        dplyr::filter(gender %in% kon)
    }

    data
  })

  data <- dplyr::bind_rows(resultat)

  return(data)
}

