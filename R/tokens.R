# nocov start

get_mapbox_token <- function ()
{
    e <- Sys.getenv()
    e <- e [grep ("mapbox|mapscan", names (e), ignore.case = TRUE)]
    tok <- unique (as.character (e))
    if (all (tok == ""))
        stop0 ()
    else if (length (tok) > 1)
    {
        e <- e [grep ("mapscan", names (e), ignore.case = TRUE)]
        tok <- unique (as.character (e))
        if (length (tok) == 0)
            stop0 ()
        else if (length (tok) > 1)
            stop ("Found multiple potential tokens named [",
                  paste0 (names (e), collapse = ","), "];\nplease specify ",
                  "only one environnmental variable which includes the ",
                  "name\n'mapscan', and contains a personal API key for ",
                  "mapbox services.")
    }
    return (tok)
}

stop0 <- function ()
{
    stop ("Map generation requires a mapbox API key to be set with ",
          "Sys.setenv\nor the package's 'set_mapbox_token' function, ",
          "using a token name that\nincludes either the strings ",
          "'mapbox' or 'mapscanner'. Tokens can be obtained\nfrom ",
          "https://docs.mapbox.com/api/overview/",
          call. = FALSE)
}

#' Set 'mapbox' token
#'
#' Set a mapbox token for use with the \link{ms_generate_map} function.
#'
#' @param token Personal mapbox API token, obtained from
#' \url{https://docs.mapbox.com/api/#access-tokens-and-token-scopes}.
#' @return `TRUE` if the token was able to be set; otherwise `FALSE`.
#' @export
set_mapbox_token <- function (token)
{
    chk <- Sys.setenv ("mapscanner" = token)
    if (chk)
        message ("Token successfully set")
    else
        warning ("Unable to set token")
}

# nocov end
