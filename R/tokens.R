get_mapbox_token <- function ()
{
    e <- Sys.getenv()
    e <- e [grep ("mapbox", names (e), ignore.case = TRUE)]
    tok <- unique (as.character (e))
    if (length (tok) != 1)
    {
        tok <- Sys.getenv ("mapscanner")
        if (tok == "")
            stop ("Map generation requires a mapbox API key to be set with ",
                  "Sys.setenv with a name that includes the string 'mapbox'")
    }
    return (tok)
}

#' set_mapbox_token
#'
#' Set a mapbox token for use with the \link{ms_generate_map} function.
#'
#' @param token Personal mapbox API token, obtained from
#' \url{https://docs.mapbox.com/api/#access-tokens-and-token-scopes}.
#' @return `TRUE` if the token was able to be set; otherwise `FALSE`.
#' @export
set_mapbox_token <- function (token)
{
    Sys.setenv ("mapscanner" = token)
}
