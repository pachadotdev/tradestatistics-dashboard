#' @title The application server-side
#' @param input,output,session Internal parameters for Shiny. DO NOT REMOVE.
app_server <- function(input, output, session) {
  # Single shared SQL connection for the whole session ----
  con <- open_con()

  session$onSessionEnded(function() close_con(con))

  # Safety net: onSessionEnded() only fires on a normal session/websocket
  # disconnect. If the R process itself is quit (e.g. Ctrl+C out of run_app()
  # then q()) without that event ever firing, the connection is instead swept
  # up by R's final garbage collection while still open, which produces a
  # warning on exit. The finalizer is registered on `con` itself (not on
  # environment(), which has no other strong referents and would become
  # collectible - and thus close the connection mid-session - as soon as
  # app_server() returns): the module server closures below hold a real,
  # strong reference to `con` for as long as they're alive, so this only
  # fires once nothing is using the connection anymore or R exits.
  #
  # reg.finalizer() requires its target to be an environment or an external
  # pointer - a DBIConnection is an S4 object, not either of those, so we
  # target con@ptr (the external pointer slot embedded in `con`) instead.
  # That slot stays reachable for exactly as long as `con` itself does (it's
  # part of `con`'s own data), so this preserves the same lifetime guarantee.
  reg.finalizer(con@ptr, function(e) {
    if (dbIsValid(con)) dbDisconnect(con)
  }, onexit = TRUE)

  # Modules ----
  mod_countries_server("co", con)
  mod_sectors_server("se", con)
  mod_cite_server("cite")

  # URL sync — action buttons are always excluded automatically
  syncUrl(session, exclude = c(
    "co-fmt", "se-fmt", "co-go", "se-go"
  ))
}
