# When served by tabler-server, TABLER_WORKER_PORT is set and this app must
# bind that assigned loopback port instead of tablerApp()'s own default port
# (which collides with tabler-server's own listening port, e.g. 3000).

# tabler_worker_port <- Sys.getenv("TABLER_WORKER_PORT", "")
tabler_worker_port <- 3000L

if (nzchar(tabler_worker_port)) {
  tradestatisticsdashboard::run_app(
    host = "127.0.0.1",
    port = as.integer(tabler_worker_port),
    launch.browser = FALSE
  )
} else {
  tradestatisticsdashboard::run_app()
}
