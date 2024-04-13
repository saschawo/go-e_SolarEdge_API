library(httr)

# IP for go-e charging box
goe.ip <- "< insert your go-e IP here >"

# API credentials SolarEdge
api.key <- "< insert your API key here >"
site.id <- "< insert your site ID here >"

wait.time <- 60*3 # Interval between checks in seconds
no.api.calls <- 300 # Maximum number of API calls per day
# no.api.calls is just for display purposes. If you exceed the limit, you will get an error message from SolarEdge and
# the script will most likely fail (not tested yet). The hard limit is 300 calls per day.

while (T) {
  cat(paste0(
    "\n\n============================================\n",
    "Starting check at ", as.character(Sys.time()), "\n",
    "============================================\n"))
  # Getting current charging power from go-eCharger
  nrg.array <- GET(paste0("http://", goe.ip, "/api/status?filter=nrg"))
  total.charging <- content(nrg.array)$nrg[[12]] / 1000

  # Check: Is car charging?
  if (total.charging > .1) {
    cat("Car is currently charging with", round(total.charging, 2), "kW\n")

    # Get current power flow from SolarEdge
    resp <- GET(url = paste0("https://monitoringapi.solaredge.com/site/",
                             site.id, "/currentPowerFlow?",
                             "api_key=", api.key))
    cont <- content(resp)
    produced <- cont$siteCurrentPowerFlow$PV$currentPower # Currently produced
    to_house <- cont$siteCurrentPowerFlow$LOAD$currentPower # Currently used in house
    overflow <- produced - to_house # Calculate current overflow
    cat(paste0("Current PV overflow: ", round(produced, 2), " (prod.) - ",
               round(to_house, 2), " (used) = ", round(overflow, 2), " kW\n"))

    # Writing one symbol to API call file for current day to keep track of number of API calls
    write("x", file = paste0("API_calls_", format(Sys.Date(), "%Y-%m-%d"), ".txt"), append = T)
    # Read API call file and calculate number of API calls that are left for current day
    api.calls <- readLines(paste0("API_calls_", format(Sys.Date(), "%Y-%m-%d"), ".txt"))
    cat(paste0("API calls left for today: ", no.api.calls - length(api.calls), "\n"))

    # 'amp.df' holds all possible charging rates (in A) with corresponding power consumption (in kWh)
    # kWh are not precise here and might vary a bit.
    amp.df <- data.frame(amp = 6:16, power = c(1.05, 1.22, 1.34, 1.56, 1.75, 1.94, 2.12, 2.44, 2.68, 2.91, 3.35))
    amp.df$enough.for.charging <- amp.df$power <= total.charging + overflow

    # 6 A is the minimum charging rate. Even when overflow is negative, the car is charged with 6 A!
    possible.with.overflow <- amp.df[amp.df$enough.for.charging,]
    if (nrow(possible.with.overflow) == 0) {
      cat("Not enough power to charge car with more than 6 A. Charging with 6 A.\n")
      set <- GET(paste0("http://", goe.ip, "/api/set?amp=", 6))
      Sys.sleep(wait.time)
    } else {
      highest.possible.amp <- possible.with.overflow[nrow(possible.with.overflow),]$amp
      set <- GET(paste0("http://", goe.ip, "/api/set?amp=", highest.possible.amp))
      cat(paste0("Charging set to ", highest.possible.amp, " A.\n"))
      Sys.sleep(wait.time) }
  } else { # This happens when the car is not charging.
    cat("Car is not charging. Waiting...\n")
    Sys.sleep(wait.time)
  }
}
