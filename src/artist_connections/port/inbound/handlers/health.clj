(ns artist-connections.port.inbound.handlers.health)

(defprotocol HealthPort
  "Protocol for the health port."

  (health-check [this _]
    "Checks the health of the service."))