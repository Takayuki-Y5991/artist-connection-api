(ns artist-connections.port.outbound.client.auth-client)

(defprotocol AuthClientPort
  "Protocol for the auth client port."

  (request-redirect-uri [this params]
    "Requests a redirect URI.")

  (callback [this params]
    "Handles a callback."))