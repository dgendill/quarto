module MyRTC (
    connection,
    basicOffer,
    createAnswerString,
    createOfferString,
    readDescription
  ) where

import Prelude
import Control.Monad.Aff.Console as Affc
import Control.Monad.Aff (Aff, forkAff, later', launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (decodeJson, encodeJson, jsonParser)
import Data.Argonaut.Core (stringify)
import Data.Either (Either)
import Data.NonEmpty (NonEmpty, singleton)
import WebRTC.RTC (Ice, RTC, RTCPeerConnection, RTCSessionDescription, ServerType(STUN), createAnswer, createDataChannel, createOffer, newRTCPeerConnection, noMediaRTCOffer, ondataChannel, onmessageChannelOnce, rtcSessionDescriptionSdp, send, setLocalDescription, setRemoteDescription)

url :: String -> { urls :: NonEmpty Array String }
url u = { urls : singleton u }

configuration :: Ice
configuration = {
  iceServers : [
    STUN $ url "stun:stun.l.google.com:3478"
    -- TURN {
    --   urls : singleton "turn:numb.viagenie.ca:3478",
    --   username : Just "",
    --   credential : Just "",
    --   credentialType : Nothing }
  ]
}

connection :: forall e. Eff (rtc :: RTC | e) RTCPeerConnection
connection = newRTCPeerConnection configuration

basicOffer :: forall e. RTCPeerConnection -> Aff (rtc :: RTC | e) RTCSessionDescription
basicOffer conn = createOffer noMediaRTCOffer conn

createOfferString :: forall e. RTCSessionDescription -> Aff (rtc :: RTC | e) String
createOfferString offer = do
  pure $ (encodeJson >>> stringify) offer

createAnswerString :: forall e. RTCSessionDescription -> Aff (rtc :: RTC | e) String
createAnswerString answer = do
  pure $ (encodeJson >>> stringify) answer

readDescription :: forall e. String -> Aff (rtc :: RTC | e) (Either String RTCSessionDescription)
readDescription s = pure $ (jsonParser s) >>= decodeJson

main :: forall e. Eff ( rtc :: RTC, err :: EXCEPTION, console :: CONSOLE | e ) Unit
main = void $ launchAff $ do

  Affc.log "Created local peer connection object p1"
  p1 <- liftEff $ connection

  Affc.log "Created remote peer connection object p2"
  p2 <- liftEff $ connection

  forkAff $ do
    p1Data <- createDataChannel "test" p1
    p2Data <- ondataChannel p2
    Affc.log "Data channels 1 and 2 are open"
    later' 2000 do
      Affc.log "Sending hellos."
      d1 <- liftAff $ onmessageChannelOnce p1Data
      d2 <- liftAff $ onmessageChannelOnce p2Data
      liftEff $ send "Hello p1" p1Data
      liftEff $ send "Hello p2" p2Data

  Affc.log "p1 createOffer start"
  offer <- createOffer noMediaRTCOffer p1

  Affc.log "Offer from pc1"
  Affc.log $ rtcSessionDescriptionSdp offer

  Affc.log "p1 setLocalDescription start."
  setLocalDescription offer p1

  -- Signaling here so p2 can get offer...

  Affc.log "p2 setRemoteDescription start."
  setRemoteDescription offer p2

  Affc.log $ "p2 createAnswer start."
  answer <- createAnswer p2

  Affc.log "Answer from p2"
  Affc.log $ rtcSessionDescriptionSdp answer

  Affc.log "p2 setLocalDescription start."
  setLocalDescription answer p2

  -- Signaling here so p1 can get answer...

  Affc.log "p1 setRemoteDescription start"
  setRemoteDescription answer p1

  pure unit
