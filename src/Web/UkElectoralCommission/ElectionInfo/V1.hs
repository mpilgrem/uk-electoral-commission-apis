{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      : Web.UkElectoralCommission.ElectionInfo.V1
-- Description : Bindings to the UK's Electoral Commission's Election
--               Information API v1
-- Copyright   : (c) Mike Pilgrem 2024
-- Maintainer  : public@pilgrem.com
-- Stability   : experimental
--
-- This package has no connection to the UK's Electoral Commission or its
-- affiliates.
module Web.UkElectoralCommission.ElectionInfo.V1
  ( Manager
  , Token (..)
  , Postcode (..)
  , Uprn (..)
  , ElectionInfo (..)
  , ElectoralServices (..)
  , Email.EmailAddress
  , Contact (..)
  , PostcodeLocation (..)
  , ElectionInfoResponse (..)
  , Address (..)
  , URI (..)
  , ElectionDate (..)
  , Day (..)
  , PollingStation (..)
  , Station (..)
  , AdvanceVotingStation (..)
  , OpeningTime (..)
  , Notification (..)
  , NotificationType (..)
  , Ballot (..)
  , Object
  , CancellationReason (..)
  , VotingSystem (..)
  , VotingSlug (..)
  , Candidate (..)
  , Party (..)
  , Person (..)
  , VoterIdRequirement (..)
  , AddressPicker
  , getElectionInfo
  , stdAddressPicker
  , firstAddressPicker
  , noAddressPicker
  , withPostcode
  , withUprn
  ) where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( forM_ )
import           Data.Aeson.Types
                   ( FromJSON (..), Object, Value (..), (.:), (.:?), withArray
                   , withObject, withText
                   )
import           Data.Int ( Int64 )
import qualified Data.List as L
import           Data.List.NonEmpty ( NonEmpty (..) )
import           Data.Proxy ( Proxy (..) )
import qualified Data.Text as T
import           Data.Text ( Text )
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Text.IO as T
import           Data.Time.Calendar ( Day (..) )
import           Data.Time.LocalTime ( TimeOfDay (..) )
import qualified Data.Vector as V
import           GHC.Generics ( Generic )
import           Network.HTTP.Client ( Manager )
import           Network.URI ( URI (..) )
import           Servant.API
                   ( Capture, Get, JSON, QueryParam, ToHttpApiData, (:<|>) (..)
                   , (:>)
                   )
import           Servant.Client
                   ( BaseUrl (..), ClientEnv (..), ClientError, ClientM
                   , Scheme (..), client, defaultMakeClientRequest, runClientM
                   )
import           System.IO ( hFlush, stdout )
import qualified Text.Email.Validate as Email
import           Text.Read ( readMaybe )

-- | Type representing UK's Electoral Commissions' API tokens.
newtype Token = Token Text
  deriving (Eq, Show, ToHttpApiData)

-- | Type representing British postcode units.
newtype Postcode = Postcode Text
  deriving (Eq, FromJSON, Show, ToHttpApiData)

-- | Type representing Unique Property Reference Numbers (UPRN) in Great Britain
-- and Northern Ireland.
newtype Uprn = Uprn { unUprn :: Int64 }
  deriving (Eq, Show, ToHttpApiData)

-- | Accepts /string/ and /number/ JSON values. The former is used in the UK's
-- Electoral Commission's Election Information API v1.
instance FromJSON Uprn where

  parseJSON v = (Uprn <$> parseJSON v) <|> parseString v
   where
    parseString = withText "Uprn" $ \t -> do
      let s = T.unpack t
      case readMaybe s of
        Nothing -> fail $ "not an integer: " <> s
        Just x -> pure $ Uprn x

-- | Type representing election information returned by the UK's Electoral
-- Commission's Election Information API v1.
data ElectionInfo = ElectionInfo
  { election_dates :: ![ElectionDate]
    -- ^ An array containing details of relevant ballots, candidates and polling
    -- station information.
  , electoral_services :: !(Maybe ElectoralServices)
    -- ^ Contact details for the user's local Electoral Services team. If the
    -- UK's Electoral Commission does not know the user's polling station, this
    -- can be used to provide contact info for their local council. This may be
    -- 'Nothing' if it is not able to determine the user's council.
  , registration :: !(Maybe Contact)
    -- ^ Sometimes the contact information for registration and proxy voting is
    -- different to the electoral services contact details. Use these if they
    -- exist and your users might have questions about voter registration. If
    -- 'Nothing', assume the electoral services contact details can be used for
    -- electoral registration related enquiries.
  , postcode_location :: !(Maybe PostcodeLocation)
    -- ^ A point describing the centroid of the input postcode. If providing a
    -- map or directions for a polling station journey, use this as the start
    -- point. This may be 'Nothing' if the UK's Electoral Commission is not able
    -- to accurately geocode a location.
  } deriving (Eq, Generic, Show)

instance FromJSON ElectionInfo where

  parseJSON = withObject "ElectionInfo" $ \o -> do
    election_dates <- o .: "dates"
    electoral_services <- o .: "electoral_services"
    registration <- o .: "registration"
    postcode_location <- o .: "postcode_location"
    pure ElectionInfo {..}

-- | Type representing information about electoral services.
data ElectoralServices = ElectoralServices
  { council_id :: !Text
    -- ^ GSS code for this council.
  , council_name :: !Text
    -- ^ Name of this council.
  , council_nation :: !Text
    -- ^ Name of nation.
  , council_contact :: !Contact
  } deriving (Eq, Generic, Show)

instance FromJSON ElectoralServices where

  parseJSON = withObject "ElectoralServices" $ \o -> do
    council_id <- o .: "council_id"
    council_name <- o .: "name"
    council_nation <- o .: "nation"
    council_address <- o .: "address"
    council_postcode <- o .: "postcode"
    council_email' <- o .: "email"
    council_phone  <- o .: "phone"
    council_website <- o .: "website"
    let council_email = unEmailAddress council_email'
        council_contact = Contact {..}
    pure ElectoralServices {..}

-- Using newtype to avoid an orphan instance.
newtype EmailAddress = EmailAddress { unEmailAddress :: Email.EmailAddress }

instance FromJSON EmailAddress where

  parseJSON = withText "EmailAddress" $ \t -> do
    case Email.emailAddress (encodeUtf8 t) of
      Nothing ->
        fail $ "'" <> T.unpack t <> "' not recognised as an email address"
      Just emailAddress -> pure $ EmailAddress emailAddress

-- | Type representing information about council contacts.
data Contact = Contact
  { council_address :: !Text
    -- ^ Contact address for this council.
  , council_postcode :: !Postcode
    -- ^ Postcode component of contact address for this council.
  , council_email :: !Email.EmailAddress
    -- ^ Contact email address for this council's Electoral Services team.
  , council_phone :: !Text
    -- ^ Telephone number for this council's Electoral Services team.
  , council_website :: !URI
    -- ^ URL for this council's website.
  } deriving (Eq, Generic, Show)

instance FromJSON Contact where

  parseJSON = withObject "Contact" $ \o -> do
    council_address <- o .: "address"
    council_postcode <- o .: "postcode"
    council_email' <- o .: "email"
    council_phone  <- o .: "phone"
    council_website <- o .: "website"
    let council_email = unEmailAddress council_email'
    pure Contact {..}

-- | Type representing centroids of postcodes.
data PostcodeLocation = PostcodeLocation
  { pcloc_x :: !Double
  , pcloc_y :: !Double
  } deriving (Eq, Generic, Show)

instance FromJSON PostcodeLocation where

  parseJSON = withObject "PostcodeLocation" $ \o -> do
    pcloc_type <- o .: "type"
    if pcloc_type == ("Feature" :: Text)
      then do
        pcloc_geometry <- o .: "geometry"
        geo_type <- pcloc_geometry .: "type"
        if geo_type == ("Point" :: Text)
          then do
            (pcloc_x, pcloc_y) <- pcloc_geometry .: "coordinates"
            pure PostcodeLocation {..}
          else fail "expected a Point geometry"
      else fail "expected a GeoJSON Feature"

-- | Type representing responses from the UK's Electoral Commission's Election
-- Information API v1.
data ElectionInfoResponse = ElectionInfoResponse
  { address_picker :: !Bool
    -- ^ True if we need to show this user an address picker.
  , addresses :: ![Address]
    -- ^ An array containing the addresses applicable to this request (if
    -- necessary).
  , election_info :: !ElectionInfo
  } deriving (Eq, Generic, Show)

instance FromJSON ElectionInfoResponse where

  parseJSON = withObject "ElectionInfoResponse" $ \o -> do
    address_picker <- o .: "address_picker"
    addresses <- o .: "addresses"
    election_info <- parseJSON (Object o)
    pure ElectionInfoResponse {..}

-- | Type representing addresses.
data Address = Address
  { address :: !Text
  , postcode :: !Postcode
  , uprn :: !Uprn
  , address_url :: !URI
    -- ^ Call this URL to get data for this registered address.
  } deriving (Eq, Generic, Show)

instance FromJSON Address where

  parseJSON = withObject "Address" $ \o -> do
    address <- o .: "address"
    postcode <- o .: "postcode"
    uprn <- o .: "slug"
    address_url <- o .: "url"
    pure Address {..}

-- | Type representing dates of electoral events.
data ElectionDate = ElectionDate
  { election_date :: !Day
  , polling_station :: !PollingStation
  , advance_voting_station :: !(Maybe AdvanceVotingStation)
  , notifications :: ![Notification]
  , ballots :: ![Ballot]
  } deriving (Eq, Generic, Show)

instance FromJSON ElectionDate where

  parseJSON = withObject "ElectionDate" $ \o -> do
    election_date <- o .: "date"
    polling_station <- o .: "polling_station"
    advance_voting_station <- o .: "advance_voting_station"
    notifications <- o .: "notifications"
    ballots <- o .: "ballots"
    pure ElectionDate {..}

-- | Type representing information about polling stations.
data PollingStation = PollingStation
  { polling_station_known :: !Bool
  , custom_finder :: !(Maybe URI)
    -- ^ If the UK's Electoral Commission does not know a user's polling
    -- station, sometimes it can provide the URL of another polling station
    -- finder. This will always be populated for users in Northern Ireland where
    -- Electoral Office for Northern Ireland run their own service.
  , report_problem_url :: !(Maybe URI)
    -- ^ If the UK's Electoral Commission provides a polling station result,
    -- this URL may be used to provide a user with a back-channel to report
    -- inaccurate data.
  , station :: !(Maybe Station)
    -- ^ A point describing the location of this polling station. Optionally
    -- 'Nothing' if the UK's Electoral Commission knows the address only but
    -- can't geocode a location.
  } deriving (Eq, Generic, Show)

instance FromJSON PollingStation

-- | Type representing locations of polling stations.
data Station = Station
  { station_id :: !Text
  , station_address :: !Text
  , station_postcode :: !Postcode
  , station_x :: !Double
  , station_y :: !Double
  } deriving (Eq, Generic, Show)

instance FromJSON Station where

  parseJSON = withObject "Station" $ \o -> do
    station_type <- o .: "type"
    if station_type == ("Feature" :: Text)
      then do
        station_id <- o .: "id"
        station_geometry <- o .: "geometry"
        geo_type <- station_geometry .: "type"
        if geo_type == ("Point" :: Text)
          then do
            (station_x, station_y) <- station_geometry .: "coordinates"
            station_properties <- o .: "properties"
            station_address <- station_properties .: "address"
            station_postcode <- station_properties .: "postcode"
            pure Station {..}
          else fail "expected a Point geometry"
      else fail "expected a GeoJSON Feature"

-- | Type representing advance voting stations.
data AdvanceVotingStation = AdvanceVotingStation
  { avs_name :: !Text
  , avs_address :: !Text
  , avs_postcode :: !Postcode
  , avs_x :: !Double
  , avs_y :: !Double
  , avs_opening_times :: ![OpeningTime]
  } deriving (Eq, Generic, Show)

instance FromJSON AdvanceVotingStation where

  parseJSON = withObject "AdvanceVotingStation" $ \o -> do
    avs_name <- o .: "name"
    avs_address <- o .: "address"
    avs_postcode <- o .: "postcode"
    avs_location <- o .: "location"
    geo_type <- avs_location .: "type"
    (avs_x, avs_y) <- if geo_type == ("Point" :: Text)
      then avs_location .: "coordinates"
      else fail "expected a Point geometry"
    avs_opening_times <- o .: "opening_times"
    pure AdvanceVotingStation {..}

-- | Type representing opening dates and times of advance voting stations.
data OpeningTime = OpeningTime
  { ot_day :: !Day
  , ot_open :: !TimeOfDay
  , ot_close :: !TimeOfDay
  } deriving (Eq, Generic, Show)

instance FromJSON OpeningTime where

  parseJSON = withArray "OpeningTime" $ \a -> do
    ot_day <- parseJSON (a V.! 0)
    ot_open <- parseJSON (a V.! 1)
    ot_close <- parseJSON (a V.! 2)
    pure OpeningTime {..}

-- | Type representing notifications by the UK's Electoral Commission.
data Notification = Notification
  { notify_type :: !NotificationType
  , notify_title :: !Text
  , notify_detail :: !Text
  , notify_url :: !URI
  } deriving (Eq, Generic, Show)

instance FromJSON Notification where

  parseJSON = withObject "Notification" $ \o -> do
    notify_type <- o .: "type"
    notify_title <- o .: "title"
    notify_detail <- o .: "detail"
    notify_url <- o .: "url"
    pure Notification {..}

-- | Type representing types of notification.
data NotificationType
  = CancelledElection
  | VoterId
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON NotificationType where

  parseJSON = withText "NotificationType" $ \t -> do
    case t of
      "cancelled_election" -> pure CancelledElection
      "voter_id" -> pure VoterId
      _ -> fail $ "unrecognised notification type: " <> T.unpack t

-- | Type representing ballots.
data Ballot = Ballot
  { ballot_paper_id :: !Text
  , ballot_title :: !Text
  , ballot_url :: !(Maybe URI)
  , poll_open_date :: !Day
  , elected_role :: !Text
  , metadata :: !(Maybe Object)
    -- ^ Object containing information about special conditions for the user to
    -- be aware about (e.g: cancelled elections, voter id pilot). (details TBC)
  , cancelled :: !Bool
  , cancellation_reason :: !(Maybe CancellationReason)
    -- ^ If a ballot has been cancelled, this key may hold a codified reason for
    -- that ballot's cancellation.
  , replaced_by :: !(Maybe Text)
    -- ^ If a ballot has been cancelled (cancelled = true) and rescheduled for a
    -- later date, this key will hold the ballot_paper_id of the ballot that
    -- replaces it.
  , replaces :: !(Maybe Text)
    -- ^ If this ballot replaces another cancelled ballot, this key will hold
    -- the ballot_paper_id of the ballot that it replaces.
  , election_id :: !Text
    -- ^ Identifier for this ballot's parent election group.
  , election_name :: !Text
  , post_name :: !Text
  , candidates_verified :: !Bool
    -- ^ True if the list of candidates for this election has been confirmed
    -- against the nomination papers for this ballot. If this property is False,
    -- the candidate list is provisional or unconfirmed.
  , voting_system :: !VotingSystem
  , seats_contested :: !Int
  , candidates :: ![Candidate]
    -- ^ Array describing candidates that will appear on this ballot paper. In
    -- an election which uses party lists, the `candidates` array is sorted by
    -- party and `list_position` within parties. For other election types it is
    -- sorted alphabetically by candidate name.
  , wcivf_url :: !(Maybe URI)
    -- ^ Link for more (human-readable) information about this ballot.
  , requires_voted_id :: !(Maybe VoterIdRequirement)
  } deriving (Eq, Generic, Show)

instance FromJSON Ballot

-- | Type representing cancellation reasons.
data CancellationReason
  = NoCandidates
  | EqualCandidates
  | UnderContested
  | CandidateDeath
  | NullReason
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON CancellationReason where

  parseJSON = withText "CancellationReason" $ \t -> case t of
    "NO_CANDIDATES" -> pure NoCandidates
    "EQUAL_CANDIDATES" -> pure EqualCandidates
    "UNDER_CONTESTED" -> pure UnderContested
    "CANDIDATE_DEATH" -> pure CandidateDeath
    "null" -> pure NullReason
    _ -> fail $ "unrecognised cancellation reason: " <> T.unpack t

-- | Type representing voting systems.
data VotingSystem = VotingSystem
  { vs_slug :: !VotingSlug
  , vs_name :: !Text
    -- ^ The name of this voting system (e.g. \"First-past-the-post\").
  , uses_party_lists :: !Bool
  } deriving (Eq, Generic, Show)

instance FromJSON VotingSystem where

  parseJSON = withObject "VotingSystem" $ \o -> do
    vs_slug <- o .: "slug"
    vs_name <- o .: "name"
    uses_party_lists <- o .: "uses_party_lists"
    pure VotingSystem {..}

-- | Type representing \'slugs\' for voting systems.
data VotingSlug
  = Ams
    -- ^ Additional Member System
  | Fptp
    -- ^ First-Past-The-Post.
  | PrCl
    -- ^ Proportional representation, closed list.
  | Sv
    -- ^ Supplementary Vote
  | Stv
    -- ^ Single Transferable Vote
  deriving (Eq, Generic, Show)

instance FromJSON VotingSlug where

  parseJSON = withText "VotingSlug" $ \t -> case t of
    "AMS" -> pure Ams
    "FPTP" -> pure Fptp
    "PR-CL" -> pure PrCl
    "sv" -> pure Sv
    "STV" -> pure Stv
    _ -> fail $ "unrecognised voting slug: " <> T.unpack t

-- | Type representing candidates.
data Candidate = Candidate
  { list_position :: !(Maybe Int)
    -- ^ Numeric position in party list. This value is only relevant to
    -- elections using party lists. It will always be null in
    -- First-Past-The-Post elections.
  , party :: !Party
  , person :: !Person

  } deriving (Eq, Generic, Show)

instance FromJSON Candidate

-- | Type representing parties.
data Party = Party
  { party_id :: !Text
  , party_name :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON Party

-- | Type representing people standing in elections.
data Person = Person
  { ynr_id :: !Int
  , person_name :: !Text
  , absolute_url :: !(Maybe URI)
    -- ^ Link for more (human-readable) information about this candidate.
  , person_email :: !(Maybe Email.EmailAddress)
    -- ^ Email address for this candidate, if the UK's Electoral Commission
    -- holds it.
  , photo_url :: !(Maybe URI)
    -- ^ URL for a photo of this candidate, if the UK's Electoral Commission
    -- holds one.
  } deriving (Eq, Generic, Show)

instance FromJSON Person where

  parseJSON = withObject "Person" $ \o -> do
    ynr_id <- o .: "ynr_id"
    person_name <- o .: "name"
    absolute_url <- o .:? "absolute_url"
    person_email' <- o .:? "email"
    photo_url <- o .:? "photo_url"
    let person_email = unEmailAddress <$> person_email'
    pure Person {..}

-- | Type representing British voter ID requirements.
data VoterIdRequirement
  = EFA2002
    -- ^ ID required under the UK's Electoral Fraud (Northern Ireland) Act 2002.
  | EA2022
    -- ^ ID required under the UK's Elections Act 2022.
  deriving (Eq, Generic, Show)

instance FromJSON VoterIdRequirement where

  parseJSON = withText "VoterIdRequirement" $ \t -> case t of
    "EFA-2002" -> pure EFA2002
    "EA-2022" -> pure EA2022
    _ -> fail $ "unrecognised voter ID requirement: " <> T.unpack t

-- | Type synonym representing the UK's Electoral Commission's Election
-- Information API v1.
type ElectionInfoApi
  =    "v1"
  :>   "postcode"
  :>   Capture "postcode" Postcode
  :>   QueryParam "token" Token
  :>   Get '[JSON] ElectionInfoResponse
  :<|> "v1"
  :>   "address"
  :>   Capture "slug" Uprn
  :>   QueryParam "token" Token
  :>   Get '[JSON] ElectionInfoResponse

-- | Type synonym representing an address picker.
type AddressPicker = Address -> NonEmpty Address -> IO (Maybe Address)

ukElectoralCommissionApis :: BaseUrl
ukElectoralCommissionApis =
  BaseUrl Https "api.electoralcommission.org.uk" 443 "/api"

api :: Proxy ElectionInfoApi
api = Proxy

postcode' :: Postcode -> Maybe Token -> ClientM ElectionInfoResponse

address' :: Uprn -> Maybe Token -> ClientM ElectionInfoResponse

postcode' :<|> address' = client api

-- | Function to access the UK's Electoral Commission's Election Information API
-- v1 @postcode@ endpoint.
withPostcode ::
     Manager
  -> Token
  -> Postcode
  -> IO (Either ClientError ElectionInfoResponse)
withPostcode mgr token postcode = runClientM
  (postcode' postcode (Just token))
#if MIN_VERSION_servant_client(0,17,0)
  (ClientEnv mgr ukElectoralCommissionApis Nothing defaultMakeClientRequest)
-- CookieJar supported from servant-client-0.13
#elif MIN_VERSION_servant_client(0,13,0)
  (ClientEnv mgr ukElectoralCommissionApis Nothing)
#else
  (ClientEnv mgr ukElectoralCommissionApis)
#endif

-- | Function to access the UK's Electoral Commission's Election Information API
-- v1 @address@ endpoint.
withUprn ::
     Manager
  -> Token
  -> Uprn
  -> IO (Either ClientError ElectionInfoResponse)
withUprn mgr token uprn = runClientM
  (address' uprn (Just token))
#if MIN_VERSION_servant_client(0,17,0)
  (ClientEnv mgr ukElectoralCommissionApis Nothing defaultMakeClientRequest)
-- CookieJar supported from servant-client-0.13
#elif MIN_VERSION_servant_client(0,13,0)
  (ClientEnv mgr ukElectoralCommissionApis Nothing)
#else
  (ClientEnv mgr ukElectoralCommissionApis)
#endif

-- | Function to access election information using the UK's Electoral
-- Commission's Election Information API v1.
getElectionInfo ::
     Manager
  -> Token
  -> AddressPicker
  -> Postcode
  -> IO (Maybe (Either ClientError ElectionInfo))
getElectionInfo mgr token addressPicker' postcode = do
  withPostcode mgr token postcode >>= \case
    Left err -> pure $ Just $ Left err
    Right postcodeResponse -> if address_picker postcodeResponse
      then do
        mAddress <- addressPicker addressPicker' (addresses postcodeResponse)
        case mAddress of
          Nothing -> pure Nothing
          Just address -> do
            withUprn mgr token (uprn address) >>= \case
              Left err -> pure $ Just $ Left err
              Right slugResponse ->
                pure $ Just $ Right (election_info slugResponse)
      else pure $ Just $ Right $ election_info postcodeResponse

-- | An address picker that uses the standard input and output channels.
stdAddressPicker :: AddressPicker
stdAddressPicker a1 (a2 :| rest) = do
  let Postcode postcode'' = postcode a1
      as = a1:a2:rest
      n = 2 + L.length rest
  T.putStrLn $ "Addresses for " <> postcode'' <> ":"
  forM_ (L.zip [1 :: Int ..] as) $ \(i, a) -> do
    T.putStrLn $ T.pack (show i) <> ". " <> displayUprn (uprn a) <> "\n" <>
      address a
  i <- pickAddress n
  pure $ Just $ as !! (i - 1)
 where
  displayUprn uprn = "(UPRN " <> T.pack (show $ unUprn uprn) <> ")"
  pickAddress n = do
    T.putStr $ "Choose address 1 to " <> T.pack (show n) <> ": "
    hFlush stdout
    line <- T.getLine
    case readMaybe (T.unpack line) of
      Nothing -> invalid line
      Just i -> if i > 0 && i <= n
        then pure i
        else invalid line
   where
    invalid line = do
      T.putStrLn $ "Response '" <> line <> "' is invalid."
      pickAddress n

-- | An address picker that picks the first address, if it is available.
firstAddressPicker :: AddressPicker
firstAddressPicker a _ = pure $ Just a

-- | An address picker that never picks an address.
noAddressPicker :: AddressPicker
noAddressPicker _ _ = pure Nothing

addressPicker :: AddressPicker -> [Address] -> IO (Maybe Address)
addressPicker _ [] = pure Nothing
addressPicker _ [a] = pure $ Just a
addressPicker f (a1:a2:rest) = f a1 (a2 :| rest)
