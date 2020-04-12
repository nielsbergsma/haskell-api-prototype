{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Example.Api.Resources.User (
    user
 ) where

import Data.Maybe
import Data.Aeson
import Network.Api.Rest.Error
import Network.Api.Rest.Handlers
import Network.Api.Rest.Validate
import Network.Api.Rest.Routing

import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HML
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Network.Api.Rest.Validation.JsonValidator as JV
import qualified Network.Api.Rest.Validation.ParameterValidator as PV

-- Parameters (url & query)
data UserParameters = UserParameters 
  { userId :: T.Text
  , options :: Maybe T.Text 
  }

-- Transfer object
data UserTransferObject
  = UserSetName { firstName :: T.Text, lastName :: T.Text }
  | UserSetAge  { age :: Int, male :: Maybe Bool }
  deriving (Show, Eq)

readUsersParameters :: HttpRequest -> Either PV.ValidationFailure UserParameters
readUsersParameters r = do
  userIdP  <- PV.required "user_id" (pathParameter r 1) (PV.string [PV.minLength 3, PV.maxLength 6])
  optionsP <- PV.optional "options" (queryParameter r "options") (PV.string [PV.inStringEnum ["none", "all"]])
  return UserParameters 
    { userId  = PV.readText userIdP
    , options = PV.readMaybeText optionsP 
    }

validateUsersBody :: JV.Validator
validateUsersBody = JV.one 
  [ JV.object 
    [ JV.member "set_name" (JV.object 
      [ JV.member "first_name" (JV.string [JV.minLength 3])
      , JV.member "last_name" (JV.string [JV.minLength 3])
      ])
    ]
  , JV.object 
    [ JV.member "set_age" (JV.object 
      [ JV.member "age" (JV.integer [JV.min 1, JV.max 120])
      ])
    ]
  ]

instance FromJSON UserTransferObject where
  parseJSON (Object o) = case headMember o of
    (Just ("set_name", (Object sn))) -> 
      UserSetName <$> sn .: "first_name" <*> sn .: "lastname"

    (Just ("set_age", (Object sa))) -> 
      UserSetAge <$> sa .: "age" <*> sa .:? "male"

    _ -> fail "expected object"
    where headMember = listToMaybe . HML.toList

  parseJSON _ = fail "expected object"


-- Handler
user :: HttpHandler
user c r = validate3
  (hasRoleAny ["admin"] c r)
  (hasParameters readUsersParameters r)
  (hasBody validateUsersBody r :: IO (Either RequestBodyError UserTransferObject))
  \_ p b -> 
    httpOkRaw (BSL.fromString ("this is a user: " ++ T.unpack (userId p) ++ " " ++ (show b) ))

