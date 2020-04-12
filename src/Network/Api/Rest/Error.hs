{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Error (
    mapToError,

    CommonError (..), RequestBodyError (..), AuthorizationError (..),

    HttpErrorable
    ) where 

import Data.Aeson 
import Network.Api.Rest.Handlers

import qualified Data.Text as T
import qualified Network.Api.Rest.Validation.JsonValidator as JV
import qualified Network.Api.Rest.Validation.ParameterValidator as PV

class HttpErrorable a where
  mapToError :: a -> IO HttpResponse

-- Common
data CommonError = CommonError T.Text T.Text

instance ToJSON CommonError where
  toJSON (CommonError c k) = object 
    [ "error" .= object 
      [ "context" .= c
      , "kind"    .= k
      ]
    ]

-- Parameter 
instance HttpErrorable PV.ValidationFailure where
  mapToError (vf) = httpBadRequest $ toJSON $ object 
    [ "error" .= object 
      [ "context"   .= ("parameters" :: T.Text)
      , "parameter" .= parameter
      , "kind"      .= kind
      ]
    ]
    where parameter = fst $ parameter_kind vf
          kind      = snd $ parameter_kind vf
          parameter_kind (PV.ExpectedBool p)       = (p, "expected_bool" :: T.Text)
          parameter_kind (PV.ExpectedInteger p)    = (p, "expected_integer" :: T.Text)
          parameter_kind (PV.ExpectedNumber p)     = (p, "expected_number" :: T.Text)
          parameter_kind (PV.ExpectedString p)     = (p, "expected_string" :: T.Text)
          parameter_kind (PV.ExpectedExactOne p)   = (p, "expected_exact_one" :: T.Text)
          parameter_kind (PV.ExpectedPresent p)    = (p, "expected_present" :: T.Text)
          parameter_kind (PV.ValueTooShort p)      = (p, "value_too_short" :: T.Text)
          parameter_kind (PV.ValueTooLong p)       = (p, "value_too_long" :: T.Text)
          parameter_kind (PV.ValueTooSmall p)      = (p, "value_too_small" :: T.Text)
          parameter_kind (PV.ValueTooLarge p)      = (p, "value_too_large" :: T.Text)
          parameter_kind (PV.ValueNotAllowed p)    = (p, "value_not_allowed" :: T.Text)
          parameter_kind (PV.DoesntMatchPattern p) = (p, "doesnt_match_pattern" :: T.Text)
          parameter_kind (PV.NoMatchingAny p)      = (p, "no_matching_any" :: T.Text)

-- Request body
data RequestBodyError = UnsupportedMediaType | CorruptBody | InvalidBody JV.ValidationFailure | UnmarshableBody

instance HttpErrorable RequestBodyError where
  mapToError UnsupportedMediaType = return HttpUnsupportedMediaType

  mapToError CorruptBody = httpBadRequest $ toJSON $ object 
    [ "error" .= object 
      [ "context"   .= ("body" :: T.Text)
      , "kind"      .= ("corrupt_body" :: T.Text)
      ]
    ]

  mapToError (InvalidBody vf) = httpBadRequest $ toJSON $ object 
    [ "error" .= object 
      [ "context"   .= ("body" :: T.Text)
      , "path"      .= show path  
      , "kind"      .= kind
      ]
    ]
    where path = fst $ path_kind vf
          kind = snd $ path_kind vf
          path_kind (JV.ExpectedNull p)       = (p, "expected_null" :: T.Text)
          path_kind (JV.ExpectedBool p)       = (p, "expected_bool" :: T.Text)
          path_kind (JV.ExpectedArray p)      = (p, "expected_array" :: T.Text)
          path_kind (JV.ExpectedString p)     = (p, "expected_string" :: T.Text)
          path_kind (JV.ExpectedObject p)     = (p, "expected_object" :: T.Text)
          path_kind (JV.ExpectedNumber p)     = (p, "expected_number" :: T.Text)
          path_kind (JV.ExpectedInteger p)    = (p, "expected_integer" :: T.Text)
          path_kind (JV.ExpectedMember p)     = (p, "expected_member" :: T.Text)
          path_kind (JV.ExpectedExactOne p)   = (p, "expected_exact_one" :: T.Text)
          path_kind (JV.TooFewMembers p)      = (p, "too_few_members" :: T.Text)
          path_kind (JV.TooManyMembers p)     = (p, "too_many_members" :: T.Text)
          path_kind (JV.ArrayTooSmall p)      = (p, "array_too_small" :: T.Text)
          path_kind (JV.ArrayTooLarge p)      = (p, "array_too_large" :: T.Text)
          path_kind (JV.ElementsNotUnique p)  = (p, "elements_not_unique" :: T.Text)
          path_kind (JV.ValueTooShort p)      = (p, "value_too_short" :: T.Text)
          path_kind (JV.ValueTooLong p)       = (p, "value_too_long" :: T.Text)
          path_kind (JV.ValueTooSmall p)      = (p, "value_too_small" :: T.Text)
          path_kind (JV.ValueTooLarge p)      = (p, "value_too_large" :: T.Text)
          path_kind (JV.ValueNotAllowed p)    = (p, "value_not_allowed" :: T.Text)
          path_kind (JV.DoesntMatchPattern p) = (p, "doesnt_match_pattern" :: T.Text)
          path_kind (JV.NoMatchingAny p)      = (p, "no_matching_any" :: T.Text)

  mapToError UnmarshableBody = httpBadRequest $ toJSON $ object 
    [ "error" .= object 
      [ "context"   .= ("body" :: T.Text)
      , "kind"      .= ("unmarshable_body" :: T.Text)
      ]
    ]

-- Authorization
data AuthorizationError = Forbidden | Unauthorized

instance HttpErrorable AuthorizationError where
  mapToError Forbidden    = return HttpForbidden
  mapToError Unauthorized = return HttpUnauthorized

