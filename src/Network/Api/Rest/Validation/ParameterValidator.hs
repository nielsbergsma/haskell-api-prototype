{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Validation.ParameterValidator (
  optional, required,
  bool, string, number, integer,
  all, any, one,
  min, max, minLength, maxLength,
  pattern, regexOf, format,
  inIntegerEnum, inStringEnum, inNumberEnum,

  readText, readMaybeText,
  
  Name, RawValue, Value(..), Validator, ValidationFailure(..)
  ) where

import Prelude hiding (all, any, min, max)
import Data.Maybe (isJust)
import Data.Either (isRight)
import Data.Function ((&))

import qualified Data.Text as T
import qualified Text.Read as R
import qualified Data.Text.ICU as ICU

type Name = T.Text

type RawValue = T.Text

type Validator = Name -> Either ValidationFailure Value -> Either ValidationFailure Value

data Value
    = NotSet
    | NoType RawValue
    | Bool Bool
    | Number Double
    | Integer Int
    | String RawValue
    deriving (Show, Eq)

data ValidationFailure 
    = ExpectedBool Name
    | ExpectedInteger Name
    | ExpectedNumber Name
    | ExpectedString Name
    | ExpectedExactOne Name
    | ExpectedPresent Name
    | ValueTooShort Name
    | ValueTooLong Name
    | ValueTooSmall Name
    | ValueTooLarge Name
    | ValueNotAllowed Name
    | DoesntMatchPattern Name
    | NoMatchingAny Name
    deriving (Show, Eq)

-- Presences
optional :: Name -> Maybe RawValue -> Validator -> Either ValidationFailure Value
optional n i v = case i of
    (Nothing) -> return $ NotSet
    (Just j)  -> v n (pure $ NoType j)

required :: Name -> Maybe RawValue -> Validator -> Either ValidationFailure Value
required n i v = case i of
    (Nothing) -> Left (ExpectedPresent n)
    (Just j)  -> v n (pure $ NoType j)

-- Types
bool :: [Validator] -> Validator
bool vs p b = ftest spec p b
    where spec (Bool _)   = all vs p b
          spec (NoType v) = bool vs p (readBool p v)
          spec _          = Left (ExpectedBool p)

string :: [Validator] -> Validator
string vs p n = ftest spec p n
    where spec (String _) = all vs p n
          spec (NoType v) = string vs p (pure $ String v)
          spec _          = Left (ExpectedString p)

number :: [Validator] -> Validator
number vs p n = ftest spec p n
    where spec (Number _) = all vs p n
          spec (NoType v) = number vs p (readNumber p v)
          spec _          = Left (ExpectedNumber p)

integer :: [Validator] -> Validator
integer vs p n = ftest spec p n
    where spec (Integer _) = all vs p n
          spec (NoType v)  = integer vs p (readInteger p v)
          spec _           = Left (ExpectedInteger p)

-- Combinators
all :: [Validator] -> Validator
all [] _ e     = e
all (v:vs) p e = case v p e of
  Right _ -> all vs p e
  Left l  -> Left l

any :: [Validator] -> Validator
any [] _ e     = e
any (v:[]) p e = case v p e of
  Right r -> Right r
  Left  _ -> Left (NoMatchingAny p)
any (v:vs) p e = case v p e of
  Right r -> Right r
  Left  _ -> any vs p e

one :: [Validator] -> Validator
one [] _ e = e
one vs p e 
  | success == 1 = e
  | otherwise   = Left (ExpectedExactOne p)
  where success = count $ map apply vs
        count   = length . (filter id)
        apply v = isRight (v p e)

-- Ranges
min :: Integral a => a -> Validator
min m p = test gte (ValueTooSmall p) p
  where gte (Number n)  = toDouble m <= n
        gte (Integer i) = toInt m <= i
        gte (String s)  = toInt m <= T.length s
        gte _           = False 

max :: Integral a => a -> Validator
max m p = test lte (ValueTooLarge p) p
  where lte (Number n)  = toDouble m >= n
        lte (Integer n) = toInt m >= n
        lte (String s)  = toInt m >= T.length s
        lte _           = False

minLength :: Int -> Validator
minLength m p r@(Right(String _)) = min m p r & failAs (ValueTooShort p) p
minLength m p r                   = min m p r

maxLength :: Int -> Validator
maxLength m p r@(Right(String _)) = max m p r & failAs (ValueTooLong p) p
maxLength m p r                   = max m p r

-- Patterns
pattern :: ICU.Regex -> Validator
pattern re p = test matches (DoesntMatchPattern p) p
  where matches (String s) = isJust (ICU.find re $ s)
        matches _          = False

format :: ICU.Regex -> Validator
format = pattern

regexOf :: T.Text -> ICU.Regex
regexOf = ICU.regex []

-- Enums
inIntegerEnum :: [Int] -> Validator
inIntegerEnum xs p = test (`elem` (map Integer xs)) (ValueNotAllowed p) p

inNumberEnum :: [Double] -> Validator
inNumberEnum xs p = test (`elem` (map Number xs)) (ValueNotAllowed p) p

inStringEnum :: [T.Text] -> Validator
inStringEnum xs p = test (`elem` (map String xs)) (ValueNotAllowed p) p

-- Transformers
readText :: Value -> T.Text
readText (String s) = s
readText _ = ""

readMaybeText :: Value -> Maybe T.Text
readMaybeText (String s) = Just s
readMaybeText _ = Nothing

-- Helpers
test :: (Value -> Bool) -> ValidationFailure -> Validator
test _ _ _ l@(Left _) = l
test v f _ r@(Right jv) 
  | v jv = r
  | otherwise = Left f

ftest :: (Value -> Either ValidationFailure Value) -> Validator
ftest _ _ l@(Left _) = l
ftest v _ (Right jv) = v jv

failAs :: ValidationFailure -> Validator
failAs f _ (Left _) = Left f
failAs _ _ r        = r

readNumber :: Name -> RawValue -> Either ValidationFailure Value
readNumber p t = case R.readEither (T.unpack t) :: Either String Double of 
    Right r -> Right $ Number r
    Left  _ -> Left  $ ExpectedNumber p

readInteger :: Name -> RawValue -> Either ValidationFailure Value
readInteger p t = case R.readEither (T.unpack t) :: Either String Int of 
    Right r -> Right $ Integer r
    Left  _ -> Left  $ ExpectedInteger p

readBool :: Name -> RawValue -> Either ValidationFailure Value 
readBool _ "true"  = Right $ Bool True
readBool _ "false" = Right $ Bool False 
readBool p _       = Left  $ ExpectedBool p

toDouble :: (Real n) => n -> Double
toDouble = fromRational . toRational

toInt :: (Integral n) => n -> Int
toInt n = fromIntegral n :: Int