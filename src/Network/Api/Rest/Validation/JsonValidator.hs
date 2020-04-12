{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Validation.JsonValidator (
    null, bool, number, integer, string, object, member, memberOpt, array, elements,
    min, max, minLength, maxLength, minMembers, maxMembers, unique, 
    all, any, one,
    inIntegerEnum, inNumberEnum, inStringEnum,
    format, pattern, regexOf,

    Validator, ValidationFailure(..)
 ) where

import Prelude hiding (all, any, min, max, null)
import Data.Maybe (isJust)
import Data.Either (isRight)
import Data.Scientific
import Data.Function ((&))

import qualified Data.Vector as DV
import qualified Data.HashMap.Strict as DHS
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import qualified Data.Aeson as Json

import qualified Network.Api.Rest.Validation.JsonPath as Json

type Validator = Json.Path -> Either ValidationFailure Json.Value -> Either ValidationFailure Json.Value

data ValidationFailure
   = ExpectedNull Json.Path
   | ExpectedBool Json.Path
   | ExpectedArray Json.Path
   | ExpectedString Json.Path
   | ExpectedObject Json.Path
   | ExpectedNumber Json.Path
   | ExpectedInteger Json.Path
   | ExpectedMember Json.Path
   | ExpectedExactOne Json.Path
   | TooFewMembers Json.Path
   | TooManyMembers Json.Path
   | ArrayTooSmall Json.Path
   | ArrayTooLarge Json.Path
   | ElementsNotUnique Json.Path
   | ValueTooShort Json.Path
   | ValueTooLong Json.Path
   | ValueTooSmall Json.Path
   | ValueTooLarge Json.Path
   | ValueNotAllowed Json.Path
   | DoesntMatchPattern Json.Path
   | NoMatchingAny Json.Path
   deriving (Show, Eq, Ord)

-- Types
null :: Validator
null p = test spec (ExpectedNull p) p
  where spec Json.Null = True
        spec _         = False

bool :: [Validator] -> Validator
bool vs p b = ftest spec p b
  where spec (Json.Bool _) = all vs p b
        spec _             = Left (ExpectedBool p)

number :: [Validator] -> Validator
number vs p n = ftest spec p n
  where spec (Json.Number _) = all vs p n 
        spec _               = Left (ExpectedNumber p)

integer :: [Validator] -> Validator
integer vs p i = ftest spec p i
  where spec (Json.Number n) = case (toBoundedInteger n :: Maybe Int) of
          (Just _)  -> all vs p (pure (Json.Number n))
          (Nothing) -> Left (ExpectedInteger p)
        spec _               = Left (ExpectedInteger p)

string :: [Validator] -> Validator
string vs p s = ftest spec p s
  where spec (Json.String _) = all vs p s
        spec _               = Left (ExpectedString p)

object :: [Validator] -> Validator
object vs p o = ftest spec p o
  where spec (Json.Object _) = all vs p o
        spec _               = Left (ExpectedObject p)

member :: T.Text -> Validator -> Validator
member n v p o = ftest spec p o
  where spec (Json.Object jo) = case DHS.lookup n jo of
          Just m  -> v mp (pure m)
          Nothing -> Left  (ExpectedMember mp)
        spec _                = Left (ExpectedObject p)
        mp                    = Json.pathWithMember p n

memberOpt :: T.Text -> Validator -> Validator
memberOpt n v p o = ftest spec p o
  where spec (Json.Object jo) = case DHS.lookup n jo of
          Just m  -> v mp (pure m)
          Nothing -> o
        spec _                = Left (ExpectedObject p)
        mp                    = Json.pathWithMember p n

array :: [Validator] -> Validator
array vs p a = ftest spec p a
  where spec (Json.Array _) = all vs p a
        spec _              = Left (ExpectedArray p)

unique :: Validator
unique p = test spec (ElementsNotUnique p) p
  where spec (Json.Array es) = DV.length (DV.uniq es) == DV.length es
        spec _               = False

elements :: Validator -> Validator
elements v p a = ftest spec p a
  where spec (Json.Array es) = apply $ DV.toList es
        spec _               = Left (ExpectedArray p)
        apply []             = a
        apply es             = stest es p v

-- Combinators
all :: [Validator] -> Validator
all [] _ e     = e
all (v:vs) p e = case v p e of
  Right _ -> all vs p e
  Left err -> Left err

any :: [Validator] -> Validator
any [] _ e     = e
any (v:[]) p e = case v p e of
  Right r -> Right r
  Left _ -> Left (NoMatchingAny p)
any (v:vs) p e = case v p e of
  Right r -> Right r
  Left _ -> any vs p e

one :: [Validator] -> Validator
one [] _ e = e
one vs p e 
  | passed == 1 = e
  | otherwise   = Left (ExpectedExactOne p)
  where passed   = count $ map passes vs
        count    = length . (filter id)
        passes v = isRight (v p e)

-- Ranges
min :: Scientific -> Validator
min m p = test gte (ValueTooSmall p) p
  where gte (Json.Number n)  = m <= n
        gte (Json.String s)  = toInt m <= T.length s
        gte (Json.Array a)   = toInt m <= length a
        gte (Json.Object o)  = toInt m <= length o
        gte _                = False 
 
max :: Scientific -> Validator
max m p = test lte (ValueTooLarge p) p
  where lte (Json.Number n)  = m >= n
        lte (Json.String s)  = toInt m >= T.length s
        lte (Json.Array a)   = toInt m >= length a
        lte (Json.Object o)  = toInt m >= length o
        lte _                = False

minMembers :: Int -> Validator
minMembers m p jv = min (fromInt m) p jv & failAs (TooFewMembers p) p

maxMembers :: Int -> Validator
maxMembers m p jv = max (fromInt m) p jv & failAs (TooManyMembers p) p

minLength :: Int -> Validator
minLength m p r@(Right(Json.Array _))  = min (fromInt m) p r & failAs (ArrayTooSmall p)  p
minLength m p r@(Right(Json.String _)) = min (fromInt m) p r & failAs (ValueTooShort p) p
minLength m p r                        = min (fromInt m) p r

maxLength :: Int -> Validator
maxLength m p r@(Right(Json.Array _))  = max (fromInt m) p r & failAs (ArrayTooLarge p) p
maxLength m p r@(Right(Json.String _)) = max (fromInt m) p r & failAs (ValueTooLong p) p
maxLength m p r                        = max (fromInt m) p r

-- Patterns
pattern :: ICU.Regex -> Validator
pattern re p = test matches (DoesntMatchPattern p) p
  where matches (Json.String s) = isJust (ICU.find re s)
        matches _ = False

format :: ICU.Regex -> Validator
format = pattern

regexOf :: String -> ICU.Regex
regexOf = ICU.regex [] . T.pack 

-- Enums
inIntegerEnum :: [Int] -> Validator
inIntegerEnum xs p = test (`elem` (map (Json.Number . fromInt) xs)) (ValueNotAllowed p) p

inNumberEnum :: [Scientific] -> Validator
inNumberEnum xs p = test (`elem` (map Json.Number xs)) (ValueNotAllowed p) p

inStringEnum :: [T.Text] -> Validator
inStringEnum xs p = test (`elem` (map Json.String xs)) (ValueNotAllowed p) p

-- Helpers
ftest :: (Json.Value -> Either ValidationFailure Json.Value) -> Validator
ftest _ _ l@(Left _) = l
ftest v _ (Right jv) = v jv

stest :: [Json.Value] -> Json.Path -> Validator -> Either ValidationFailure Json.Value
stest jvs p v = visit jvs p 0 v
  where visit ([]) _ _ _         = error "stest called with no values"
        visit (jv':[]) p' e v'   = v' (Json.pathWithElement p' e) (pure jv')
        visit (jv':jvs') p' e v' = case v' (Json.pathWithElement p' e) (pure jv') of
          Left l  -> Left l
          Right _ -> visit jvs' p' (e + 1) v'

failAs :: ValidationFailure -> Validator
failAs f _ (Left _) = Left f
failAs _ _ r        = r

toInt :: Scientific -> Int
toInt n = case toBoundedInteger n :: Maybe Int of
  Just i  -> i
  Nothing -> error "value is out of bound"

fromInt :: Int -> Scientific
fromInt = unsafeFromRational . toRational

test :: (Json.Value -> Bool) -> ValidationFailure -> Validator
test _ _ _ l@(Left _)   = l
test v f _ r@(Right jv)
  | v jv = r 
  | otherwise = Left f