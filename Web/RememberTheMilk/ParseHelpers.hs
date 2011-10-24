{-# LANGUAGE OverloadedStrings #-}
module Web.RememberTheMilk.ParseHelpers ((.:/),
                                         (.:/|),
                                         deepValue,
                                         listAttribute,
                                         coerceEmpty,
                                         coerceBool,
                                         coerceInteger,
                                         dv) where

import           Control.Applicative ((<$>), (<*>), pure)
import           Data.Aeson (Value(..),
                             Object,
                             FromJSON,
                             parseJSON,
                             (.:),
                             (.:?))
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Attoparsec.Number (Number(I))
import           Data.List (intercalate)
import qualified Data.Map as M
import           Data.Vector (singleton)
import           Data.Text (Text, unpack)

(.:/) :: (FromJSON a) => Object
                         -> [Text]
                         -> Parser a
obj .:/ (k:ks) = maybe (fail msg) parseJSON parsed
 where parsed = deepValue ks =<< M.lookup k obj
       msg    = traversalMsg (k:ks)
obj .:/ []     = parseJSON $ Object obj

(.:/|) :: (FromJSON a) => Object
                         -> ([Text], a)
                         -> Parser a
obj .:/| ((k:ks), dflt) = maybe (pure dflt) parseJSON parsed
 where parsed = deepValue ks =<< M.lookup k obj
obj .:/| ([],_)    = parseJSON $ Object obj

deepValue :: [Text]
             -> Value
             -> Maybe Value
deepValue (k:[]) (Object obj) = M.lookup k obj
deepValue (k:[]) _            = Nothing
deepValue (k:ks) (Object obj) = deepValue ks =<< M.lookup k obj
deepValue (k:ks) _            = Nothing
deepValue [] v                = Just v

dv = flip deepValue

traversalMsg :: [Text] -> String
traversalMsg ks = "Failed to find " ++ (intercalate "/" $ map unpack ks)

listAttribute :: FromJSON a => Object
                               -> [Text]
                               -> Parser a
listAttribute obj (k:ks) = maybe (fail msg) (parseJSON . coerce) parsed
  where parsed = deepValue ks =<< M.lookup k obj
        msg = traversalMsg (k:ks)
        coerce v@(Object _) = Array (singleton v)
        coerce v@(Array _)  = v

coerceEmpty :: FromJSON a => Object -> Text -> Parser (Maybe a)
coerceEmpty obj key = maybe (pure Nothing) coerce $ M.lookup key obj
  where coerce (String "") = pure Nothing
        coerce v           = parseJSON v

coerceBool :: Object -> Text -> Parser Bool
coerceBool obj key = maybe (pure False) coerce $ M.lookup key obj
  where coerce (String "0") = pure False
        coerce (String "1") = pure True
        coerce (Bool b)     = pure b
        coerce v            = typeMismatch "Bool" v

coerceInteger :: Object -> Text -> Parser Integer
coerceInteger obj key = maybe (fail "Could not parse Integer") coerce $ M.lookup key obj
  where coerce (String str) = pure . read . unpack $ str
        coerce (Number (I int)) = pure int
        coerce v            = typeMismatch "Integer" v
