module Web.RememberTheMilk.ParseHelpers ((.:/),
                                         (.:/|),
                                         deepValue,
                                         listAttribute,
                                         dv) where

import           Control.Applicative ((<$>), (<*>), pure)
import           Data.Aeson (Value(..),
                             Object,
                             FromJSON,
                             parseJSON,
                             (.:),
                             (.:?))
import           Data.Aeson.Types (Parser, typeMismatch)
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
