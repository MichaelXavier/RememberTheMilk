{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Web.RememberTheMilk.Testing.Types (specs) where

import           Data.Aeson (json,
                             fromJSON,
                             Result(..))
import           Data.Attoparsec.Lazy (parse, eitherResult)
import           Data.String.QQ (s)
import           Network.URL
import           Test.Hspec (Specs, describe, descriptions, it)
import           Test.Hspec.HUnit
import           Test.HUnit.Base ((~?=))

import Web.RememberTheMilk.Types

specs :: Specs
specs = descriptions [describe_parseFrob,
                      describe_parseRTMTokenSummary,
                      describe_parseUser]

describe_parseFrob :: Specs
describe_parseFrob = describe "parsing Frob" [
  it "parses a well-formed Frob" (parseStr frobJSON ~?= Right frob)]
  where frobJSON = [s|
{
  "rsp": {
    "frob": "abcdefg", 
    "stat": "ok"
  }
}
        |]

describe_parseRTMTokenSummary :: Specs
describe_parseRTMTokenSummary = describe "parsing RTMTokenSummary" [
  it "parses a well-formed token summary" (parseStr tokenSummaryJSON ~?= Right tokenSummary)]
  where tokenSummaryJSON = [s|
{
  "perms": "delete", 
  "token": "TOK", 
  "user": {
    "fullname": "Michael Xavier", 
    "id": "1234", 
    "username": "someusername"
  }
}
        |]

describe_parseUser :: Specs
describe_parseUser = describe "parsing User" [
  it "parses a well-formed token summary" (parseStr userJSON ~?= Right usr)]
  where userJSON = [s|
{
  "fullname": "Michael Xavier", 
  "id": "1234", 
  "username": "someusername"
}
        |]

---- Fixtures
frob :: Frob
frob = Frob "abcdefg"

tokenSummary :: RTMTokenSummary
tokenSummary = RTMTokenSummary { toksumToken = "TOK",
                                  toksumPerms = Delete,
                                  toksumUser  = usr }

usr :: User
usr = User { userId       = "1234",
              userFullname = Just "Michael Xavier", 
              userName     = Just "someusername"}

---- Helpers
parseStr str = fjson =<< parsed
  where fjson v = case fromJSON v of
                    Success a -> Right a
                    Error e   -> Left e
        parsed  = eitherResult $ parse json str
