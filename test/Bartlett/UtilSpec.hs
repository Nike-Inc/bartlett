module Bartlett.UtilSpec where

import           Bartlett.Types
import           Bartlett.Util

import           Control.Exception         (evaluate)
import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy      as Lazy
import           GHC.Exts
import           Network.HTTP.Types.Status hiding (statusCode, statusMessage)
import qualified Network.Wreq              as W
import           Test.Hspec

import           Data.Either.Unwrap
import           URI.ByteString

-- | Helper to create instances of 'JenkinsInstance'.
jks :: Bool -> JenkinsInstance
jks withSSL =
  fromRight $ parseURI strictURIParserOptions url
    where url = if withSSL
                   then "https://example.com"
                   else "http://example.com"

spec_util =
  describe "Util tests" $ do

    describe "mkUrl" $ do
      it "should return the JSON API at the root of the base url when no JobPath is given" $
        uriToString (mkUrl (jks False) "" "/api/json") `shouldEndWith` "/api/json"
      it "should return a fully qualified API endpoint when given a JobPath" $
        uriToString (mkUrl (jks True) "foo" "/api/json") `shouldBe` "https://example.com/job/foo/api/json"

    describe "mkJobPath" $ do
      it "should return the empty string given empty input" $
        mkJobPath "" `shouldBe` ""
      it "should not contain '/job/' if just a '/' was passed in" $
        mkJobPath "/" `shouldBe` ""
      it "should prepend 'foo' with '/job/'" $
        mkJobPath "/foo" `shouldBe` "/job/foo"
      it "should intersperse the string with '/job'" $
        mkJobPath "/foo/bar/baz" `shouldBe` "/job/foo/job/bar/job/baz"
      it "should not return a string ending with '/job/'" $
        unpack (mkJobPath "/foo/bar") `shouldEndWith` "bar"

    describe "withForcedSSL" $ do
      it "should return a uri with the https protocol" $
        uriToString (withForcedSSL (jks False)) `shouldStartWith` "https://"
      it "should return a uri with the https protocol when https is provided" $
        withForcedSSL (jks True) `shouldBe` jks True

    describe "segmentPath" $ do
      it "should return an empty collection if nothing is passed in" $
        segmentPath "" `shouldBe` []
      it "should return a collection of 'foo' given '/foo/'" $
        segmentPath "/foo/" `shouldBe` ["foo"]
      it "should return 'foo' and 'bar' given '/foo/bar/'" $
        segmentPath "/foo/bar/" `shouldBe` ["foo", "bar"]

    describe "toPrettyJson" $
      it "should return a json string with pretty new-lines :^)" $ do
        let json = Lazy.toStrict $ encode (Object $ fromList [("foo", String "bar")])
        toPrettyJson json `shouldBe` "{\n    \"foo\": \"bar\"\n}"

    describe "parameters and options builders" $ do
      it "should return an options builder given a list of tuples" $
        optionsBuilder (parametersBuilder [("foo", "bar")])
          `shouldSatisfy` (== ("foo", "bar")) . Prelude.head . view W.params
      it "should return an empty options builder if no input is given" $
        optionsBuilder (parametersBuilder [])
          `shouldSatisfy` Prelude.null . view W.params

    describe "toResponseStatus" $
      it "should convert a Status to a StatusResponse" $
        toResponseStatus (Status 201 "Created!?")
          `shouldBe` StatusResponse{ statusCode = 201, statusMessage = "Created!?"}
