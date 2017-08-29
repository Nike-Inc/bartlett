{-# LANGUAGE DuplicateRecordFields #-}

module Bartlett.TypesSpec where

import Bartlett.Types

import Network.Wreq   hiding (statusCode, statusMessage)
import Test.Hspec

spec_types =
  describe "Type tests" $ do

    describe "getBasicAuth for BasicAuthUser" $
      it "should construct a bathic auth object for use in Wreq" $ do
        let usr = User "foo" "bar"
        getBasicAuth usr `shouldBe` basicAuth "foo" "bar"

    describe "accessors for basic StatusResponse" $ do
      it "should return its status code" $ do
        let s = StatusResponse {statusCode = 9001, statusMessage = "It's over 9000!"}
        statusCode s `shouldBe` 9001
      it "should return its status message" $ do
        let s = StatusResponse {statusCode = 201, statusMessage = "Created."}
        statusMessage s `shouldBe` "Created."
