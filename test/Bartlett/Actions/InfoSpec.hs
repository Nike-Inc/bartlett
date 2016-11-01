module Bartlett.Actions.InfoSpec where

import Bartlett.Actions.Info

import Data.ByteString.Lazy.Char8
import Test.Hspec

-- TODO figure out how to mock some HTTP request action
--      this might be a good resource: https://github.com/bos/wreq/blob/master/tests/UnitTests.hs

spec :: Spec
spec = describe "Info tests" $

    describe "getInfo" $
      it "should make a request to jenkins" $
        1 `shouldBe` 1
