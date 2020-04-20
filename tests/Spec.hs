-- Sempervivum -- A plant watering tracker
-- Copyright 2020 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do

  describe "Nothing" $ do

    it "A is A" $ do
      "A" `shouldBe` ("A" :: String)
