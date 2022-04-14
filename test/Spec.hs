-- {-# LANGUAGE OverloadedStrings #-}
module Spec (main) where


import Test.Hspec ( hspec, describe, it, Spec )
import Test.Hspec.Wai ( get, shouldRespondWith, with )
import Test.Hspec.Wai.JSON ()

main :: IO ()
main = hspec undefined -- hspec spec

-- don't work need change
{-
spec :: Spec
spec = with (return app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
            get "/users" `shouldRespondWith` users
-}
