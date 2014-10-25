module FreePalace.Media.LoaderSpec where

import Test.Hspec

import qualified FreePalace.Media.Loader as MediaLoader

spec :: Spec
spec = do
  describe "escapeFilename" $ do
    it "does not escape letters" $ do
      let untouched = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
          result = MediaLoader.escapeFilename untouched
      result `shouldBe` untouched

    it "does not escape numbers" $ do
      let untouched = "0123456789"
          result = MediaLoader.escapeFilename untouched
      result `shouldBe` untouched

    it "does not escape hyphens, underscores, or periods" $ do
      let untouched = "_-."
          result = MediaLoader.escapeFilename untouched
      result `shouldBe` untouched

    it "should escape everything else" $ do
      let toModify = " !\"#$%&'()*+,/:;<=>?@[\\]^`{|}~"
          escaped = "%20%21%22%23%24%25%26%27%28%29%2A%2B%2C%2F%3A%3B%3C%3D%3E%3F%40%5B%5C%5D%5E%60%7B%7C%7D%7E"
          actualResult = MediaLoader.escapeFilename toModify
      actualResult `shouldBe` escaped

