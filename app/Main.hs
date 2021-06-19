{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics                     (Generic)
import System.Environment               (getArgs)
import System.Exit                      (die, exitSuccess)
import Control.Monad                    (unless)
import Data.Text
import Data.Text.Encoding
import Data.ByteString



data Content = Content { content :: Text
                       , bytes   :: ByteString
                       , size    :: Int  }
    deriving (Show)



stringToText :: String -> Text
stringToText = Data.Text.pack

textToByteString :: Text -> ByteString
textToByteString = Data.Text.Encoding.encodeUtf8

byteStringLength :: ByteString -> Int
byteStringLength = Data.ByteString.length

main :: IO ()
main = do
    args <- getArgs
    unless (Prelude.length args == 2)
        (die $ "usage: textstringbytes-exe \"some text\" filename")

    let arg_as_string = args !! 0
        arg_as_text = stringToText arg_as_string
        arg_as_bytestring = textToByteString arg_as_text
        content   = Content arg_as_text arg_as_bytestring (byteStringLength arg_as_bytestring)
        file_path = args !! 1
    Prelude.writeFile file_path (show content)





