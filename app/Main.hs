{-# LANGUAGE DeriveGeneric #-}
-- stack install && textstringbytes-exe "foo 🚷♻⛄" myfile.txt
module Main where

import GHC.Generics                     (Generic)
import System.Environment               (getArgs)
import System.Exit                      (die, exitSuccess)
import Control.Monad                    (unless)
import Data.Text
import Data.Text.Encoding
import Data.ByteString
import Data.ByteString.Char8
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL



data Content = Content { content :: Text
                       , size    :: Int  }
    deriving (Show, Generic)

instance ToJSON Content
instance FromJSON Content

main :: IO ()
main = do
    args <- getArgs
    unless (Prelude.length args == 2)
        (die $ "usage: textstringbytes-exe \"some text\" filename")

    let arg_as_string = args !! 0
        arg_as_text = Data.Text.pack arg_as_string
        arg_as_bytestring = Data.Text.Encoding.encodeUtf8 arg_as_text
        ct   = Content arg_as_text (Data.ByteString.length arg_as_bytestring)
        file_path = args !! 1
    let json = encode ct
        newc = decode json :: Maybe Content
    Prelude.putStrLn $ "received: " <> arg_as_string
    Prelude.putStrLn $ "show content:" <> (show ct)
    Prelude.putStrLn $ "encode json:" <> (Data.ByteString.Char8.unpack (BL.toStrict json))
    case newc of
      Nothing -> die "couldn't decode!"
      Just c -> Prelude.putStrLn $ "decoded content:" <> (show c)
    Prelude.putStrLn $ "decoded again:" <> (show newc)
    Prelude.putStrLn $ "decoded content:" <> (case (fmap (Data.Text.unpack . content) newc) of
                                                Nothing -> ""
                                                Just s -> s)


    





