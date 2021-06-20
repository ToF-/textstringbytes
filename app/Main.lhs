Here's an example of using String, Text and ByteString in a context of serializing data with json.
Allow for our data type to have a generic representation which will be used by Aeson to produce json representation of our values.

  stack install && textstringbytes-exe "some text with unicode 🔥"

> {-# LANGUAGE DeriveGeneric #-}


> module Main where

> import Control.Monad                        (unless)
> import Data.Aeson as J                      (FromJSON, ToJSON, decode, encode)

Size of a ByteString:

> import Data.ByteString as BS                (length)

Converting from ByteString (Strict) to String:

> import Data.ByteString.Char8 as C           (unpack)

Converting from String to Text and vice versa:

> import Data.Text as T                       (Text, pack, unpack)

Converting from Text to ByteString:

> import Data.Text.Encoding as E              (encodeUtf8)

Converting from ByteString.Lazy (which Aeson encode produces) to ByteString.Strict:

> import qualified Data.ByteString.Lazy as BL (toStrict)

> import GHC.Generics                         (Generic)
> import Prelude as P
> import System.Environment                   (getArgs)
> import System.Exit                          (die, exitSuccess)

> data Content = Content {
>
>    content :: Text,
>    size    :: Int
>    }
>    deriving (Show, Generic)
>
> instance ToJSON   Content
> instance FromJSON Content

First let's check that we have arguments on the command line:

> main :: IO ()
> main = do
>    args <- getArgs
>    unless (P.length args == 1)
>        (die "usage: textstringbytes-exe \"some text\"")
>

Let's collect and convert the arguments:

>    let st = args !! 0
>        fp = args !! 1
>        tx = T.pack st
>        bs = E.encodeUtf8 tx
>        ct = Content { content = tx, size = BS.length bs }
>        js = J.encode ct
>        nj = J.decode js :: Maybe Content

and display what we have:

>    P.putStrLn $ "received: " <> st
>    P.putStrLn $ "show content:" <> (show ct)
>    P.putStrLn $ "encode json:" <> (C.unpack (BL.toStrict js))
>    case nj of
>      Nothing -> die "couldn't decode!"
>      Just c -> P.putStrLn $ "decoded content:" <> (show ct)
>    P.putStrLn $ "decoded again:" <> (show nj)
>    P.putStrLn $ "decoded content:" <> (case (fmap (T.unpack . content) nj) of
>                                                Nothing -> ""
>                                                Just s -> s)

