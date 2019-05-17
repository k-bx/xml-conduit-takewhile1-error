{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (ConduitT, (.|), runConduit)
import qualified Data.Conduit.List as CL
import Data.Default (def)
import qualified Data.String.Class as S
import qualified Data.XML.Types as X
import Text.InterpolatedString.Perl6 (q)
import qualified Text.XML.Stream.Parse as X

data GRBookDetails =
  GRBookDetails
  deriving (Show)

bookShowEx01 :: BL.ByteString
bookShowEx01 =
  [q|
<?xml version="1.0" encoding="UTF-8"?>
<GoodreadsResponse>
  <Request>
    <authentication>true</authentication>
    <key>key</key>
    <method><![CDATA[book_show]]></method>
  </Request>
  <book>
    <popular_shelves>
      <shelf name="to-read" count="6" />
      <shelf name="письменниця" count="1" />
      <shelf name="ukrainian-literature" count="1" />
      <shelf name="praktisk" count="1" />
    </popular_shelves>
  </book>
</GoodreadsResponse>
|]

parseBookDetails' ::
     (MonadThrow m, MonadUnliftIO m)
  => ConduitT X.Event o m (Maybe GRBookDetails)
parseBookDetails' = do
  fmap join $
    X.tag' "GoodreadsResponse" X.ignoreAttrs $ \_ -> do
      _ <- X.ignoreTreeContent "Request"
      bd <-
        X.tag' ("book") X.ignoreAttrs $ \_ -> do
          _ <- X.ignoreTreeContent "popular_shelves"
          pure $ GRBookDetails
      pure bd
      -- pure $ GRBookDetails

parseBookDetails ::
     (MonadThrow m, MonadUnliftIO m) => BL.ByteString -> m GRBookDetails
parseBookDetails bs = do
  res <-
    runResourceT $
    runConduit $
    CL.sourceList [S.fromLazyByteString bs] .| X.parseText' def .|
    -- X.parseLBS def bs .|
    X.force "GoodreadsResponse root required" parseBookDetails'
  pure res

main :: IO ()
main = do
  res <- parseBookDetails bookShowEx01
  print res
