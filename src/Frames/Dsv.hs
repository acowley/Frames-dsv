module Frames.Dsv where
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Word (Word8)

import Frames.CSV (Separator)

import qualified HaskellWorks.Data.Dsv.Lazy.Cursor as SVL

import Pipes (Producer, (>->), yield)
import qualified Pipes as P
import qualified Pipes.Prelude as P

-- | Produce one DSV row at a time.
rowLoop :: Monad m => SVL.DsvCursor -> Producer [LBS.ByteString] m ()
rowLoop c =
  if SVL.dsvCursorPosition d > SVL.dsvCursorPosition c && not (SVL.atEnd c)
  then yield (V.toList (SVL.getRowBetween c d dEnd)) >> rowLoop (SVL.trim d)
  else return ()
  where nr = SVL.nextRow c
        d = SVL.nextPosition nr
        dEnd = SVL.atEnd nr

-- | Produce rows of raw 'LBS.ByteString' values.
dsvRowsByte :: FilePath -> Word8 -> Producer [LBS.ByteString] IO ()
dsvRowsByte fp columnSeparator =
  do bs <- P.liftIO (LBS.readFile fp)
     rowLoop (SVL.makeCursor columnSeparator bs)

-- | Produce rows of UTF-8 encoded 'Text' values.
dsvRows' :: FilePath -> Word8 -> Producer [Text] IO ()
dsvRows' fp = (>-> P.map (map (T.decodeUtf8 . LBS.toStrict)))
           . dsvRowsByte fp

-- | Produce rows of Latin-1 (aka ISO-8859-1) encoded 'Text' values.
dsvRowsLatin1' :: FilePath -> Word8 -> Producer [Text] IO ()
dsvRowsLatin1' fp = (>-> P.map (map (T.decodeLatin1 . LBS.toStrict)))
                  . dsvRowsByte fp

-- | Call 'error' indicating the problem with a separator intended for
-- use with the @hw-dsv@ library.
dsvSepErr :: DsvParserOptionsError -> a
dsvSepErr = error . ("DSV separator must be a single character: "++) . show

-- | Produce rows of UTF-8 encoded 'Text' values.
dsvRows :: FilePath -> Separator -> Producer [Text] IO ()
dsvRows fp = either dsvSepErr (dsvRows' fp) . separatorWord8

-- | Produce rows of Latin-1 (aka ISO-8859-1) encoded 'Text' values.
dsvRowsLatin1 :: FilePath -> Separator -> Producer [Text] IO ()
dsvRowsLatin1 fp = either dsvSepErr (dsvRowsLatin1' fp) . separatorWord8

-- | The ways in which an arbitrary 'Text' value may be unsuitable for
-- use as a separator for the @hw-dsv@ package.
data DsvParserOptionsError = SeparatorIsNull
                           | SeparatorCharIsMoreThanOneByte
                           | SeparatorIsMoreThanOneChar
  deriving (Eq, Show)

-- | The @Frames@ library supports column separators that can be
-- arbitrary 'Text' values, but the @hw-dsv@ library requires a single
-- byte be used to demarcate values. If the given 'Text' can be
-- losslessly represented as a single byte, we sue it, otherwise we
-- return an error indicating the problem.
separatorWord8 :: Separator -> Either DsvParserOptionsError Word8
separatorWord8 sep =
  case T.uncons sep of
    Nothing -> Left SeparatorIsNull
    Just (h, t)
      | T.null t -> let i = fromEnum h
                    in if i < 256
                       then Right (fromIntegral i)
                       else Left SeparatorCharIsMoreThanOneByte
      | otherwise -> Left SeparatorIsMoreThanOneChar
