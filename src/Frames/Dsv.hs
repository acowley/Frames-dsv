{-# LANGUAGE TypeOperators #-}
-- | CSV parsers for use with the @Frames@ package.
--
-- Most commonly used are 'dsvTableTypes' for generating type
-- definitions at compile time based on a CSV file, and 'readDsvTable'
-- to load the table at run time. These are comparable to @tableTypes@
-- and @readTable@ from the @Frames@ package, but use an alternative
-- CSV parser.
module Frames.Dsv where
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Vinyl
import Data.Vinyl.Functor ((:.), Compose(..))
import Data.Word (Word8)

import Frames (recMaybe, Record)
import Frames.CSV (defaultParser, pipeTableMaybeOpt)
import Frames.CSV (ReadRec(..), Separator, ParserOptions(..))
import Frames.TH (rowGen, RowGen(..), tableTypes')

import qualified HaskellWorks.Data.Dsv.Lazy.Cursor as SVL

import Pipes (MonadIO, Producer, (>->), yield)
import qualified Pipes as P
import qualified Pipes.Prelude as P

import Language.Haskell.TH

-- * Row Reading

-- | Produce one DSV row at a time.
rowLoop :: Monad m => SVL.DsvCursor -> Producer [BS.ByteString] m ()
rowLoop c =
  if SVL.dsvCursorPosition d > SVL.dsvCursorPosition c && not (SVL.atEnd c)
  then do yield (V.toList (SVL.getRowBetweenStrict c d dEnd))
          rowLoop (SVL.trim d)
  else return ()
  where nr = SVL.nextRow c
        d = SVL.nextPosition nr
        dEnd = SVL.atEnd nr

-- | Produce rows of raw 'LBS.ByteString' values.
dsvRowsByte :: MonadIO m => FilePath -> Word8 -> Producer [BS.ByteString] m ()
dsvRowsByte fp columnSeparator =
  do bs <- P.liftIO (LBS.readFile fp)
     rowLoop (SVL.makeCursor columnSeparator bs)

-- | Produce rows of UTF-8 encoded 'Text' values.
dsvRows' :: MonadIO m => FilePath -> Word8 -> Producer [Text] m ()
dsvRows' fp = (>-> P.map (map T.decodeUtf8)) . dsvRowsByte fp

-- | Produce rows of Latin-1 (aka ISO-8859-1) encoded 'Text' values.
dsvRowsLatin1' :: MonadIO m => FilePath -> Word8 -> Producer [Text] m ()
dsvRowsLatin1' fp = (>-> P.map (map T.decodeLatin1)) . dsvRowsByte fp

-- | Call 'error' indicating the problem with a separator intended for
-- use with the @hw-dsv@ library.
dsvSepErr :: DsvParserOptionsError -> a
dsvSepErr = error . ("DSV separator must be a single character: "++) . show

-- | Produce rows of UTF-8 encoded 'Text' values.
dsvRows :: MonadIO m => FilePath -> Separator -> Producer [Text] m ()
dsvRows fp = either dsvSepErr (dsvRows' fp) . separatorWord8

-- | Produce rows of Latin-1 (aka ISO-8859-1) encoded 'Text' values.
dsvRowsLatin1 :: MonadIO m => FilePath -> Separator -> Producer [Text] m ()
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

-- * Whole Table Reading

-- | Produce rows where any given entry can fail to parse.
readDsvTableMaybeOpt :: (MonadIO m, ReadRec rs, RMap rs)
                     => ParserOptions
                     -> FilePath
                     -> P.Producer (Rec (Maybe :. ElField) rs) m ()
readDsvTableMaybeOpt opts csvFile =
  dsvRows csvFile (columnSeparator opts)  >-> pipeTableMaybeOpt opts
{-# INLINE readDsvTableMaybeOpt #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readDsvTableOpt :: (MonadIO m, ReadRec rs, RMap rs)
                => ParserOptions -> FilePath -> P.Producer (Record rs) m ()
readDsvTableOpt opts csvFile = readDsvTableMaybeOpt opts csvFile P.>-> go
  where go = P.await >>= maybe go (\x -> P.yield x >> go) . recMaybe
{-# INLINE readDsvTableOpt #-}

-- | Returns a producer of rows for which each column was successfully
-- parsed.
readDsvTable :: (MonadIO m, ReadRec rs, RMap rs)
             => FilePath -> P.Producer (Record rs) m ()
readDsvTable = readDsvTableOpt defaultParser
{-# INLINE readDsvTable #-}

-- * Template Haskell

-- | Like 'tableType', but additionally generates a type synonym for
-- each column, and a proxy value of that type. If the CSV file has
-- column names \"foo\", \"bar\", and \"baz\", then this will declare
-- @type Foo = "foo" :-> Int@, for example, @foo = rlens \@Foo@, and
-- @foo' = rlens' \@Foo@.
dsvTableTypes :: String -> FilePath -> DecsQ
dsvTableTypes n fp =
  tableTypes' (rowGen fp) { rowTypeName = n
                          , lineReader = dsvRows fp }
