{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Internal.Parsers
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Base on pdf-toolbox Parser code
--------------------------------------------------------------------------------
module Data.Rakhana.Internal.Parsers where

--------------------------------------------------------------------------------
import           Prelude hiding (take)
import           Control.Applicative ((<$), (<|>), many)
import           Data.ByteString (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (digitToInt, isDigit, isHexDigit)
import           Data.Map (Map, fromList)
import qualified Data.Vector           as V

--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString.Char8 hiding (isDigit)
import Data.Scientific (floatingOrInteger)

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Types

--------------------------------------------------------------------------------
parseHeader :: Parser Header
parseHeader
    = do _      <- string "%PDF-"
         majVer <- decimal
         _      <- char '.'
         minVer <- decimal
         return $ makeHeader majVer minVer

--------------------------------------------------------------------------------
startXRef :: Parser Int
startXRef
    = do res <- many $
                    do _ <- manyTill anyChar $ string  "startxref"
                       skipSpace
                       offset <- decimal
                       skipSpace
                       _ <- string "%%EOF"
                       return offset
         case res of
             [] -> fail "Trailer not found"
             xs -> return $ last xs

--------------------------------------------------------------------------------
tableXRef :: Parser ()
tableXRef
    = do _ <- string "xref"
         pdfEndOfLine

--------------------------------------------------------------------------------
parseXRef :: Parser XRef
parseXRef
    = do skipSpace
         tableXRef
         h@(s,_) <- parseSubsectionHeader
         es      <- parseTableEntries s
         t       <- parseTrailerAfterTable
         return $ makeXRef h (fromList es) t

--------------------------------------------------------------------------------
parseSubsectionHeader :: Parser (Int, Int)
parseSubsectionHeader
    = do start <- decimal
         skipSpace
         len   <- decimal
         pdfEndOfLine
         return (start, len)

--------------------------------------------------------------------------------
parseTrailerAfterTable :: Parser Dictionary
parseTrailerAfterTable
    = do skipSpace
         _ <- string "trailer"
         pdfEndOfLine
         skipSpace
         Dict d <- parseDict
         return d

--------------------------------------------------------------------------------
parseTableEntries :: Int -> Parser [(Reference, TableEntry)]
parseTableEntries start
    = loop start
  where
    loop i
        = do mT <- optional parseTableEntry
             case mT of
                 Nothing -> return []
                 Just t
                     -> let gen = tableEntryGeneration t
                            off = tableEntryOffset t
                            ref = (i,gen) in
                        if (off /= 0)
                        then fmap ((ref,t):) $ loop (i+1)
                        else loop (i+1)

--------------------------------------------------------------------------------
parseTableEntry :: Parser TableEntry
parseTableEntry
    = do skipSpace
         offset <- decimal
         skipSpace
         gen <- decimal
         skipSpace
         c <- anyChar
         case c of
             'n' -> return $ makeTableEntry offset gen False
             'f' -> return $ makeTableEntry offset gen True
             _   ->
                 let msg = "error parsing XRef table entry: unknown char: " ++
                           [c] in
                 fail msg

--------------------------------------------------------------------------------
parseDict :: Parser Object
parseDict
    = do _    <- string "<<"
         dict <- many parseKey
         skipSpace
         _ <- string ">>"
         return $ Dict $ fromList dict

--------------------------------------------------------------------------------
parseKey :: Parser (ByteString, Object)
parseKey
    = do skipSpace
         key <- parseNameString
         obj <- parseObject
         return (key, obj)

--------------------------------------------------------------------------------
parseName :: Parser Object
parseName = fmap Name parseNameString

--------------------------------------------------------------------------------
parseNumber :: Parser Object
parseNumber = regularNumber <|> irregularNumber
  where
    toNumber sc
        = case floatingOrInteger sc of
              Left d  -> Number $ Real d
              Right n -> Number $ Natural n

    regularNumber = fmap toNumber scientific
    irregularNumber
        = fmap (Number . Real) $ signed $
              do _ <- char '.'
                 s <- takeWhile1 isDigit
                 let d = "0." ++ B8.unpack s
                 return $ read d

--------------------------------------------------------------------------------
parseNameString :: Parser ByteString
parseNameString
    = do _ <- char '/'
         takeWhile1 isRegularChar
  where
    isRegularChar c = c `notElem` "[]()/<>{}% \n\r"

--------------------------------------------------------------------------------
parseStringBytes :: Parser Object
parseStringBytes
    = do _ <- char '('
         s <- takeStr (0 :: Int) []
         return $ Bytes $ B8.pack s
  where
    takeStr lvl res
        = do c <- anyChar
             case c of
                 '(' -> takeStr (lvl+1) (c:res)
                 ')' | lvl == 0  -> return $ reverse res
                     | otherwise -> takeStr (lvl-1) (c:res)
                 '\\'
                     -> do c' <- anyChar
                           if c' `elem` "()\\"
                               then takeStr lvl (c':res)
                               else
                               case c' of
                                   'r'  -> takeStr lvl ('\r':res)
                                   'n'  -> takeStr lvl ('\n':res)
                                   'f'  -> takeStr lvl ('\f':res)
                                   'b'  -> takeStr lvl ('\b':res)
                                   't'  -> takeStr lvl ('\t':res)
                                   '\r' -> takeStr lvl res
                                   _ -> do
                                       c''  <- anyChar
                                       c''' <- anyChar
                                       let i'   = charToInt c' * 64
                                           i''  = charToInt c'' * 8
                                           i''' = charToInt c'''
                                           h    = toEnum $  i' + i'' + i'''
                                       takeStr lvl (h:res)
                 _ -> takeStr lvl (c:res)

--------------------------------------------------------------------------------
parseHexBytes :: Parser Object
parseHexBytes
    = do _ <- char '<'
         s <- many hex
         _ <- char '>'
         return $ Bytes $ B.pack s
  where
    hex = do h <- satisfy isHexDigit
             l <- satisfy isHexDigit
             return $ fromIntegral $ digitToInt h * 16 + digitToInt l

--------------------------------------------------------------------------------
parseBoolean :: Parser Object
parseBoolean = fmap Boolean go
  where
    go = True  <$ string "true" <|>
         False <$ string "false"

--------------------------------------------------------------------------------
parseArray :: Parser Object
parseArray
    = do _ <- char '['
         a <- many parseObject
         skipSpace
         _ <- char ']'
         return $ Array $ V.fromList a

--------------------------------------------------------------------------------
parseRef :: Parser Object
parseRef
    = do o <- decimal
         skipSpace
         g <- decimal
         skipSpace
         _ <- char 'R'
         return $ Ref o g

--------------------------------------------------------------------------------
parseObject :: Parser Object
parseObject = skipSpace >> go
  where
    go = parseName        <|>
         parseDict        <|>
         parseBoolean     <|>
         parseArray       <|>
         parseRef         <|>
         parseHexBytes    <|>
         parseStringBytes <|>
         parseNumber

--------------------------------------------------------------------------------
parseStreamHeader :: Parser ()
parseStreamHeader
    = do skipSpace
         _     <- string "stream"
         skipSpace
         return ()

--------------------------------------------------------------------------------
parseIndirectObject :: Parser (Int, Int, Object)
parseIndirectObject
    = do skipSpace
         idx <- decimal
         skipSpace
         gen <- decimal
         skipSpace
         _   <- string "obj"
         skipSpace
         skipComment
         obj <- parseObject
         return (idx, gen, obj)

--------------------------------------------------------------------------------
makeXRef :: (Int, Int)
         -> Map Reference TableEntry
         -> Dictionary
         -> XRef
makeXRef header entries dict
    = XRef
      { xrefHeader  = header
      , xrefEntries = entries
      , xrefTrailer = dict
      }

--------------------------------------------------------------------------------
makeTableEntry :: Integer -> Int -> Bool -> TableEntry
makeTableEntry offset gen free
    = TableEntry
      { tableEntryOffset     = offset
      , tableEntryGeneration = gen
      , tableEntryFree       = free
      }

--------------------------------------------------------------------------------
makeHeader :: Int -> Int -> Header
makeHeader mj mi
    = Header
      { headerMaj    = mj
      , headerMin    = mi
      }

--------------------------------------------------------------------------------
parseTillStreamData :: Parser ()
parseTillStreamData
    = do skipSpace
         _ <- string "stream"
         pdfEndOfLine

--------------------------------------------------------------------------------
charToInt :: Char -> Int
charToInt c = fromEnum c - 48

--------------------------------------------------------------------------------
pdfEndOfLine :: Parser ()
pdfEndOfLine
    = do _ <- many $ char ' '
         endOfLine <|> () <$ char '\r'

--------------------------------------------------------------------------------
skipComment :: Parser ()
skipComment
    = option () $
          do _ <- char '%'
             skipWhile (not . isSpace)

--------------------------------------------------------------------------------
parseEndOfObject :: Parser ()
parseEndOfObject
    = do skipSpace
         _ <- string "endobj"
         return ()

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
natural :: Monad m => Object -> m Integer
natural (Number (Natural i)) = return i
natural _                    = fail "natural"

--------------------------------------------------------------------------------
optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> return Nothing
