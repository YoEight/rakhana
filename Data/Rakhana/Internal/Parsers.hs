{-# LANGUAGE OverloadedStrings   #-}
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
import           Control.Applicative ((<$), (<|>), many)
import           Data.ByteString (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (digitToInt, isDigit, isHexDigit)

--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.Attoparsec.ByteString.Char8 hiding (isDigit)
import Data.Scientific (floatingOrInteger)

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Types

--------------------------------------------------------------------------------
parseHeader :: Parser (Int, Int)
parseHeader
    = do _   <- string "%PDF-"
         maj <- decimal
         _   <- char '.'
         min <- decimal
         return (maj, min)

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
parseSubsectionHeader :: Parser (Int, Int)
parseSubsectionHeader
    = do start <- decimal
         skipSpace
         count <- decimal
         pdfEndOfLine
         return (start, count)

--------------------------------------------------------------------------------
parseTrailerAfterTable :: Parser Dictionary
parseTrailerAfterTable
    = do _ <- string "trailer"
         pdfEndOfLine
         skipSpace
         Dict d <- parseDict
         return d

--------------------------------------------------------------------------------
parseTableEntry :: Parser (Int, Int, Bool)
parseTableEntry
    = do offset <- decimal
         skipSpace
         gen <- decimal
         skipSpace
         c <- anyChar
         case c of
             'n' -> return (offset, gen, False)
             'f' -> return (offset, gen, True)
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
         return $ Dict dict

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
         s <- takeStr 0 []
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
         return $ Array a

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
parseIndirectObject :: Parser (Reference, Object)
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
         let ref = (idx, gen)
         case obj of
             Dict d ->
                 let stream = parseTillStreamData >>
                              return (ref, Stream d Null) in
                 stream <|> (parseEndOfObject >> return (ref, obj))
             _      -> parseEndOfObject >> return (ref, obj)

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
             _ <- manyTill anyChar pdfEndOfLine
             return ()

--------------------------------------------------------------------------------
parseEndOfObject :: Parser ()
parseEndOfObject
    = do skipSpace
         _ <- string "endobj"
         return ()
