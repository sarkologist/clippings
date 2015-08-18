module Text.Kindle.Clippings.Reader where

import Control.Applicative ((<$>), (<*>), (*>), (<*), (<|>))
import Data.Bifunctor (bimap)
import Data.Functor.Extras ((<$$>),for)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.String.Extras (pad, chomp)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Parse (strptime)
import Text.Kindle.Clippings.Types (Clipping(..),Location(..),Document(..),Position(..),Content(..))
import Text.Parsec (many1, digit, alphaNum, string, skipMany, oneOf, try, char, manyTill, anyToken, optionMaybe)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator.Extras (but, tryBut1, tryMaybe, tryString, stringCI)

eol :: Parser ()
eol = skipMany $ oneOf "\n\r"

eor :: Parser String
eor = string "=========="

readDocument :: Parser Document
readDocument = do
  result@(Document t a) <- Document <$> readTitle <*> readAuthor
  return $ if fmap last a == Just ' '
    then Document (t ++ " (" ++ init (fromJust a)) Nothing
    else result

readTitle :: Parser String
readTitle = chomp . concat <$> textAndBrackets
  where brackets = (\a b c -> a:(concat b)++c) <$> char '(' <*> textAndBrackets <*> string ") "
        textAndBrackets = many1 (tryBut1 "(\r\n)" <|> try brackets)

readAuthor :: Parser (Maybe String)
readAuthor = tryMaybe $ init 
         <$> (char '(' *> but "\n\r")

readContentType :: Parser String
readContentType = (tryString "- Your " <|> string "- ")
               *> but " "
               <* (tryString " on " <|> tryString " at " <|> many1 (char ' '))

readPageNumber :: Parser (Maybe (Int, Maybe Int))
readPageNumber = optionMaybe . try $ do
  left <- stringCI "Page " *> many1 alphaNum
  right <- optionMaybe . try $ string "-" *> many1 alphaNum
  _  <- string " | "
  return (read left, fmap read right)

readLocation :: Parser (Maybe Location)
readLocation = tryMaybe 
             $ (tryString "Loc. " <|> stringCI "Location ")
            *> (try readLocationRegion <|> readLocationInt)
            <* but "|" <* char '|' <* many1 (char ' ')

readLocationInt :: Parser Location
readLocationInt = Location . read <$> many1 digit

readLocationRegion :: Parser Location
readLocationRegion = parseRegion 
                <$$> (,) 
                 <$> many1 digit 
                 <*> (char '-' *> many1 digit)

parseRegion :: (String, String) -> Location
parseRegion = Region . bimap read read . pad

parseDate :: String -> LocalTime
parseDate = fst . fromJust . fromJust {-[^1]-} . find isJust . for formats . flip strptime
  where formats =
          [ "%A, %d %B %y %X"
          , "%A, %B %d, %Y %r"
          , ""
          ]
-- [^1]: This is safe: `strptime x ""` is `Just` for all `x`.

readDate :: Parser LocalTime
readDate = parseDate <$> (string "Added on " *> but "\n\r")

readContent :: Parser String
readContent = chomp <$> manyTill anyToken (try eor)

readClipping :: Parser (Maybe Clipping)
readClipping = clipping
           <$> (readDocument <* eol)
           <*> readContentType
           <*> (Position <$> readPageNumber <*> readLocation)
           <*> readDate <* eol
           <*> readContent <* eol

clipping :: Document -> String -> Position -> LocalTime -> String -> Maybe Clipping
clipping d t p l c
  |(==) t "Highlight" = Just $ Clipping d p l $ Highlight c
  |(==) t "Note"      = Just $ Clipping d p l $ Annotation c
  |(==) t "Bookmark"  = Just $ Clipping d p l Bookmark
  |otherwise = Nothing

readClippings :: Parser [Maybe Clipping]
readClippings = many1 readClipping
