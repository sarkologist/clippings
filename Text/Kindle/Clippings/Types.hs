module Text.Kindle.Clippings.Types 
( Clipping (..)
, Document (..)
, Position (..)
, Location (..)
, Content  (..)
) where

import Data.Time.LocalTime (LocalTime)

data Clipping = Clipping
  { document :: Document
  , position :: Position
  , date     :: LocalTime
  , content  :: Content
  } deriving (Eq)

data Document = Document 
  { title  :: String
  , author :: Maybe String -- Not always present, e.g. "Oxford Dictionary of English\n".
  } deriving (Eq)

data Position = Position
  { page     :: Maybe Int
  , location :: Maybe Location -- PDFs don't get these.
  } deriving (Eq)

data Location = Location Int | Region (Int,Int)

instance Eq Location where
  Location a == Location b = a==b
  Region a   == Region b = a==b
  _ ==  _ = False

data Content = Bookmark | Highlight String | Annotation String

instance Eq Content where 
  Bookmark      == Bookmark       =  True
  Highlight s0  == Highlight s1   =  s0 == s1
  Annotation s0 == Annotation s1  =  s0 == s1
  _ == _                          =  False
