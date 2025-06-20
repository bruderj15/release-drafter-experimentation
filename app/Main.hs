{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (putStrLn, getContents)
import Data.Text (Text)
import Data.Text.IO (getContents)
import Data.Attoparsec.Text
import Data.Sequence hiding ((|>), (<|))
import Control.Lens hiding (children, Empty)
import Control.Monad (join)
import Control.Applicative ((<|>))

type Indentation = Int

data ReleaseNote = ReleaseNote
  { _note                :: Text
  , _indentation         :: Indentation
  } deriving (Show, Eq)
$(makeLenses ''ReleaseNote)

indentBy :: Indentation -> ReleaseNote -> ReleaseNote
indentBy i rn = rn & indentation +~ i

data ReleaseCategory = ReleaseCategory
  { _categoryTitle        :: Text
  , _categoryReleaseNotes :: Seq ReleaseNote
  } deriving (Show, Eq)
$(makeLenses ''ReleaseCategory)

data ReleaseBody = ReleaseBody
  { _features     :: Seq ReleaseNote
  , _fixes        :: Seq ReleaseNote
  , _maintenance  :: Seq ReleaseNote
  , _dependencies :: Seq ReleaseNote
  , _footer       :: Text
  } deriving (Show, Eq)
$(makeLenses ''ReleaseBody)

main :: IO ()
main = do
  input <- getContents
  case parseOnly parseCategories input of
    Left err -> print err
    Right res -> print res

parseCategories :: Parser (Seq ReleaseCategory)
parseCategories = fromList <$> many' parseCategory

parseCategory :: Parser ReleaseCategory
parseCategory = do
  _ <- skipSpace >> string "##" >> skipSpace
  catTitle <- takeTill isEndOfLine
  _ <- skipSpace
  rns <- fmap (join . fromList) $ many' $ do
    rn <- parseReleaseNote
    children <- parsePullRequestBody
    _ <- skipSpace
    let indented_children = indentBy 2 <$> children
    return $ rn <| indented_children

  return ReleaseCategory { _categoryTitle = catTitle, _categoryReleaseNotes = rns }

parsePullRequestBody :: Parser (Seq ReleaseNote)
parsePullRequestBody = parsePullRequestBodyTemplate <|> parsePullRequestBodyDependabot

parsePullRequestBodyTemplate :: Parser (Seq ReleaseNote)
parsePullRequestBodyTemplate = do
  _ <- anyChar `manyTill` (skipSpace >> string "## Custom Release-Notes")
  _ <- skipSpace
  fromList <$> manyTill parseReleaseNote (skipSpace >> string "<!-- end-of-pr-marker -->")

parsePullRequestBodyDependabot :: Parser (Seq ReleaseNote)
parsePullRequestBodyDependabot = do
  _ <- anyChar `manyTill` (skipSpace >> string "`@dependabot ignore this dependency` will close this PR and stop Dependabot creating any more for this dependency (unless you reopen the PR or upgrade to it yourself)")
  _ <- skipSpace >> string "</details>" >> skipSpace
  return mempty

parseReleaseNote :: Parser ReleaseNote
parseReleaseNote = do
  indent <- Prelude.length <$> many' space
  _ <- string "- "
  title <- takeTill isEndOfLine
  _ <- endOfLine
  return $ ReleaseNote { _note = title, _indentation = indent }

parseDependencies :: Parser (Seq ReleaseNote)
parseDependencies = _

parseFooter :: Parser Text
parseFooter = _
