{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module OrgMode where

import qualified System.IO as SIO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Text.Parsec.Text
import Control.Monad
import Control.Applicative hiding (many)

import Control.Lens (makeLenses)
import Data.Aeson.TH

data TextBlock = Paragraph T.Text | ListBlock [ListEntry]
    deriving (Show)

data ListEntry = ListEntry
    { _leTitle :: T.Text
    , _leContent :: [TextBlock]
    }
    deriving (Show)

makeLenses ''ListEntry
$(deriveJSON (defaultOptions{fieldLabelModifier = drop 3}) ''ListEntry)

makeLenses ''TextBlock
$(deriveJSON (defaultOptions) ''TextBlock)

type Tag = T.Text
type Status = T.Text
type Date = T.Text

data Outline = Outline
    { _olTitle    :: T.Text
    , _olStatus   :: Maybe Status
    , _olCloseDate :: Maybe Date
    , _olProperties :: [(Int, Int)]
    , _olTags     :: [Tag]
    , _olText     :: [TextBlock]
    , _olChildren :: [Outline]
    } deriving (Show)
makeLenses ''Outline
$(deriveJSON (defaultOptions{fieldLabelModifier = drop 3}) ''Outline)

type Header = T.Text

data OrgDoc = OrgDoc
    { _odHeader   :: Header
    , _odOutline  :: Outline
    } deriving (Show)
makeLenses ''OrgDoc
$(deriveJSON (defaultOptions{fieldLabelModifier = drop 3}) ''OrgDoc)


tilleol :: Parser T.Text
tilleol = T.pack <$> (many $ noneOf "\n")

noOut :: Monad m => m a -> m ()
noOut = (>> return ())

space_ :: Parser ()
space_ = noOut space

char_ :: Char -> Parser ()
char_ c = noOut $ char c

newline_ :: Parser ()
newline_ = noOut newline

outlineParser :: [Status] ->  Int -> Parser Outline
outlineParser userStatus level = do
    replicateM_ level $ char_ '*'
    space_
    status <- optionMaybe $ T.pack <$> do
        st <- (choice $ map (try . string . T.unpack) userStatus)
        space_
        return st
    title <- T.pack <$> (many $ noneOf ":\n")
    tags <- sepBy tagParser (many $ char ' ')
    newline_
    text <- nodeTextParser level
    spaces
    children <- many $ try $ outlineParser userStatus (level + 1)
    return $ Outline (T.strip title) status Nothing [] tags text children

tagParser :: Parser Tag
tagParser = do
    char_ ':'
    tag <- many (noneOf " :\n")
    char_ ':'
    return $ T.pack tag

paragraphParser :: Int -> Parser TextBlock
paragraphParser level = Paragraph <$> T.unlines <$> many1 line where
    line = do
        replicateM_ (level + 1) $ char_ ' '
        ws <- tilleol
        newline_
        return ws

listEntryParser :: Int -> Parser ListEntry
listEntryParser level = do
    replicateM_ (level + 1) $ char_ ' '
    string "- "
    title <- tilleol
    content <- nodeTextParser (level + 2)
    return $ ListEntry title content

textBlockParser :: Int -> Parser TextBlock
textBlockParser level = choice
    [ (try $ ListBlock <$> (sepEndBy1 (listEntryParser level) (many1 newline)))
    , paragraphParser level
    ]

nodeTextParser :: Int -> Parser [TextBlock]
nodeTextParser level = sepEndBy (textBlockParser level) (many1 newline_)

headerParser :: Parser Header
headerParser = T.concat <$> sepBy hline newline_ where
    hline = option "" (char_ '#' >> tilleol)

orgDocParser :: [Status] ->  Int -> Parser OrgDoc
orgDocParser userStatus level = OrgDoc <$> headerParser <*> outlineParser userStatus level

parseOrgDoc :: [Status] -> String -> T.Text -> Either ParseError OrgDoc
parseOrgDoc userStatus srcName inp = parse (orgDocParser userStatus 1) srcName inp

parseOrgFile :: String -> [Status] -> IO (Either ParseError OrgDoc)
parseOrgFile path tags = do
    SIO.withFile path SIO.ReadMode $ \h -> do
        SIO.hSetEncoding h SIO.utf8_bom
        t <- TIO.hGetContents h
        return $ parseOrgDoc tags path t
