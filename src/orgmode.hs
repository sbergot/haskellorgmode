{-# LANGUAGE OverloadedStrings #-}

module OrgMode where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Control.Monad
import Control.Applicative hiding (many)

type Paragraph = T.Text
type NodeText = [Paragraph]
type Tag = T.Text
type Tags = [Tag]
type Status = T.Text

data Outline = Outline
    { _olTitle    :: T.Text
    , _olStatus   :: Maybe Status
    , _olTags     :: Tags
    , _olText     :: NodeText
    , _olChildren :: [Outline]
    } deriving (Show)

type Header = T.Text

data OrgDoc = OrgDoc
    { _odHeader   :: Header
    , _odOutline  :: Outline
    } deriving (Show)

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
    return $ Outline (T.strip title) status tags text children

tagParser :: Parser Tag
tagParser = do
    char_ ':'
    tag <- many (noneOf " :\n")
    char_ ':'
    return $ T.pack tag

paragraphParser :: Int -> Parser Paragraph
paragraphParser level = T.unlines <$> many line where
    line = do
        replicateM_ (level + 1) $ char_ ' '
        ws <- tilleol
        newline_
        return ws

nodeTextParser :: Int -> Parser NodeText
nodeTextParser level = sepBy (paragraphParser level) (many1 newline_)

headerParser :: Parser Header
headerParser = T.concat <$> sepBy hline newline_ where
    hline = option "" (char_ '#' >> tilleol)

orgDocParser :: [Status] ->  Int -> Parser OrgDoc
orgDocParser userStatus level = OrgDoc <$> headerParser <*> outlineParser userStatus level

parseOrgDoc :: [Status] -> String -> T.Text -> Either ParseError OrgDoc
parseOrgDoc userStatus srcName inp = parse (orgDocParser userStatus 1) srcName inp

-- main = do
--     (src:_) <- getArgs
--     t <- TIO.readFile src
--     case parseOrgDoc ["TODO"] src t of
--         Left err -> print err
--         Right val -> print $ _odOutline val
