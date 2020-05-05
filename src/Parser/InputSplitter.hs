module Parser.InputSplitter
  ( splitLine
  ) where

import Control.Applicative ((<|>), empty, Alternative)
import Data.Char (isSpace)

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f p = Parser fn
    where fn s = runParser p s >>= \(a, s') -> return (f a, s')

instance Applicative (Parser s) where
  pure a = Parser { runParser = Just . ((,) a) }
  fp <*> p = Parser fn   where
    fn s = do
      (f, s' ) <- runParser fp s
      (a, s'') <- runParser p s'
      return (f a, s'')

instance Alternative (Parser s) where
  empty   = Parser (\_ -> Nothing)
  p <|> q = Parser fn where
    fn s = runParser p s <|> runParser q s

instance Monad (Parser s) where
  return = pure
  p >>= f = Parser fn   where
    fn s = do
      (a, s') <- runParser p s
      runParser (f a) s'

-- | Never fails and change the input.
ok :: Parser s ()
ok = Parser (\s -> return ((), s))

-- | Checks that the input is over.
eof :: Parser s ()
eof = Parser checkEof
 where
  checkEof s | null s    = return ((), s)
             | otherwise = empty

-- | Checks the first element with the predicate.
satisfy :: (s -> Bool) -> Parser s s
satisfy pred = Parser fn where
  fn []       = Nothing
  fn (x : xs) = if pred x
                then return (x, xs)
                else Nothing

-- | Parse the first element of the stream.
element :: Eq s => s -> Parser s s
element s = satisfy ((==) s)

-- | Applies p zero or more times.
many :: Parser s a -> Parser s [a]
many p = (:) <$> p <*> many p <|> pure []

-- | Applies p at least once.
many1 :: Parser s a -> Parser s [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])

-- | Parse a character which is not equal to quot.
noQuot :: Parser Char Char
noQuot = satisfy (not . (==) '\"')

-- | Characters which don't contain quote inside.
insideQuots :: Parser Char String
insideQuots = many noQuot

-- | Parse quote character.
quoteSign :: Parser Char Char
quoteSign = element '\"'

-- | Parse text in quote.
quote :: Parser Char String
quote = quoteSign *> insideQuots <* quoteSign

-- | Parse a character which is not quote or space.
textChar :: Parser Char Char
textChar = satisfy (\c -> not (isSpace c) && not (c == '\"'))

-- | Parse block of text without spaces and quote
text :: Parser Char String
text = many1 textChar

-- | Parse space character.
space :: Parser Char ()
space = satisfy isSpace *> ok

-- | Parse block of spaces.
spaces :: Parser Char ()
spaces = many space *> ok

-- | Extracts text in quote and splits by spaces.
input :: Parser Char [String]
input = many (spaces *> text <|> spaces*> quote) <* spaces <* eof

-- | Nice splitter which is silly but better than splitting by spaces.
-- It was too late when I realized that something like "blah\"" is possible and valid,
-- sorry for that.
-- Please, don't torture it and don't use quote inside quote, it won't do what you want anyway.
splitLine :: String -> Either String [String]
splitLine line = case runParser input line of
  Just (res, _) -> Right res
  Nothing       -> Left "Error: Molodoy chelovek, kavychku zakroyte za soboy"