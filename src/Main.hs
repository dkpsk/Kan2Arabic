{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude as P 
import qualified Data.Attoparsec.Text as A
import Control.Applicative
import Control.Monad
import Data.Text as T

-- 1桁
one :: A.Parser Integer
one = (A.choice $ mkParser <$> P.zip ds [1..]) where
  ds :: [Char]
  ds = ['一','二','三','四','五','六','七','八','九']
  mkParser :: (Char, Integer) -> A.Parser Integer
  mkParser (c, i) = A.char c *> pure i

-- 桁を考慮しない
simpleParser :: A.Parser [Integer]
simpleParser = A.many1 (zero <|> one) where
  zero :: A.Parser Integer
  zero = A.char '〇' *> pure 0

-- 基本の4桁 X千X百X十X where X := one
basicParser :: A.Parser Integer
basicParser = (+) <$> (digitToInt <$> sequence parsers) <*> (one <|> pure 0) where
  digitToInt :: [Integer] -> Integer
  digitToInt = sum . P.zipWith (*) [1000,100,10] 
  parsers :: [A.Parser Integer]
  parsers = mkParser <$> ['千', '百', '十']
  mkParser :: Char -> A.Parser Integer
  mkParser c = A.choice [A.char c *> (pure 1), one <* A.char c] <|> pure 0

digits :: [Text]
digits =  ["万","億","兆","京","垓","𥝱","穰","溝","澗","正","載","極","恒河沙","阿僧祇","那由他","不可思議","無量大数"]

-- 5桁以上の数字 X兆X億X万 where X := basic
moreParser :: A.Parser Integer
moreParser = moreParser' digits

moreParser' :: [Text] -> A.Parser Integer
moreParser' ds = fmap sum $ (sequence.P.reverse) (f <$> P.zip digits [4,8..]) where
  f :: (Text, Integer) -> A.Parser Integer
  f (t, i) = (*(10^i)) <$> mkParser t
  parsers :: [A.Parser Integer]
  parsers = mkParser <$> ds
  mkParser :: Text -> A.Parser Integer
  mkParser s = basicParser <* A.string s <|> pure 0

parse :: Text -> Either String Text
parse s = let digital' = (+) <$> moreParser <*> basicParser
              digital = T.pack.show <$> digital' <* A.endOfInput
              simple = T.concat . fmap (T.pack.show) <$> simpleParser <* A.endOfInput
              parser = simple <|> digital
          in A.parseOnly parser s

runtests :: [Text] -> [Either String Text]
runtests tests = parse <$> tests

testcase :: [Text]
testcase = ["二〇三四", "一億二千七百十一万四十七", "四京二千三百十億八千十万百七", "一那由他"]

main :: IO()
main = print $ runtests testcase
