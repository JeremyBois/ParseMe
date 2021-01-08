import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck

import Parser

main :: IO ()
main = do
  -- Only QuickCheck
  -- Testing Functor laws
  -- quickCheck (functorCompose :: Fun Char Char -> Fun Char Char -> Parser Char -> Bool)
  -- quickCheck (functorIdentity :: Parser Char -> Bool)
  -- Testing Applicative laws
  -- Testting Monad laws
  -- quickCheck $ property (testParseChar <$> arbitrary <*> arbitrary)

  -- With Hspec
  hspec $ do
    -- Testing character parsing
    describe "Can parse a Character" $ do
      it "Parsing <a> from <a> should be successful" $
        runParser (char 'a') (mkSource "a") `shouldBe` Right (test_mkSrc 1 "", 'a')
      it "Any character can be parsed" $
        property (testParseCharSuccess <$> arbitrary)
      it "Trying to parse any character on empty source return UnexpectedEof" $
        property (testParseCharExhausted <$> arbitrary)
      it "Try parsing different kind of character" $
        withMaxSuccess 200 $ property (testParseChar <$> arbitrary <*> arbitrary)

    -- Testing string parsing
    describe "Can parse a String" $ do
      it "Parsing <Hello> from <Hello> should be successful" $
        runParser (stringP "Hello") (mkSource "Hello")
          `shouldBe` Right (test_mkSrc 5 "", "Hello")
      it "Try parsing different kind of string" $ do
        property (testParseString <$> stringGenerator <*> stringGenerator)

testParseCharSuccess :: Char -> Bool
testParseCharSuccess myChar =
  runParser (char myChar) (mkSource [myChar]) == result
  where
    result = Right (test_mkSrc 1 "", myChar)

testParseCharExhausted :: Char -> Bool
testParseCharExhausted myChar =
  runParser (char myChar) (mkSource "") == if myChar /= '\00' then failure else success
  where
    failure = Left $ Error (test_mkPos 0) UnexpectedEof (Context "EOF")
    success = Right (mkSource "", '\NUL')

testParseChar :: Char -> Char -> Bool
testParseChar asked found =
  runParser (char asked) (mkSource [found]) == result
  where
    result
      | [found] == "" = Left $ Error (test_mkPos 0) UnexpectedEof (Context "EOF")
      | asked /= found =
        Left $
          Error
            (test_mkPos 0)
            (UnexpectedChar (asked, found))
            (Context "Specific Character")
      | otherwise = Right (test_mkSrc 1 "", found)

testParseString :: String -> String -> Bool
testParseString asked found
  | asked == found = parseResult == result
  | null found = parseResult == Left (Error (test_mkPos 0) UnexpectedEof (Context "String"))
  --  Should be updated to test if position and missmatch is correct
  | otherwise = parseResult /= result
  where
    parseResult = runParser (stringP asked) (mkSource found)
    result = Right (test_mkSrc (length asked) "", found)

stringGenerator :: Gen String
stringGenerator = frequency [(10, arbitrary), (5, pure ""), (5, pure "Same string ! hfda 32 q")]

-- testParseChar' :: Char -> Char -> (Parser.Result Char, Parser.Result Char)
-- testParseChar' asked found =
--   (runParser (char asked) (test_mkSrc 0 [found]), result)
--   where
--     result
--       | [found] == "" = Left $ Error (test_mkPos 0) UnexpectedEof (Context "EOF")
--       | asked /= found =
--         Left $
--           Error
--             (test_mkPos 0)
--             (UnexpectedChar (asked, found))
--             (Context "Specific Character")
--       | otherwise = Right (test_mkSrc 1 "", found)

-- FUNCTOR LAWS
-- functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
-- functorIdentity f = fmap id f == f

-- functorCompose :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
-- functorCompose (Fun _ g) (Fun _ f) x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- instance (Arbitrary a) => Arbitrary (Parser a) where
--     arbitrary = Parser <$> arbitrary

-- instance Eq a => EqProp (Parser a) where
--     (=-=) = eq
