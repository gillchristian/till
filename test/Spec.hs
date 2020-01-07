import Data.Either (isLeft)
import Lib
import Test.Hspec
import qualified Text.Parsec as P

(<=>) :: (Show a, Eq a) => a -> a -> Expectation
(<=>) = shouldBe

successParse :: String -> Matcher -> Expectation
successParse input expected =
  either (error . show) (expected <=>) $ P.parse matcher input input

failParse :: String -> Expectation
failParse input = shouldSatisfy (P.parse matcher input input) isLeft

main :: IO ()
main = hspec $ do
  describe "Till.match" $ do
    it "ignores empty line" $
      match undefined "" <=> False
    describe "Pattern" $ do
      it "matches on substring" $ do
        match (Pattern "foo") "football" <=> True
        match (Pattern "ball") "football" <=> True
        match (Pattern "tba") "football" <=> True
      it "matches case sensitive" $ do
        match (Pattern "FOO") "football" <=> False
        match (Pattern "BALL") "football" <=> False
    describe "NonPattern"
      $ it "does not match on substring"
      $ do
        match (NonPattern "foo") "football" <=> False
        match (NonPattern "ball") "football" <=> False
        match (NonPattern "tba") "football" <=> False
        match (NonPattern "...") "football" <=> True
    describe "And" $ do
      it "checks both sides" $ do
        match (And (Pattern "foo") (Pattern "ball")) "football" <=> True
        match (And (Pattern "foo") (Pattern "ball")) "football" <=> True
        match (And (Pattern "...") (Pattern "ball")) "football" <=> False
      it "matches nested cases" $
        match (And (Pattern "foo") (And (Pattern "tba") (Pattern "ball"))) "football" <=> True
    describe "Or" $ do
      it "checks one side" $ do
        match (Or (Pattern "foo") (Pattern "ball")) "football" <=> True
        match (Or (Pattern "...") (Pattern "ball")) "football" <=> True
        match (Or (Pattern "foo") (Pattern "...")) "football" <=> True
        match (Or (Pattern "...") (Pattern "...")) "football" <=> False
      it "matches nested cases" $ do
        match (Or (Pattern "...") (Or (Pattern "...") (Pattern "ball"))) "football" <=> True
        match (And (Pattern "...") (Or (Pattern "...") (Pattern "ball"))) "football" <=> False
  describe "Till.matcher" $ do
    it "succedes on a valid expression" $ do
      successParse "foo" (Pattern "foo")
      successParse " foo" (Pattern "foo") -- ignores whitespace
      successParse "!foo" (NonPattern "foo")
      successParse "'foo and ball'" (Pattern "foo and ball")
      successParse "\"foo and ball\"" (Pattern "foo and ball")
      successParse "'a | b'" (Pattern "a | b")
      successParse "foo & ball" (And (Pattern "foo") (Pattern "ball"))
      successParse "foo | ball" (Or (Pattern "foo") (Pattern "ball"))
      successParse "foo | ball | tba" (Or (Or (Pattern "foo") (Pattern "ball")) (Pattern "tba"))
      successParse "foo | ball & tba" (Or (Pattern "foo") (And (Pattern "ball") (Pattern "tba")))
    it "fails on invalid expression" $ do
      failParse "foo foo"
      failParse "'foo' foo"
      failParse "'foo' '"
      failParse "\"foo\" \""
      failParse "'foo''"
      failParse "\"foo\"\""
      failParse "foo && bar"
      failParse "foo |& bar"
      failParse "foo &"
      failParse "foo |"
  describe "Till.runMatchers" $ do
    let foo = Pattern "foo"
    let bar = Pattern "bar"
    let none = Pattern "..."
    let matchers = [foo, bar, none]
    let fooMatch = "football is boring, but it matches"
    let barMatch = "John went to a bar, and it matches"
    let bothMatch = [fooMatch, barMatch]
    let noMatch =
          [ "No matching this line",
            "No matching this line either",
            "Neither this line"
          ]
    let lns = bothMatch ++ noMatch
    it "matches on empty matchers list" $
      runMatchers [] lns <=> True
    it "does not match on empty lines list" $
      runMatchers matchers [] <=> False
    it "does not match when none of the matchers match" $ do
      runMatchers matchers noMatch <=> False
      runMatchers [foo, none] (barMatch : noMatch) <=> False
      runMatchers [bar, none] (fooMatch : noMatch) <=> False
      runMatchers [And foo bar] (fooMatch : noMatch) <=> False
      runMatchers [And foo bar] (barMatch : noMatch) <=> False
      runMatchers [And foo bar] bothMatch <=> False -- the same line DOEST NOT match both
    it "matches when at least one of the matchers matches" $ do
      runMatchers matchers bothMatch <=> True
      runMatchers matchers lns <=> True
      runMatchers [foo, none] (fooMatch : noMatch) <=> True
      runMatchers [bar, none] (barMatch : noMatch) <=> True
      runMatchers [And foo bar] [fooMatch ++ barMatch] <=> True -- the same line DOES match both
      runMatchers [Or foo bar] [fooMatch] <=> True
      runMatchers [Or foo bar] [barMatch] <=> True
