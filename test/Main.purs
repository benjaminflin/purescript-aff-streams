module Test.Main where

import Prelude
import Control.MonadPlus ((<|>), empty)
import Data.AffStream (fromCallback, fromFoldable, recv, send, singleton, take, (<?>), (>>-), scan)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] streamSpec

streamSpec :: Spec Unit
streamSpec = do
  describe "aff stream" do
    describe "creation" do
      it "can be created from a foldable" do
        let
          s = fromFoldable [ 1, 2, 3 ]
        take 3 s >>= shouldEqual [ 1, 2, 3 ]
      it "can be created from a callback" do
        let
          s =
            fromCallback \emit -> do
              emit 1
              emit 2
              emit 3
        take 3 s >>= shouldEqual [ 1, 2, 3 ]
      it "can be created as a singleton" do
        let
          s = singleton 1
        take 1 s >>= shouldEqual [ 1 ]
    describe "functor/filter" do
      it "should map identity arrow" do
        let
          s = fromFoldable [ 1, 2, 3 ]
        l <- take 3 (map identity s)
        l' <- take 3 (identity s)
        l `shouldEqual` l'
      it "should map composed arrows" do
        let
          s = fromFoldable [ 1, 2, 3 ]

          f = (_ * 2)

          g = (_ + 5)
        l <- take 3 <<< (g <$> _) <<< (f <$> _) $ s
        l' <- take 3 <<< ((g <<< f) <$> _) $ s
        l `shouldEqual` l'
      it "should filter out values correctly" do
        let
          s = fromFoldable [ 1, 2, 3 ]

          s' = (_ < 3) <?> s
        take 2 s' >>= shouldEqual [ 1, 2 ]
    describe "applicative" do
      it "should combine latest" do
        let
          s = fromFoldable [ 1, 2, 3 ]

          s' = fromFoldable [ 4 ]

          s'' = (+) <$> s <*> s'
        take 3 s'' >>= shouldEqual [ 5, 6, 7 ]
    describe "monad/flatMap" do
      it "should follow left identity" do
        let
          s = do
            x <- fromFoldable [ 1 ]
            pure $ x * 2

          s' = fromFoldable [ 1 * 2 ]
        x <- take 1 s
        x' <- take 1 s'
        x `shouldEqual` x'
      it "should follow right identity" do
        let
          s = fromFoldable [ 1 ]

          s' = s >>= pure
        x <- take 1 s
        x' <- take 1 s'
        x `shouldEqual` x'
      it "should be associative" do
        let
          f = pure <<< (_ * 2)

          g = pure <<< (_ + 5)

          m = fromFoldable [ 1 ]

          s = (m >>= f) >>= g

          s' = (m >>= \x -> f x >>= g)
        x <- take 1 s
        x' <- take 1 s'
        x `shouldEqual` x'
    describe "send/recv" do
      it "should send values" do
        let
          s = pure 1

          s' = send 2 s
        take 2 s' >>= shouldEqual [ 1, 2 ]
        take 1 s >>= shouldEqual [ 1 ]
      it "should receive single values" do
        let
          s = fromFoldable [ 1, 2, 3 ]

          s' = do
            x <- recv s
            x' <- recv s
            fromFoldable [ x, x' ]
        take 2 s' >>= shouldEqual [ 1, 1 ]
    describe "monad effect and monad aff" do
      it "should lift effects" do
        ref <- liftEffect $ Ref.new "foo"
        let
          eff = do
            Ref.read ref

          s = liftEffect eff
        take 1 s >>= shouldEqual [ "foo" ]
      it "should lift affs" do
        var <- AVar.new "foo"
        let
          aff = do
            delay $ Milliseconds 100.0
            AVar.read var

          s = liftAff aff
        take 1 s >>= shouldEqual [ "foo" ]
    describe "monadplus" do
      it "should have a well behaved neutral element" do
        let
          s = pure 1
        let
          s' = s <|> empty
          s'' = empty <|> s
        take 1 s' >>= shouldEqual [ 1 ]
        take 1 s'' >>= shouldEqual [ 1 ]
      it "should be associative" do
        let
          emitAfter ms a =
            fromCallback
              ( \emit -> do
                  delay $ Milliseconds ms
                  emit a
              )
          s = emitAfter 50.0 1
          s' = emitAfter 100.0 2
          s'' = emitAfter 150.0 3
          l = (s <|> s') <|> s''

          r = s <|> (s' <|> s'')
        take 3 l >>= shouldEqual [ 1, 2, 3 ]
        take 3 r >>= shouldEqual [ 1, 2, 3 ]
    describe "switchMap" do
      it "should switch when it recieves a new value" do
        let
          emitAfter ms a =
            fromCallback
              ( \emit -> do
                  delay $ Milliseconds ms
                  emit a
              )
          s = fromFoldable [ 1, 2, 3 ]
          s' = s >>- (\x -> emitAfter 100.0 (x * 2))
        take 1 s' >>= shouldEqual [ 6 ]
    describe "scan" do
      it "should keep track of a value across stream emissions" do
        let 
          s = fromFoldable [ 1, 2, 3 ]
        let
          s' = scan (+) 0 s
        take 3 s' >>= shouldEqual [ 1, 3, 6 ]
              
