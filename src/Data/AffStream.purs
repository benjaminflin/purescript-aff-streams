module Data.AffStream where

import Prelude
import Control.MonadPlus (class Alt, class Alternative, class MonadPlus, class MonadZero, class Plus, guard)
import Control.Monad.Rec.Class (class MonadRec, Step(..), forever, tailRecM)
import Control.Parallel (parOneOf)
import Data.Array (snoc, (..))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, forkAff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (empty, new, put, take, read) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

newtype Stream a
  = Stream (Aff (AVar a))

derive instance newtypeStream :: Newtype (Stream a) _

instance functorStream :: Functor Stream where
  map f s = (pure <<< f) =<< s

instance applyStream :: Apply Stream where
  apply = ap

instance applicativeStream :: Applicative Stream where
  pure = wrap <<< AVar.new

instance bindStream :: Bind Stream where
  bind s f =
    fromCallback
      ( \emit -> do
          var <- unwrap s
          void <<< forkAff <<< forever $ (subscribe emit <<< f) =<< AVar.take var
      )

instance monadStream :: Monad Stream

instance altStream :: Alt Stream where
  alt = merge 

instance plusStream :: Plus Stream where
  empty = empty

instance alternativeStream :: Alternative Stream
instance monadZero :: MonadZero Stream
instance monadPlus :: MonadPlus Stream

instance monadEffectStream :: MonadEffect Stream where
  liftEffect eff = wrap $ AVar.new =<< liftEffect eff

instance monadAffStream :: MonadAff Stream where
  liftAff aff = wrap $ AVar.new =<< aff

take :: forall a. Int -> Stream a -> Aff (Array a)
take n s = do
  var <- unwrap s
  for (1 .. n) $ const (AVar.take var)

takeUntil :: forall a. (a -> Boolean) -> Stream a -> Aff (Array a)
takeUntil f s = do
  var <- unwrap s
  tailRecM go (Tuple var [])
  where
  go (Tuple var acc) = do
    val <- AVar.take var
    pure $ if f val then Loop $ Tuple var (snoc acc val) else Done acc

subscribe :: forall a. (a -> Aff Unit) -> Stream a -> Aff Unit
subscribe f s = void <<< forkAff $ consume f s

consume :: forall a m. MonadAff m => MonadRec m => (a -> m Unit) -> Stream a -> m Unit
consume f s =
  void do
    var <- liftAff $ unwrap s
    forever (f =<< liftAff (AVar.take var))

send :: forall a. a -> Stream a -> Stream a
send x s =
  wrap do
    var <- unwrap s
    void <<< forkAff $ AVar.put x var
    pure var

-- Creates a stream with only the latest value
recv :: forall a. Stream a -> Stream a
recv s =
  wrap do
    var <- unwrap s
    val <- AVar.read var
    AVar.new val

fromCallback :: forall a. ((a -> Aff Unit) -> Aff Unit) -> Stream a
fromCallback f =
  wrap do
    var <- AVar.empty
    void <<< forkAff <<< f $ (flip AVar.put) var
    pure var

singleton :: forall a. a -> Stream a
singleton = pure

empty :: forall a. Stream a
empty = wrap $ AVar.empty

fromFoldable :: forall a f. Foldable f => f a -> Stream a
fromFoldable f = foldl folder empty f
  where
  folder s x =
    wrap do
      var <- unwrap s
      void $ forkAff $ AVar.put x var
      pure var

merge :: forall a. Stream a -> Stream a -> Stream a
merge a b = flatten $ fromFoldable [ a, b ]

scan :: forall a b. (b -> a -> b) -> b -> Stream a -> Stream b
scan f x0 s = fromCallback $ \emit -> do
    v <- unwrap s
    tailRecM go { x: x0, emit, v }
    where
    go { x, emit, v } = do
        a <- AVar.take v 
        let b = f x a
        emit b
        pure $ Loop { x: b, emit, v }

flatten :: forall a. Stream (Stream a) -> Stream a
flatten s = s >>= identity

filter :: forall a. (a -> Boolean) -> Stream a -> Stream a
filter f s = do
  x <- s
  guard $ f x  
  pure x 

filterFlipped :: forall a. Stream a -> (a -> Boolean) -> Stream a
filterFlipped = flip filter

infixl 4 filter as <?>

infixr 4 filterFlipped as <:>

switchMap :: forall a b. Stream a -> (a -> Stream b) -> Stream b
switchMap s f =
  fromCallback
    ( \emit -> do
        var <- unwrap s
        val <- AVar.take var
        var' <- unwrap (f val)
        switch emit var var'
    )
  where
  switch emit var =
    tailRecM \var' -> do
      val <- parOneOf [ (pure <<< Left) =<< AVar.take var, (pure <<< Right) =<< AVar.take var' ]
      case val of
        Left v -> do
          (pure <<< Loop) =<< (unwrap $ f v)
        Right v -> do
          emit v
          pure (Loop var')

switchMapFlipped :: forall a b. (a -> Stream b) -> Stream a -> Stream b
switchMapFlipped = flip switchMap

infixl 1 switchMap as >>-

infixr 1 switchMapFlipped as -<<
