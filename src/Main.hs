{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (ap)
import qualified Control.Monad.State as S
import qualified Data.Map as M

data DSL next = Get String (String -> next)
              | Set String String next
              | End

instance Functor DSL where
    fmap g (Get n f) = Get n (g . f)
    fmap g (Set n v a) = Set n v (g a)
    fmap g End = End

data Free f a = Free (f (Free f a)) | Return a

instance Functor f => Functor (Free f) where
    fmap g (Return a) = Return $ g a
    fmap g (Free f) = Free ((g <$>) <$> f)

instance Functor f => Applicative (Free f) where
    pure = Return
    (<*>) = ap

instance Functor f => Monad (Free f) where
    Return a >>= g = g a
    Free f >>= g = Free $ (>>= g) <$> f

p1 :: DSL (DSL (DSL next))
p1 = Get "foo" $ \foo -> Set "bar" foo End

p2 :: Free DSL next
p2 = Free (Get "foo" $ \foo -> Free (Set "bar" foo (Free End)))

p3 :: Free DSL next
p3 = do
    foo <- Free $ Get "foo" Return
    Free $ Set "bar" foo (Return ())
    Free End

liftFree :: Functor f => f a -> Free f a
liftFree action = Free (Return <$> action)

get :: String -> Free DSL String
get key = liftFree $ Get key id

set :: String -> String -> Free DSL ()
set key value = liftFree $ Set key value ()

end :: Free DSL next
end = liftFree End

p4 :: Free DSL next
p4 = do
    foo <- get "foo"
    set "bar" foo
    end

copy :: String -> String -> Free DSL ()
copy from to = get from >>= set to

p5 :: Free DSL next
p5 = do
    copy "foo" "bar"
    end

follow :: String -> Free DSL String
follow key = do
    key' <- get key
    get key'

p6 :: Free DSL a
p6 = do
    foo <- follow "foo"
    set "bar" foo
    end

type State = M.Map String String

interpretInState :: Free DSL a -> S.State State ()
interpretInState (Free (Get key next)) = do
    m <- S.get
    let res = m M.! key
    interpretInState $ next res
interpretInState (Free (Set key value next)) = do
    m <- S.get
    let m' = M.insert key value m
    S.put m'
    interpretInState next
interpretInState (Free End) = return ()
interpretInState (Return a) = return ()

safeInterpretInState :: (forall a. Free DSL a) -> S.State State ()
safeInterpretInState = interpretInState

main :: IO ()
main = do
    print $ S.execState (safeInterpretInState p5) (M.singleton "foo" "hello")
    print $ S.execState (safeInterpretInState p6) (M.fromList [("foo", "baz"), ("baz", "hi")])
