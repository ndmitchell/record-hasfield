{-# LANGUAGE FunctionalDependencies
           , TypeApplications
           , DataKinds
  #-}

module Main(main) where

import GHC.Records.Extra

data Foo = Foo {foo :: Int} deriving (Show,Eq)
instance HasField "foo" Foo Int where
    hasField r = (\x -> r{foo=x}, foo r)

data Bar = Bar {bar :: Foo} deriving (Show,Eq)
instance HasField "bar" Bar Foo where
    hasField r = (\x -> r{bar=x}, bar r)

main :: IO ()
main = do
    let a === b = if a == b then putStr "." else fail $ "Mismatch: " ++ show a ++ " /= " ++ show b
    modifyField @'("bar","foo") (Bar $ Foo 12) (*2) === Bar {bar = Foo {foo = 24}}
    getField @"foo" (Foo 3) === 3
    setField @"_1" (1, True) 8 === (8, True)
    putStrLn " success"
