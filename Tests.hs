{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Tests where

-- GHC
import System.Exit
import System.Environment
-- import System.IO.Silently

-- External
import Test.HUnit

-- Lib
import Problem10

--------------------------------------------------------------------------------
-- util
--------------------------------------------------------------------------------

err :: String -> String
err problem = problem ++ " is type-checking, not undefined, and not an infinite loop?\
                         \But you still have something wrong with it."
--------------------------------------------------------------------------------
-- p1
--------------------------------------------------------------------------------

pr1_1' :: Lte1 Z (S (S (S (S Z))))
pr1_1' = pr1_1

pr1_2' :: Lte1 (S (S Z)) (S (S Z))
pr1_2' = pr1_2 

pr1_3' :: Lte1 (S (S Z)) (S (S (S Z)))
pr1_3' = pr1_3


p1 :: Test
p1 = test [
  finite1 pr1_1' == () @? err "pr1_1",
  finite1 pr1_2' == () @? err "pr1_2",
  finite1 pr1_3' == () @? err "pr1_1"
  ]

--------------------------------------------------------------------------------
-- p2
--------------------------------------------------------------------------------

eq2 :: Lte2 m n -> Lte2 m' n' -> Bool
eq2 Done Done = True

pr2_1' :: Lte2 Z (S (S (S (S Z))))
pr2_1' = pr2_1

pr2_2' :: Lte2 (S (S Z)) (S (S Z))
pr2_2' = pr2_2

pr2_3' :: Lte2 (S (S Z)) (S (S (S Z)))
pr2_3' = pr2_3


p2 :: Test
p2 = test [
  fin2_2 pr2_1' == () @? err "pr2_1",
  fin2_2 pr2_2' == () @? err "pr2_2",
  fin2_2 pr2_3' == () @? err "pr2_3"
  ]

--------------------------------------------------------------------------------
-- p3
--------------------------------------------------------------------------------

data Shape = R | I | T Shape Shape
  deriving Eq

shape :: Lte3 m n -> Shape
shape Refl = R
shape (Inst _) = I
shape (Trans xs ys) = T (shape xs) (shape ys)

pr3_1a' :: Lte3 Z (S (S (S (S Z))))
pr3_1a' = pr3_1a

pr3_1b' :: Lte3 Z (S (S (S (S Z))))
pr3_1b' = pr3_1b

pr3_2' :: Lte3 (S (S Z)) (S (S Z))
pr3_2' = pr3_2

pr3_3' :: Lte3 (S (S Z)) (S (S (S Z)))
pr3_3' = pr3_3

p3 :: Test
p3 = test [
  fin3 pr3_1a' == () @? err "pr3_1a",
  fin3 pr3_1b' == () @? err "pr3_1b",
  shape pr3_1a' /= shape pr3_1b'
    @? "pre3_1a and pre3_1b should have different implementations.",
  fin3 pr3_2' == () @? err "pr3_2",
  fin3 pr3_3' == () @? err "pr3_3"
  ]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

argMap :: Int -> Test
argMap 1 = p1
argMap 2 = p2
argMap 3 = p3
argMap _ = test [p1, p2, p3]

hd :: [a] -> Maybe a
hd (x : _) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  let tests = case read <$> (hd args) of
                Just x -> argMap x
                Nothing -> argMap 42
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
