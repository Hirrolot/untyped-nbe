import Lib
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = do
  defaultMain (testGroup "Unit tests" [evalTests, quoteTests, normalizeTests])

evalTests :: TestTree
evalTests = testGroup "eval" [evalVars, evalLams, evalAppls]

evalVars :: TestTree
evalVars = testCase
  "Evaluate variables"
  $ do
    assertEqual "A variable 0" (eval [vvar 123] $ TVar 0) (vvar 123)
    assertEqual
      "A variable 1"
      (eval [vvar 0, VClosure [vvar 123] $ TVar 0] $ TVar 1)
      (VClosure [vvar 123] $ TVar 0)
    assertEqual
      "A variable 2"
      (eval [vvar 0, vvar 1, vappl (NVar 123) (vvar 42)] $ TVar 2)
      (vappl (NVar 123) (vvar 42))

idTerm :: Term
idTerm = TLam $ TVar 0

idValue :: Value
idValue = VClosure [] $ TVar 0

evalLams :: TestTree
evalLams = testCase
  "Evaluate lambdas"
  $ do
    assertEqual "An identity closure" (eval [] idTerm) idValue
    assertEqual
      "Don't evaluate under a binder"
      (eval [] $ TLam $ TAppl idTerm (TVar 0))
      (VClosure [] $ TAppl idTerm (TVar 0))

evalAppls :: TestTree
evalAppls = testCase
  "Evaluate applications"
  $ do
    assertEqual
      "A reducible application"
      (eval [vvar 123] $ TAppl idTerm (TVar 0))
      (vvar 123)
    assertEqual
      "A reducible application with a reducible arg"
      (eval [vvar 123] $ TAppl idTerm (TAppl idTerm (TVar 0)))
      (vvar 123)
    assertEqual
      "A reducible application with a closure result"
      (eval [vvar 123] $ TAppl (TLam $ TLam $ TVar 1) (TVar 0))
      (VClosure [vvar 123, vvar 123] $ TVar 1)

    assertEqual
      "A neutral application"
      (eval [vvar 123, vvar 42] $ TAppl (TVar 0) (TVar 1))
      (vappl (NVar 123) (vvar 42))
    assertEqual
      "A neutral application with a reducible arg"
      (eval [vvar 123, vvar 42] $ TAppl (TVar 0) (TAppl idTerm (TVar 1)))
      (vappl (NVar 123) (vvar 42))

    assertEqual
      "Nested applications"
      ( eval [vvar 123] $
          TAppl
            ( TAppl
                ( TAppl
                    (TLam $ TLam $ TLam $ TVar 0)
                    (TVar 0)
                )
                (TVar 0)
            )
            (TVar 0)
      )
      (vvar 123)

quoteTests :: TestTree
quoteTests = testCase
  "quote"
  $ do
    assertEqual "An identity lambda" (quote 0 idValue) idTerm
    assertEqual
      "A nested lambda"
      (quote 0 $ VClosure [] $ TLam $ TLam $ TVar 2)
      (TLam $ TLam $ TLam $ TVar 2)

    assertEqual
      "A lambda with reducible body"
      (quote 0 $ VClosure [] $ TAppl idTerm (TVar 0))
      (TLam $ TVar 0)
    assertEqual
      "A lambda with neutral body"
      ( quote 0 $
          VClosure [] $
            TLam $
              TAppl (TVar 0) (TAppl (TVar 1) idTerm)
      )
      (TLam $ TLam $ TAppl (TVar 0) (TAppl (TVar 1) idTerm))

    assertEqual
      "A complex lambda"
      ( quote 0 $
          VClosure [] $
            TAppl
              ( TLam $
                  TLam $
                    TAppl (TLam $ TLam $ TAppl (TLam idTerm) (TVar 0)) (TVar 0)
              )
              (TVar 0)
      )
      (TLam $ TLam $ TLam idTerm)

normalizeTests :: TestTree
normalizeTests = testCase
  "normalize"
  $ do
    let value = TLam $ TLam $ TAppl (TVar 1) (TVar 0)
     in assertEqual "Normalize a value" (normalize 0 [] value) value

    let s = TLam $ TLam $ TLam $ TAppl (TAppl (TVar 2) (TVar 0)) (TAppl (TVar 1) (TVar 0))
        k = TLam $ TLam $ TVar 1
        i = TLam $ TVar 0
     in do
          assertEqual "SKK = I" (normalize 0 [] $ TAppl (TAppl s k) k) i
          assertEqual "KSK = S" (normalize 0 [] $ TAppl (TAppl k s) k) s
          assertEqual "SKSK = K" (normalize 0 [] $ TAppl (TAppl (TAppl s k) s) k) k
