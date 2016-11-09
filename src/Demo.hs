module Demo (demo, test) where

import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass

demo :: Pretty a => String -> a -> IO ()
demo str x = putStrLn . render $ text str <+> text "=" <+> pPrint x $$ text ""

test :: Testable a => String -> a -> IO ()
test str p = putStr (str ++ ": ") >> quickCheck p
