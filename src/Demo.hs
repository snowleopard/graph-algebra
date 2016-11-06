module Demo (demo) where

import Data.List
import Text.PrettyPrint.HughesPJClass

demo :: Pretty a => [(String, a)] -> IO ()
demo items = putStrLn . render . vcat $ intersperse (text "") docs
  where
    docs = map (\(str, item) -> hsep [ text str, text "=", pPrint item]) items
