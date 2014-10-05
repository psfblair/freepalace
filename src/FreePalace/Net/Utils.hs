module FreePalace.Net.Utils where

import Data.Monoid

infixr 4 .++
(.++) :: Monoid m => m -> m -> m
(.++) = mappend

