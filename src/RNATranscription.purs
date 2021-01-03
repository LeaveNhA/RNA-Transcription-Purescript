module RNATranscription where

import Data.Traversable(traverse)
import Prelude((<$>))

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)

toRNA' :: Char -> Maybe Char
toRNA' 'C' = Just 'G'
toRNA' 'G' = Just 'C'
toRNA' 'T' = Just 'A'
toRNA' 'A' = Just 'U'
toRNA' _ = Nothing

toRNA :: String -> Maybe String
toRNA dna = fromCharArray <$> traverse toRNA' (toCharArray dna)
