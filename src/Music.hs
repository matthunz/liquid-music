module Music where

import Data.Int (Int8)
import Data.Word (Word8)

data Pitch = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B
  deriving (Enum)

{-@ word :: Pitch -> {v:Word8 | v >= 0 && v <= 11} @-}
word :: Pitch -> Word8
word p = case p of
  C -> 0
  Db -> 1
  D -> 2
  Eb -> 3
  E -> 4
  F -> 5
  Gb -> 6
  G -> 7
  Ab -> 8
  A -> 9
  Bb -> 10
  B -> 11

{-@ type MidiNote = {w:Word8 | isMidiNote w} @-}

{-@ inline isMidiNote @-}
isMidiNote :: Word8 -> Bool
isMidiNote w = w <= 127

{-@ midiNote :: Pitch -> Octave -> MidiNote @-}
midiNote :: Pitch -> Int8 -> Word8
midiNote p o = word p + fromIntegral (o + 1) * 12

{-@ type Octave = {w:Int8 | isOctave w} @-}

{-@ inline isOctave @-}
isOctave :: Int8 -> Bool
isOctave i = i >= -1 && i <= 8
