module Util.Unicode where

import Data.Char
import Numeric

-- | Unicode Little box 
box = '\x2B1C'
memberOf = '\x2208'
notMemberOf = '\x2209'
because = '\x2235'
assign = '\x2254'
check = '\x2713'
emptySet = '\x2205'

startItalic = fst $ head $ readHex "1D434"

upperA = 65

-- | Map character of Arabic alphabet to italic script character, using unicode code point 'startItalic' as \'A\'
italic :: Char -> Char
italic c = chr $ (ord c - upperA) + startItalic

-- | Convert 'Char' to 'String' by creating a singleton list. (I don't know if this is a good idea or not.)
c2s :: Char -> String 
c2s c = [c]