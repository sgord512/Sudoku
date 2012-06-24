module Util.String where

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Enum, Bounded)

escapeString n = "\ESC[" ++ (show n) ++ "m"

blackNum = 30

colorStringThenSwitchToColor :: Color -> Color -> String -> String
colorStringThenSwitchToColor highlight standard str = (escapeString $ blackNum + fromEnum highlight) ++ str ++ (escapeString $ blackNum + fromEnum standard)

color :: Color -> String -> String
color c str = colorStringThenSwitchToColor c White str
                                                       