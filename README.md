# advent2023

input

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

choose line + up/down

.$........
..35.+633.
......#...

create symbols from all lines
.s...ss...

spread symbols

s...ss.... prev
.s...ss... curr
..s...ss.. next

merge symbols

sss..sss.. merged
..35..633. digits

merge digits with symbols

..s3d5..s6s3d3...

-- d1
-- d2
pDigit = char 'd' <&> digit

-- s1
-- s2
pSymbolDigit = char 's' <&> digit

parse symboldigits

sepBy pDot (many pDigit <&> pSymbolDigit <&> many (pDigit <|> pSymbolDigit)) -- or use (not . pDot)


