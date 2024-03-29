type L = [Char]
type S = [Char]
type A = [Char]
type B = [Char]
type C = [Char]

data TextEdit = TextEdit L S A B deriving (Show)
t = (TextEdit "sdad" "dsad" "oppj" "dsadasd")
e = (TextEdit "sdad" "" "oppj" "dsadasd")
f = (TextEdit "sdad" "" "oppj" "")
o = (TextEdit "I am a very " "" "good boi" "")


create :: TextEdit
create = (TextEdit [] [] [] [])

init :: TextEdit -> TextEdit
init (TextEdit _ _ _ _)= (TextEdit [] [] [] [])

getLeftCharacters:: TextEdit -> L
getLeftCharacters (TextEdit l _ _ _) = l

setLeftCharacters:: TextEdit -> L -> TextEdit
setLeftCharacters (TextEdit _ s a b) x = (TextEdit x s a b)

getRightCharacters:: TextEdit -> A
getRightCharacters (TextEdit _ _ a _) = a

setRightCharacters:: TextEdit -> A -> TextEdit
setRightCharacters (TextEdit l s _ b) z = (TextEdit l s z b)

getSelectedCharacters:: TextEdit -> S
getSelectedCharacters (TextEdit _ s _ _) = s

setSelectedCharacters:: TextEdit -> S-> TextEdit
setSelectedCharacters (TextEdit l _ a b) y = (TextEdit l y a b)

getCopyBufferCharacters:: TextEdit -> B
getCopyBufferCharacters (TextEdit _ _ _ b) = b

setCopyBufferCharacters:: TextEdit -> B -> TextEdit
setCopyBufferCharacters (TextEdit l s a _) j = (TextEdit l s a j)

insertCharacter:: TextEdit -> Char -> TextEdit
insertCharacter (TextEdit l s a b) c
    |length (l ++ a) <= 1024 = (TextEdit (l ++ [c]) [] a b)
    |otherwise = (TextEdit l s a b)

backspace :: TextEdit -> TextEdit
backspace (TextEdit l s a b)
    | s == [] = (TextEdit(reverse (tail (reverse l))) [] a b)
    | l == [] = (TextEdit [] s a b)
    | otherwise = (TextEdit l [] a b)

delete :: TextEdit -> TextEdit
delete (TextEdit l s a b)
    | s == [] = (TextEdit l s (tail a) b)
    | a == [] = (TextEdit l s [] b)
    | otherwise = (TextEdit l [] a b)
copy :: TextEdit -> TextEdit
copy (TextEdit l s a b)  
    | s == [] = (TextEdit l s a b)
    |otherwise = (TextEdit l s a s)

paste :: TextEdit -> TextEdit
paste (TextEdit l s a b)
    |length (l ++ a ++ b) <= 1024 = (TextEdit (l ++ b) [] a b)
    |otherwise = (TextEdit l s a b)

cut :: TextEdit -> TextEdit
cut (TextEdit l s a b)
    |s == [] = (TextEdit l s a b)
    |otherwise = (TextEdit l [] a s)

moveLeft :: TextEdit -> TextEdit
moveLeft (TextEdit l _ a b)
    |l == [] = (TextEdit [] [] a b)
    |otherwise = (TextEdit (reverse (tail (reverse l))) [] ([(head(reverse l))]++a) b)

moveRight :: TextEdit -> TextEdit
moveRight (TextEdit l _ a b)
    |a == [] = (TextEdit l [] [] b)
    |otherwise = (TextEdit (l ++ [(head a)]) [] (tail a) b)

moveWordLeft :: TextEdit -> TextEdit
moveWordLeft (TextEdit l s a b) = move(moveLeft(TextEdit l s a b)) where
 move(TextEdit l s a b)
  |l == [] = (TextEdit [] s a b)
  |((head (reverse l)) == ' ' && (head a) /= ' ') = (TextEdit l s a b)
  |otherwise = moveWordLeft (moveLeft (TextEdit l s a b))

moveWordRight :: TextEdit -> TextEdit
moveWordRight (TextEdit l s a b) = move(moveRight(TextEdit l s a b)) where
 move(TextEdit l s a b)
  |a == [] = (TextEdit l s [] b)
  |((head a) /= ' ' && (last l) == ' ') = (TextEdit l s a b)
  |otherwise = moveWordRight (moveRight (TextEdit l s a b))

selectLeft :: TextEdit -> TextEdit
selectLeft (TextEdit l s a b) = (TextEdit (reverse (tail (reverse l))) ([(head(reverse l))]++s) a b)

selectRight :: TextEdit -> TextEdit
selectRight (TextEdit l s a b) = (TextEdit (l) (s ++ [(head a)]) (tail a) b)

selectWordLeft :: TextEdit -> TextEdit
selectWordLeft (TextEdit l s a b) = select(selectLeft (TextEdit l s a b)) where
  select(TextEdit l s a b)
    |l == [] = (TextEdit [] s a b)
    |((head (reverse l)) == ' ' && (head s) /= ' ') = (TextEdit l s a b)
    |otherwise = selectWordLeft (selectLeft (TextEdit l s a b))

selectWordRight :: TextEdit -> TextEdit
selectWordRight (TextEdit l s a b) = select(selectRight (TextEdit l s a b)) where
 select(TextEdit l s a b)
  |a == [] = (TextEdit l s [] b)
  |((head a) /= ' ' && (last s) == ' ') = (TextEdit l s a b)
  |otherwise = selectWordRight (selectRight (TextEdit l s a b))

selectAllLeft :: TextEdit -> TextEdit
selectAllLeft (TextEdit l s a b) = (TextEdit [] (l++s) a b)

selectAllRight :: TextEdit -> TextEdit
selectAllRight (TextEdit l s a b) = (TextEdit l (s++a) [] b)

selectAll :: TextEdit -> TextEdit
selectAll (TextEdit l s a b) = (TextEdit [] (l++s++a) [] b)

home :: TextEdit -> TextEdit
home (TextEdit l s a b) = (TextEdit [] [] (l++s++a) b)

end :: TextEdit -> TextEdit
end (TextEdit l s a b) = (TextEdit (l++s++a) [] [] b)
