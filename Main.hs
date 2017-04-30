type L = [Char]
type S = [Char]
type A = [Char]
type B = [Char]
type C = [Char]

data TextEdit = TextEdit L S A B deriving (Show)

getLeftCharacters:: TextEdit -> L
getLeftCharacters (TextEdit l _ _ _) = l

setLeftCharacters:: TextEdit -> L -> TextEdit
setLeftCharacters (TextEdit _ s a b) x = (TextEdit x s a b)

getSelectedCharacters:: TextEdit -> S
getSelectedCharacters (TextEdit _ s _ _) = s

setSelectedCharacters:: TextEdit -> S-> TextEdit
setSelectedCharacters (TextEdit l _ a b) y = (TextEdit l y a b)

getRightCharacters:: TextEdit -> A
getRightCharacters (TextEdit _ _ a _) = a

setRightCharacters:: TextEdit -> A -> TextEdit
setRightCharacters (TextEdit l s _ b) z = (TextEdit l s z b)

getCopyBufferCharacters:: TextEdit -> B
getCopyBufferCharacters (TextEdit _ _ _ b) = b

setCopyBufferCharacters:: TextEdit -> B -> TextEdit
setCopyBufferCharacters (TextEdit l s a _) j = (TextEdit l s a j)

insertCharacter:: TextEdit -> C -> TextEdit
insertCharacter (TextEdit l _ a b) c = (TextEdit (l ++ [head c]) [] a b)

backspace :: TextEdit -> TextEdit
backspace (TextEdit l s a b) = (TextEdit(reverse (tail (reverse l))) [] a b)
