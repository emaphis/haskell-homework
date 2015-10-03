-- | Exercises from Chapter 2

module UsePictures where
import PicturesSVG

-- Ex 2.1  define this module

-- Ex 2.2
pic2_2a, pic2_2b, row1, board1 :: Picture

pic2_2a =
  above (beside white black) (beside black white)

pic2_2b =
  beside (above white black) (above black white)

row1 =
  beside (beside pic2_2a pic2_2a) (beside pic2_2a pic2_2a)

board1 =
  above (above row1 row1) (above row1 row1)

-- Ex 2.3 Horse variations
var1, var2, var3, var4 :: Picture

var1 = above
       (beside horse (invertColour horse))
       (beside (invertColour horse) horse)

var2 = above
       (beside horse (invertColour horse))
       (beside (flipV (invertColour horse)) (flipV horse))

var3 = above
       (beside horse (invertColour horse))
       (beside (flipH (flipV (invertColour horse)))
        (flipH (flipV horse)))

-- Ex 2.4  Another variation

var4 = above
       (beside horse (invertColour horse))
       (beside (flipH (invertColour horse)) (flipH horse))
