module Zwerg.Data.ZColor where

import Prelude (Eq, Double, fromIntegral, round, ($), (*))
import GHC.Generics as EXPORTED (Generic)
import Data.Binary as EXPORTED (Binary)
import Data.Word (Word8)

data ZColor = ZColor Word8 Word8 Word8
    deriving (Eq, Generic)
instance Binary ZColor

darken :: Double -> ZColor -> ZColor
darken scale (ZColor r g b) =
    ZColor
    (round $ fromIntegral r * scale)
    (round $ fromIntegral g * scale)
    (round $ fromIntegral b * scale)

aliceblue :: ZColor
aliceblue = ZColor 240 248 255

antiquewhite :: ZColor
antiquewhite = ZColor 250 235 215

aqua :: ZColor
aqua = ZColor 0 255 255

aquamarine :: ZColor
aquamarine = ZColor 127 255 212

azure :: ZColor
azure = ZColor 240 255 255

beige :: ZColor
beige = ZColor 245 245 220

bisque :: ZColor
bisque = ZColor 255 228 196

black :: ZColor
black = ZColor 0 0 0

blanchedalmond :: ZColor
blanchedalmond = ZColor 255 235 205

blue :: ZColor
blue = ZColor 0 0 255

blueviolet :: ZColor
blueviolet = ZColor 138 43 226

brown :: ZColor
brown = ZColor 165 42 42

burlywood :: ZColor
burlywood = ZColor 222 184 135

cadetblue :: ZColor
cadetblue = ZColor 95 158 160

chartreuse :: ZColor
chartreuse = ZColor 127 255 0

chocolate :: ZColor
chocolate = ZColor 210 105 30

coral :: ZColor
coral = ZColor 255 127 80

cornflowerblue :: ZColor
cornflowerblue = ZColor 100 149 237

cornsilk :: ZColor
cornsilk = ZColor 255 248 220

crimson :: ZColor
crimson = ZColor 220 20 60

cyan :: ZColor
cyan = ZColor 0 255 255

darkblue :: ZColor
darkblue = ZColor 0 0 139

darkcyan :: ZColor
darkcyan = ZColor 0 139 139

darkgoldenrod :: ZColor
darkgoldenrod = ZColor 184 134 11

darkgray :: ZColor
darkgray = ZColor 169 169 169

darkgreen :: ZColor
darkgreen = ZColor 0 100 0

darkgrey :: ZColor
darkgrey = ZColor 169 169 169

darkkhaki :: ZColor
darkkhaki = ZColor 189 183 107

darkmagenta :: ZColor
darkmagenta = ZColor 139 0 139

darkolivegreen :: ZColor
darkolivegreen = ZColor 85 107 47

darkorange :: ZColor
darkorange = ZColor 255 140 0

darkorchid :: ZColor
darkorchid = ZColor 153 50 204

darkred :: ZColor
darkred = ZColor 139 0 0

darksalmon :: ZColor
darksalmon = ZColor 233 150 122

darkseagreen :: ZColor
darkseagreen = ZColor 143 188 143

darkslateblue :: ZColor
darkslateblue = ZColor 72 61 139

darkslategray :: ZColor
darkslategray = ZColor 47 79 79

darkslategrey :: ZColor
darkslategrey = ZColor 47 79 79

darkturquoise :: ZColor
darkturquoise = ZColor 0 206 209

darkviolet :: ZColor
darkviolet = ZColor 148 0 211

deeppink :: ZColor
deeppink = ZColor 255 20 147

deepskyblue :: ZColor
deepskyblue = ZColor 0 191 255

dimgray :: ZColor
dimgray = ZColor 105 105 105

dimgrey :: ZColor
dimgrey = ZColor 105 105 105

dodgerblue :: ZColor
dodgerblue = ZColor 30 144 255

firebrick :: ZColor
firebrick = ZColor 178 34 34

floralwhite :: ZColor
floralwhite = ZColor 255 250 240

forestgreen :: ZColor
forestgreen = ZColor 34 139 34

fuchsia :: ZColor
fuchsia = ZColor 255 0 255

gainsboro :: ZColor
gainsboro = ZColor 220 220 220

ghostwhite :: ZColor
ghostwhite = ZColor 248 248 255

gold :: ZColor
gold = ZColor 255 215 0

goldenrod :: ZColor
goldenrod = ZColor 218 165 32

gray :: ZColor
gray = ZColor 128 128 128

grey :: ZColor
grey = ZColor 128 128 128

green :: ZColor
green = ZColor 0 128 0

greenyellow :: ZColor
greenyellow = ZColor 173 255 47

honeydew :: ZColor
honeydew = ZColor 240 255 240

hotpink :: ZColor
hotpink = ZColor 255 105 180

indianred :: ZColor
indianred = ZColor 205 92 92

indigo :: ZColor
indigo = ZColor 75 0 130

ivory :: ZColor
ivory = ZColor 255 255 240

khaki :: ZColor
khaki = ZColor 240 230 140

lavender :: ZColor
lavender = ZColor 230 230 250

lavenderblush :: ZColor
lavenderblush = ZColor 255 240 245

lawngreen :: ZColor
lawngreen = ZColor 124 252 0

lemonchiffon :: ZColor
lemonchiffon = ZColor 255 250 205

lightblue :: ZColor
lightblue = ZColor 173 216 230

lightcoral :: ZColor
lightcoral = ZColor 240 128 128

lightcyan :: ZColor
lightcyan = ZColor 224 255 255

lightgoldenrodyellow :: ZColor
lightgoldenrodyellow = ZColor 250 250 210

lightgray :: ZColor
lightgray = ZColor 211 211 211

lightgreen :: ZColor
lightgreen = ZColor 144 238 144

lightgrey :: ZColor
lightgrey = ZColor 211 211 211

lightpink :: ZColor
lightpink = ZColor 255 182 193

lightsalmon :: ZColor
lightsalmon = ZColor 255 160 122

lightseagreen :: ZColor
lightseagreen = ZColor 32 178 170

lightskyblue :: ZColor
lightskyblue = ZColor 135 206 250

lightslategray :: ZColor
lightslategray = ZColor 119 136 153

lightslategrey :: ZColor
lightslategrey = ZColor 119 136 153

lightsteelblue :: ZColor
lightsteelblue = ZColor 176 196 222

lightyellow :: ZColor
lightyellow = ZColor 255 255 224

lime :: ZColor
lime = ZColor 0 255 0

limegreen :: ZColor
limegreen = ZColor 50 205 50

linen :: ZColor
linen = ZColor 250 240 230

magenta :: ZColor
magenta = ZColor 255 0 255

maroon :: ZColor
maroon = ZColor 128 0 0

mediumaquamarine :: ZColor
mediumaquamarine = ZColor 102 205 170

mediumblue :: ZColor
mediumblue = ZColor 0 0 205

mediumorchid :: ZColor
mediumorchid = ZColor 186 85 211

mediumpurple :: ZColor
mediumpurple = ZColor 147 112 219

mediumseagreen :: ZColor
mediumseagreen = ZColor 60 179 113

mediumslateblue :: ZColor
mediumslateblue = ZColor 123 104 238

mediumspringgreen :: ZColor
mediumspringgreen = ZColor 0 250 154

mediumturquoise :: ZColor
mediumturquoise = ZColor 72 209 204

mediumvioletred :: ZColor
mediumvioletred = ZColor 199 21 133

midnightblue :: ZColor
midnightblue = ZColor 25 25 112

mintcream :: ZColor
mintcream = ZColor 245 255 250

mistyrose :: ZColor
mistyrose = ZColor 255 228 225

moccasin :: ZColor
moccasin = ZColor 255 228 181

navajowhite :: ZColor
navajowhite = ZColor 255 222 173

navy :: ZColor
navy = ZColor 0 0 128

oldlace :: ZColor
oldlace = ZColor 253 245 230

olive :: ZColor
olive = ZColor 128 128 0

olivedrab :: ZColor
olivedrab = ZColor 107 142 35

orange :: ZColor
orange = ZColor 255 165 0

orangered :: ZColor
orangered = ZColor 255 69 0

orchid :: ZColor
orchid = ZColor 218 112 214

palegoldenrod :: ZColor
palegoldenrod = ZColor 238 232 170

palegreen :: ZColor
palegreen = ZColor 152 251 152

paleturquoise :: ZColor
paleturquoise = ZColor 175 238 238

palevioletred :: ZColor
palevioletred = ZColor 219 112 147

papayawhip :: ZColor
papayawhip = ZColor 255 239 213

peachpuff :: ZColor
peachpuff = ZColor 255 218 185

peru :: ZColor
peru = ZColor 205 133 63

pink :: ZColor
pink = ZColor 255 192 203

plum :: ZColor
plum = ZColor 221 160 221

powderblue :: ZColor
powderblue = ZColor 176 224 230

purple :: ZColor
purple = ZColor 128 0 128

red :: ZColor
red = ZColor 255 0 0

rosybrown :: ZColor
rosybrown = ZColor 188 143 143

royalblue :: ZColor
royalblue = ZColor 65 105 225

saddlebrown :: ZColor
saddlebrown = ZColor 139 69 19

salmon :: ZColor
salmon = ZColor 250 128 114

sandybrown :: ZColor
sandybrown = ZColor 244 164 96

seagreen :: ZColor
seagreen = ZColor 46 139 87

seashell :: ZColor
seashell = ZColor 255 245 238

sienna :: ZColor
sienna = ZColor 160 82 45

silver :: ZColor
silver = ZColor 192 192 192

skyblue :: ZColor
skyblue = ZColor 135 206 235

slateblue :: ZColor
slateblue = ZColor 106 90 205

slategray :: ZColor
slategray = ZColor 112 128 144

slategrey :: ZColor
slategrey = ZColor 112 128 144

snow :: ZColor
snow = ZColor 255 250 250

springgreen :: ZColor
springgreen = ZColor 0 255 127

steelblue :: ZColor
steelblue = ZColor 70 130 180

tanbrown :: ZColor
tanbrown = ZColor 210 180 140

teal :: ZColor
teal = ZColor 0 128 128

thistle :: ZColor
thistle = ZColor 216 191 216

tomato :: ZColor
tomato = ZColor 255 99 71

turquoise :: ZColor
turquoise = ZColor 64 224 208

violet :: ZColor
violet = ZColor 238 130 238

wheat :: ZColor
wheat = ZColor 245 222 179

white :: ZColor
white = ZColor 255 255 255

whitesmoke :: ZColor
whitesmoke = ZColor 245 245 245

yellow :: ZColor
yellow = ZColor 255 255 0

yellowgreen :: ZColor
yellowgreen = ZColor 154 205 50
