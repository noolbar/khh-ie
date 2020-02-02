module KKH.Types where

import Data.Newtype (class Newtype)

newtype MD5 = MD5 String
derive instance newtypeMD5 ∷ Newtype MD5 _

type Note =
  { key ∷ MD5
  , cells ∷ Array Cell 
  }

data Showable = None | Source | Output | SourceOutput
data Cell
  = PlainTextCell CellStructure
  | CodeCell CellStructure
  | MarkDownCell CellStructure
  | ImagePngCell CellStructure
  | ImageJpegCell CellStructure

type CellStructure =
  { key ∷ MD5
  , metadata ∷ String
  , showable ∷ Showable
  , source ∷ String
  , outputs ∷ Array Output
  }

newtype Base64 = Base64 String


data Output
  = PlainTextOut String
  | MarkDownOut String
  | ImagePngOut Base64
  | ImageJpegOut Base64
