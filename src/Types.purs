module KKH.Types where

import Prelude

import Data.Newtype (class Newtype)
import Foreign (Foreign, ForeignError(..), fail)
import Foreign.Index (readProp)
import Simple.JSON (class ReadForeign, readImpl)

newtype MD5 = MD5 String
derive instance newtypeMD5 ∷ Newtype MD5 _

instance readForeignMD5 ∷ ReadForeign MD5 where
  readImpl o = do
    str :: String ← readImpl o
    pure $ MD5 str


type Note' =
  { key ∷ MD5
  , cells ∷ Array Cell 
  }
newtype Note = Note Note'
derive instance newtypeNote :: Newtype Note _

instance readForeignNote ∷ ReadForeign Note where
  readImpl o = do
    key :: MD5 ← readImpl =<< readProp "key" o
    cells :: (Array Cell) ← readImpl =<< readProp "cells" o
    pure $ Note {key, cells}

type NoteJson =
  { key ∷ String
  , cells ∷ Foreign
  }
    
data Showable = None | Source | Output | SourceOutput
data Cell'
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
  , outputs ∷ Array CellOutput
  }
newtype Cell = Cell Cell'

type CellJson =
  { celltype ∷ String
  , key ∷ String
  , metadata ∷ String
  , showable ∷ String
  , source ∷ String
  , outputs ∷ Foreign
  }

instance readForeignCell :: ReadForeign Cell where
  readImpl o = do
    { celltype, key, metadata, showable, source, outputs } ∷ CellJson ← readImpl o
    parse ∷ Array CellOutput ← readImpl outputs
    let d = { key: MD5 key, metadata, showable: toShowable showable, source, outputs: parse } 
    case celltype of
      "PlainTextCell" → pure $ Cell (PlainTextCell d)
      "CodeCell" → pure $ Cell (CodeCell d)
      "MarkDownCell" → pure $ Cell (MarkDownCell d)
      "ImagePngCell" → pure $ Cell (ImagePngCell d)
      "ImageJpegCell" → pure $ Cell (ImageJpegCell d)
      other → fail <<< ForeignError $ "cannot read result_type: " <> other
    where
      toShowable ∷ String → Showable
      toShowable t = case t of
        "None" → None 
        "Source" → Source 
        "Output" → Output 
        "SourceOutput" → SourceOutput
        _ → None      
        
data CellOutput'
  = PlainTextOut String
  | MarkDownOut String
  | ImagePngOut Base64
  | ImageJpegOut Base64
  
newtype Base64 = Base64 String
derive instance newtypeBase64 :: Newtype Base64 _

newtype CellOutput = CellOutput CellOutput'

type CellOutputJSON =
  { outputtype ∷ String
  , content ∷ String
  }

instance readForeignCellOutput ∷ ReadForeign CellOutput where
  readImpl o = do
    { outputtype, content } ∷ CellOutputJSON ← readImpl o
    case outputtype of
      "PlainTextOut" → pure $ CellOutput (PlainTextOut content)
      "MarkDownOut" → pure $ CellOutput (MarkDownOut content)
      "ImagePngOut" → pure $ CellOutput (ImagePngOut (Base64 content))
      "ImageJpegOut" → pure $ CellOutput (ImageJpegOut (Base64 content))
      other → fail <<< ForeignError $ "connot read outputtype: " <> other

newtype URL = URL String
derive instance newtypeURL ∷ Newtype URL _

type State = { url ∷ URL
             , entryPoint ∷ URL
             , hash ∷ MD5
             , note ∷ Note
             }

defaultState ∷ State
defaultState =
  { url : URL "example.com"
  , entryPoint : URL "localhost"
  , hash : MD5 "123456"
  , note : defaultNote
  }


defaultNote ∷ Note
defaultNote =
  Note  { key : MD5 "123"
        , cells :
            [ Cell $ PlainTextCell
              { metadata :  ""
              , key : MD5 "456"
              , showable : SourceOutput
              , source : "default Source"
              , outputs :
                  [ CellOutput $ PlainTextOut "default Output"
                  ]
              }
            ]
        }




