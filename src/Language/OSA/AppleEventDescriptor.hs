{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, PolyKinds, QuasiQuotes  #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeFamilies, ViewPatterns #-}
-- | Provides API to extract information from AppleEventDescriptor, not creating.
module Language.OSA.AppleEventDescriptor
       (NSAppleEventDescriptor,
        asBool, data_, asInt32, itemCount, asText,
        descriptorAt) where

import           Data.ByteString      (ByteString)
import           Data.Int             (Int32)
import           Data.Text            (Text)
import           Foreign              (nullPtr)
import           Foreign.C.Types      (CInt (..), CLong (..))
import           Language.ObjC.Inline (objcCtxWithClasses)
import           Language.ObjC.Inline (defClass)
import           Language.ObjC.Inline (import_)
import           Language.ObjC.Inline (ObjC (..))
import           Language.ObjC.Inline (fromObjC)
import qualified Language.ObjC.Inline as C

C.context (objcCtxWithClasses [defClass "NSAppleEventDescriptor"
                              ,defClass "NSAppleScript"
                              ,defClass "NSDictionary"])

import_ "Foundation/Foundation.h"

type NSAppleEventDescriptor = ObjC "NSAppleEventDescriptor"

asBool :: NSAppleEventDescriptor -> IO Bool
asBool evt = (1 ==) <$> [C.exp| BOOL { ($raw:(NSAppleEventDescriptor *evt)).booleanValue } |]

data_ :: NSAppleEventDescriptor -> IO ByteString
data_ evt = [C.exp'| NSData * {$raw:(NSAppleEventDescriptor *evt).data} |]

asInt32 :: NSAppleEventDescriptor -> IO CInt
asInt32 evt = [C.exp| signed int { ($raw:(NSAppleEventDescriptor *evt)).int32Value } |]

itemCount :: NSAppleEventDescriptor -> IO Int
itemCount evt = fromIntegral <$> [C.exp| NSInteger { ($raw:(NSAppleEventDescriptor *evt)).numberOfItems } |]

asText :: NSAppleEventDescriptor -> IO (Maybe Text)
asText evt = do
  txt@(ObjC obj) <- [C.exp| NSString * { ($raw:(NSAppleEventDescriptor *evt)).stringValue } |]
  if obj == nullPtr
    then return Nothing
    else Just <$> fromObjC txt

descriptorAt :: NSAppleEventDescriptor -> Int -> IO (Maybe NSAppleEventDescriptor)
descriptorAt evt (fromIntegral -> n) = do
  o@(ObjC ptr) <- [C.exp| NSAppleEventDescriptor * { [($raw:(NSAppleEventDescriptor *evt))
                                                      descriptorAtIndex: $(NSInteger n)] } |]
  if ptr == nullPtr
  then return Nothing
  else return $ Just o
