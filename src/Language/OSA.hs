{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, PolyKinds, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                               #-}
module Language.OSA ( doAppleScript, eventDescriptorData ) where
import           Data.ByteString      (ByteString)
import           Data.Text            (Text)
import qualified Language.C.Inline    as C
import           Language.ObjC.Inline

C.context (objcCtxWithClasses [defClass "NSAppleEventDescriptor"])
type NSAppleEventDescriptor = ObjC "NSAppleEventDescriptor"

import_ "<Foundation/Foundation.h>"

eventDescriptorData :: NSAppleEventDescriptor -> IO ByteString
eventDescriptorData desc =
  fromObjC . ObjC =<< [C.exp| NSData * { [($raw:(NSAppleEventDescriptor *desc)) data] } |]

doAppleScript :: Text -> IO NSAppleEventDescriptor
doAppleScript src = ObjC <$>
  [C.exp| NSAppleEventDescriptor * { [[[NSAppleScript alloc] initWithSource: $txt:src] executeAndReturnError: NULL] } |]
