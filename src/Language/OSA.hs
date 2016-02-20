{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, PolyKinds, QuasiQuotes #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeFamilies              #-}
module Language.OSA ( NSAppleEventDescriptor, NSAppleScript,
                      compile, compileOrError,
                      execute, executeOrError,
                      executeAppleEvent,
                      doAppleScript, eventDescriptorData, newAppleScript, AppleScriptError(..),
                      isCompiled, source) where

import Language.OSA.AppleEventDescriptor

import Control.Exception    (Exception)
import Control.Exception    (throwIO)
import Control.Monad        ((<=<))
import Data.ByteString      (ByteString)
import Data.Text            (Text)
import Data.Typeable        (Typeable)
import Foreign              (alloca)
import Foreign              (peek)
import Foreign.C.Types
import Foreign.Ptr          (nullPtr)
import Language.ObjC.Inline as C

C.context (objcCtxWithClasses [defClass "NSAppleEventDescriptor"
                              ,defClass "NSAppleScript"
                              ,defClass "NSDictionary"])

type NSAppleScript = ObjC "NSAppleScript"
type NSDictionary = ObjC "NSDictionary"

import_ "<Foundation/Foundation.h>"

eventDescriptorData :: NSAppleEventDescriptor -> IO ByteString
eventDescriptorData desc = [C.exp'| NSData * { [($raw:(NSAppleEventDescriptor *desc)) data] } |]

newAppleScript :: Text -> IO NSAppleScript
newAppleScript src = [C.exp| NSAppleScript * { [[NSAppleScript alloc] initWithSource: $txt:src] } |]

execute :: NSAppleScript -> IO (Either AppleScriptError NSAppleEventDescriptor)
execute as = alloca $ \ptr -> do
  o@(ObjC evt) <- [C.exp| NSAppleEventDescriptor * { [($raw:(NSAppleScript *as))
                                                      executeAndReturnError: $(NSDictionary **ptr)] } |]
  if evt == nullPtr
    then fmap Left . dicToASError . ObjC =<< peek ptr
    else return $ Right o

executeOrError :: NSAppleScript -> IO NSAppleEventDescriptor
executeOrError = either throwIO return <=< execute

doAppleScript :: Text -> IO NSAppleEventDescriptor
doAppleScript = either throwIO return <=< execute <=< newAppleScript

compile :: NSAppleScript -> IO (Either AppleScriptError ())
compile as = alloca $ \ptr ->  do
  suc <- [C.exp| BOOL { [($raw:(NSAppleScript *as)) compileAndReturnError: $(NSDictionary **ptr)] } |]
  dic <- peek ptr
  if suc == 1 then return (Right ()) else Left <$> dicToASError (ObjC dic)

compileOrError :: NSAppleScript -> IO ()
compileOrError = either throwIO return <=< compile

isCompiled :: NSAppleScript -> IO Bool
isCompiled as = (== 1) <$> [C.exp| BOOL { ($raw:(NSAppleScript *as)).compiled } |]

source :: NSAppleScript -> IO Text
source as = [C.exp'| NSString * { $raw:(NSAppleScript *as).source } |]

data AppleScriptError = AppleScriptError { asErrorMessage      :: Text
                                         , asErrorNumber       :: Int
                                         , asErrorAppName      :: Text
                                         , asErrorBriefMessage :: Text
                                         , asErrorRange        :: (Int, Int)
                                         } deriving (Read, Show, Eq, Ord, Typeable)

instance Exception AppleScriptError

dicToASError :: NSDictionary -> IO AppleScriptError
dicToASError dic = do
  asErrorMessage <- withNull "" =<< [C.exp| NSString * { [($raw:(NSDictionary *dic)) valueForKey: NSAppleScriptErrorMessage] } |]
  asErrorNumber  <- fromIntegral <$> [C.exp'| int { [[($raw:(NSDictionary *dic)) valueForKey: NSAppleScriptErrorNumber] intValue] } |]
  asErrorAppName <- withNull "" =<< [C.exp| NSString * { [($raw:(NSDictionary *dic)) valueForKey: NSAppleScriptErrorAppName] } |]
  asErrorBriefMessage <- withNull "" =<< [C.exp| NSString * { [($raw:(NSDictionary *dic)) valueForKey: NSAppleScriptErrorBriefMessage] } |]
  begin <- fromIntegral <$> ([C.exp| int { [[($raw:(NSDictionary *dic)) valueForKey: NSAppleScriptErrorRange] rangeValue].location } |])
  len <- fromIntegral <$> ([C.exp| int { [[($raw:(NSDictionary *dic)) valueForKey: NSAppleScriptErrorRange] rangeValue].length } |])
  let asErrorRange = (begin, len)
  return AppleScriptError{..}

isNilObject :: ObjC a -> Bool
isNilObject (ObjC ptr) = ptr == nullPtr

executeAppleEvent :: NSAppleScript -> NSAppleEventDescriptor -> IO (Either AppleScriptError NSAppleEventDescriptor)
executeAppleEvent as evt = alloca $ \ptr -> do
  ans <- [C.exp| NSAppleEventDescriptor * { [($raw:(NSAppleScript *as))
                                               executeAppleEvent: ($raw:(NSAppleEventDescriptor *evt))
                                               error: $(NSDictionary **ptr)]} |]
  if isNilObject ans
    then fmap Left . dicToASError . ObjC =<< peek ptr
    else return $ Right ans

withNull :: Object a => Haskell a -> ObjC a -> IO (Haskell a)
withNull def o@(ObjC ptr) | ptr == nullPtr = return def
                          | otherwise = fromObjC o
