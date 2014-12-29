{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell #-}
module Main where
import Data.Typeable          (Typeable)
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

import qualified AppDelegate as Delegate

objc_import ["<Cocoa/Cocoa.h>"]

nsApplicationMain :: IO ()
nsApplicationMain = $(objc [] $ void [cexp| NSApplicationMain(0, NULL) |])

objc_emit

main :: IO ()
main = do
  objc_initialise
  Delegate.objc_initialise
  nsApplicationMain
