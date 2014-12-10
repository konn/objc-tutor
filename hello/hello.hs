{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

objc_import ["<Foundation/Foundation.h>"]

nslog :: String -> IO ()
nslog msg = $(objc ['msg :> ''String] $
              void [cexp| NSLog(@"%@", msg) |])

objc_emit

main = do
  objc_initialise
  nslog "Hello, from Haskell-Cocoa!"
