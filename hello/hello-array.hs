{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where
import Control.Monad          (forM, forM_)
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

objc_import ["<Foundation/Foundation.h>"]

newtype NSString = NSString (ForeignPtr ())
newtype NSArray = NSArray (ForeignPtr ())
newtype NSMutableArray = NSMutableArray (ForeignPtr ())

objc_typecheck

nsArrToListOfStrings :: NSArray -> IO [String]
nsArrToListOfStrings arr = do
  len <- $(objc ['arr :> Class ''NSArray] $ ''Int <: [cexp| [arr count] |])
  forM [0..len -1] $ \ i ->
    $(objc ['arr :> Class ''NSArray, 'i :> ''Int] $ ''String <: [cexp| [arr objectAtIndex: i] |])

listOfStringsToNSArr :: [String] -> IO NSArray
listOfStringsToNSArr strs = do
  marr <- $(objc [] $ Class ''NSMutableArray <: [cexp| [NSMutableArray array] |])
  forM_ strs $ \str ->
    $(objc ['marr :> Class ''NSMutableArray, 'str :> ''String] $
      void [cexp| [marr addObject: str] |])
  $(objc ['marr :> Class ''NSMutableArray] $
           Class ''NSArray <: [cexp| marr |])

objc_marshaller 'listOfStringsToNSArr 'nsArrToListOfStrings

logArray :: [String] -> IO ()
logArray msg = $(objc ['msg :> [t| [String] |]] $
               void [cexp| NSLog(@"%@", msg) |])

takeHead :: [String] -> IO String
takeHead strs =
  $(objc ['strs :> [t| [String] |]] $
    ''String <: [cexp| [strs objectAtIndex: 0] |])

takeFirstTwo :: [String] -> IO [String]
takeFirstTwo strs =
  $(objc ['strs :> [t| [String] |]] $
    [t| [String] |] <: [cexp| @[ [strs objectAtIndex: 0] , [strs objectAtIndex: 1] ] |])

objc_emit

main = do
  objc_initialise
  logArray ["Hello", "Cocoa", "World"]
  print =<< takeHead ["Hello, I'm first!", "and second", "no see me."]
  print =<< takeFirstTwo ["Only I and...", "I can appear", "but not me!"]
