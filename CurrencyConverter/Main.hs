{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell #-}
module Main where
import Data.Typeable          (Typeable)
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

objc_import ["<Cocoa/Cocoa.h>"]

objcMain :: IO ()
objcMain = $(objc [] $ void [cexp| NSApplicationMain(0, NULL) |])

objc_interface [cunit|
@interface AppDelegate : NSObject <NSApplicationDelegate>
@property (weak) typename NSTextField *dollarsField;
@property (weak) typename NSTextField *rateField;
@property (weak) typename NSTextField *resultField;

- (void)convert:(id)sender;
@end
 |]

newtype NSTextField = NSTextField (ForeignPtr NSTextField)
                      deriving (Typeable)

marshalNSTextField :: NSTextField -> IO NSTextField
marshalNSTextField = return

objc_marshaller 'marshalNSTextField 'marshalNSTextField

newtype AppDelegate = AppDelegate (ForeignPtr AppDelegate)
                      deriving (Typeable)

marshalAppDel :: AppDelegate -> IO AppDelegate
marshalAppDel = return

objc_marshaller 'marshalAppDel 'marshalAppDel

intValue :: NSTextField -> IO Int
intValue txt = $(objc ['txt :> ''NSTextField] $
                 ''Int <: [cexp| [txt intValue] |])

doubleValue :: NSTextField -> IO Double
doubleValue txt = $(objc ['txt :> ''NSTextField] $
                          ''Double <: [cexp| [txt intValue] |])

dollarsField :: AppDelegate -> IO NSTextField
dollarsField app = $(objc ['app :> ''AppDelegate] $
                     Class ''NSTextField <: [cexp| app.dollarsField |])

rateField :: AppDelegate -> IO NSTextField
rateField app = $(objc ['app :> ''AppDelegate] $
                     Class ''NSTextField <: [cexp| app.rateField |])

resultField :: AppDelegate -> IO NSTextField
resultField app = $(objc ['app :> ''AppDelegate] $
                     Class ''NSTextField <: [cexp| app.resultField |])

setIntValue :: Int -> NSTextField -> IO ()
setIntValue i txt =
  $(objc ['i :> ''Int, 'txt :> ''NSTextField] $
    void [cexp| [txt setIntValue: i] |])

convert :: AppDelegate -> IO ()
convert app = do
  input <- intValue    =<< dollarsField app
  rate  <- doubleValue =<< rateField app
  setIntValue (floor $ fromIntegral input * rate) =<< resultField app

objc_implementation [Typed 'convert] [cunit|
@implementation AppDelegate
- (void) convert: (id)sender
{
  convert(self);
}
@end
|]

objc_emit

main :: IO ()
main = do
  objc_initialise
  objcMain
