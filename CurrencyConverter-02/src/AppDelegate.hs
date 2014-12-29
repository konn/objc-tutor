{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell #-}
module AppDelegate where
import Data.Typeable          (Typeable)
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

objc_import ["<Cocoa/Cocoa.h>"]

newtype AppDelegate = AppDelegate (ForeignPtr AppDelegate)
                      deriving (Typeable)

marshalAppDel :: AppDelegate -> IO AppDelegate
marshalAppDel = return

objc_marshaller 'marshalAppDel 'marshalAppDel

nsLog :: String -> IO ()
nsLog str = $(objc ['str :> ''String] $ void [cexp| NSLog(@"%@", str) |] )

changed :: AppDelegate -> IO ()
changed app = nsLog "dummy!"

objc_interface [cunit|
@interface AppDelegate : NSResponder <NSApplicationDelegate, NSControlTextEditingDelegate>
@property (weak) typename NSTextField *dollarsField;
@property (weak) typename NSTextField *rateField;
@property (weak) typename NSTextField *resultField;

- (void)controlTextDidChange:(typename NSNotification *)obj;
@end
 |]


objc_implementation [Typed 'changed] [cunit|
@implementation AppDelegate
- (void) controlTextDidChange:(typename NSNotification*) aNotification
{
  changed(self);
}

@end
 |]

objc_emit
