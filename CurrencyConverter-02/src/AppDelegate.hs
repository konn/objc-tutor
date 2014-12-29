{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies                   #-}
module AppDelegate where
import Control.Applicative    ((<*>))
import Control.Applicative    ((<$>))
import Data.Typeable          (Typeable)
import FRP.Sodium             (newBehaviour)
import FRP.Sodium             (sync)
import FRP.Sodium             (value)
import FRP.Sodium             (listen)
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

import Messaging

objc_import ["<Cocoa/Cocoa.h>"]

defineClass "NSObject" Nothing

objc_interface [cunit|
@interface AppDelegate : NSResponder <NSApplicationDelegate, NSControlTextEditingDelegate>
@property (weak) typename NSTextField *dollarsField;
@property (weak) typename NSTextField *rateField;
@property (weak) typename NSTextField *resultField;

- (void)controlTextDidChange:(typename NSNotification *)obj;
@end
 |]

defineClass "AppDelegate" (Just ''NSObject)
idMarshaller ''AppDelegate

defineClass "NSTextField" (Just ''NSObject)
idMarshaller ''NSTextField

defineClass "NSNotification" (Just ''NSObject)
idMarshaller ''NSNotification

defineSelector newSelector { selector = "setIntValue"
                           , reciever = (''NSTextField, "field")
                           , arguments = ["num" :>>: ''Int]
                           , definition = [cexp| [field setIntValue: num] |]
                           }

defineSelector newSelector { selector = "intValue"
                           , reciever = (''NSTextField, "txt")
                           , returnType = Just [t| Int |]
                           , definition = [cexp| [txt intValue] |]
                           }

defineSelector newSelector { selector = "doubleValue"
                           , reciever = (''NSTextField, "txt")
                           , returnType = Just [t| Double |]
                           , definition = [cexp| [txt doubleValue] |]
                           }

defineSelector newSelector { selector = "resultField"
                           , reciever = (''AppDelegate, "app")
                           , definition = [cexp| [app resultField] |]
                           , returnType = Just [t| NSTextField |]
                           }

defineSelector newSelector { selector = "rateField"
                           , reciever = (''AppDelegate, "app")
                           , definition = [cexp| [app rateField] |]
                           , returnType = Just [t| NSTextField |]
                           }

defineSelector newSelector { selector = "dollarsField"
                           , reciever = (''AppDelegate, "app")
                           , definition = [cexp| [app dollarsField] |]
                           , returnType = Just [t| NSTextField |]
                           }

defineSelector newSelector { selector = "sender"
                           , reciever = (''NSNotification, "notif")
                           , returnType = Just [t| NSTextField |]
                           , definition = [cexp| [notif object] |]
                           }


type Dollars = Int
type Rate    = Double

data Session = Session { pushDollars :: Dollars -> IO ()
                       , pushRate    :: Rate    -> IO ()
                       , application :: AppDelegate
                       } deriving (Typeable)

newSession :: AppDelegate -> IO Session
newSession app = sync $ do
  (dolBh, dolL) <- newBehaviour 0
  (ratBh, ratL) <- newBehaviour 0
  _ <- listen (value $ (*) <$> dolBh <*> ratBh) $ \val ->
    app # resultField #. setIntValue (floor val)
  return $ Session (sync . dolL . fromIntegral) (sync . ratL) app

nsLog :: String -> IO ()
nsLog str = $(objc ['str :> ''String] $ void [cexp| NSLog(@"%@", str) |] )

changed :: Session -> NSNotification -> IO ()
changed session notif = do
  sendF <- notif # sender
  rateF <- application session # rateField
  dollF <- application session # dollarsField
  if sendF == rateF
    then pushRate    session =<< rateF # doubleValue
    else pushDollars session =<< dollF # intValue

objc_implementation [Typed 'changed, Typed 'newSession] [cunit|
@interface AppDelegate ()
@property (assign) typename HsStablePtr session;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.session = newSession(self);
}

- (void) controlTextDidChange:(typename NSNotification*) aNotification
{
  changed(self.session, aNotification);
}

@end
 |]

objc_emit
