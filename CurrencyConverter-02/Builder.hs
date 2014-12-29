{-# LANGUAGE RecordWildCards #-}
module Main where
import           Control.Applicative                   ((<$>))
import           Control.Monad                         (forM_, when)
import           Data.List                             (intersperse, nub)
import           Data.Maybe
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Distribution.Cab.Sandbox
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import qualified GHC.Paths                             as Path
import qualified System.Directory                      as D
import           System.Environment                    (getArgs)
import           System.Environment                    (withArgs)

build :: FilePath
build = "_build"

data AppSetting = AppSetting { app      :: String
                             , ldFlags  :: [String]
                             , packDBs  :: [FilePath]
                             , includes :: [FilePath]
                             } deriving (Read, Show, Eq, Ord)

main :: IO ()
main = do
  cab <- head . filter ((== ".cabal") . takeExtension) <$> (D.getDirectoryContents =<< D.getCurrentDirectory)
  packdesc <- readPackageDescription silent cab
  msandbox <- getSandbox
  let deps0 = buildDepends $ packageDescription packdesc
      exes  = map (condTreeData . snd) $ condExecutables packdesc
      name  = fst $ head $ condExecutables packdesc
      deps1 = condTreeConstraints $ snd $ head $ condExecutables packdesc
      dbs   = maybeToList msandbox
      incs  = map (</> "include") [Path.libdir, Path.libdir ++ "/../.."]
  if length exes /= 1
    then error "only one executable is currently supported"
    else
    let targ = head exes
        deps = deps0 ++ deps1 ++ targetBuildDepends (buildInfo targ)
        packs = prefixing "-package" $ nub $ map getPackageName deps
        frams = prefixing "-framework" $ frameworks $ buildInfo targ
        opts  = ["-optl-ObjC", "-threaded"]
        setting = AppSetting { app = name
                             , ldFlags = packs ++ frams ++ opts
                             , packDBs = dbs
                             , includes = incs
                             }
    in shakeArgs shakeOptions{shakeFiles=build} $ buildWith setting

prefixing :: a -> [a] -> [a]
prefixing _   [] = []
prefixing str xs = str : intersperse str xs

getPackageName :: Dependency -> String
getPackageName (Dependency (PackageName name) _) = name

buildWith :: AppSetting -> Rules ()
buildWith AppSetting {..} = do
  let dbs  = prefixing "-package-db" packDBs
      dest = app <.> "app"
      skeleton = build </> app <.> "app"
      exe = build </> app
      xcode = "xcode_proj" </> app </> app <.> "xcodeproj"
  want [dest]

  phony "clean" $ do
    putNormal "cleaning..."
    forM_ [build, dest, "xcode_proj" </> app </> "build"] $ \pth -> do
      remove <- doesDirectoryExist pth
      when remove $ removeFilesAfter pth ["//*"]

  dest *> \out -> do
    putNormal "setting up for bundle..."
    need [skeleton, exe]
    () <- cmd "cp" "-r" skeleton out
    copyFile' exe (out </> "Contents" </> "MacOS" </> app)
    putNormal "Application bundle successfully compiled."

  build </> app <.> "app" *> \out -> do
    need [xcode]
    putNormal "compiling xcodeproj..."
    () <- cmd "xcodebuild -project" xcode
    cmd "cp" "-r" ("xcode_proj" </> app </> "build/Release" </> app <.> "app") out

  build </> app *> \out -> do
    hss <- getDirectoryFiles "src" ["//*.hs"]
    let objs = [build </> hs -<.> "o" | hs <- hss, hs `notElem` ["Setup.hs", "Builder.hs"]]
    need objs
    addObs <- getDirectoryFiles "" ["_build//*_objc.o"]
    putNormal $ "linking executable... "
    cmd "ghc" "-o" out "-odir" build "-hidir" build "-stubdir" build "-outputdir" build "-isrc"
              (objs ++ addObs) ldFlags dbs

  ["_build//*.o", "_build//*.hi"] &*> \ [out, hi] -> do
    putNormal $ "building object: " ++ out
    let hs = "src" </> (dropDirectory1 $ out -<.> "hs")
        dep = out -<.> "dep"
        obcBase = dropExtension (dropDirectory1 out) ++ "_objc"
        obcm = obcBase <.> "m"
        obch = obcBase <.> "h"
    command_ [] "ghc" $ [ "-M", "-odir", build, "-hidir", build, "-isrc"
                        , "-dep-suffix", "", "-dep-makefile", dep, hs] ++ dbs
    needMakefileDependencies dep
    () <- cmd "ghc" "-c" hs "-dynamic" "-o" out dbs "-odir" build "-hidir" build ("-i" ++ build)
    gen'd <- doesFileExist $ "src" </> obcm
    when gen'd $ do
      putNormal $ "compiling gen'd file: " ++ obcm
      () <- cmd "mv" ("src" </> obcm) (build </> obcm)
      () <- cmd "mv" ("src" </> obch) (build </> obch)
      () <- cmd "cc" "-fobjc-arc"
        (map ("-I"++) includes)
        "-c -o" (build </> obcBase <.> "o") (build </> obcm)
      cmd "rm" "-f" $ dropExtension (dropDirectory1 out) ++ "_stub" <.> "h"
    putNormal $ "built: " ++ out
