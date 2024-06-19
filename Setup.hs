import Data.Maybe (fromJust)
import Distribution.PackageDescription qualified as PD
import Distribution.Simple
  ( Args,
    UserHooks,
    cleanHook,
    confHook,
    defaultMainWithHooks,
    postClean,
    preConf,
    simpleUserHooks,
  )
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (localPkgDescr),
  )
import Distribution.Simple.Setup
  ( CleanFlags,
    ConfigFlags,
    cleanVerbosity,
    configVerbosity,
    fromFlag,
  )
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.Verbosity qualified as Verbosity
import System.Directory (getCurrentDirectory)
import System.FilePath (joinPath)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = myPreConf,
        confHook = myConfHook,
        postClean = myPostClean
      }

myPreConf :: Args -> ConfigFlags -> IO PD.HookedBuildInfo
myPreConf args flags = do
  buildInfo <- preConf simpleUserHooks args flags
  rawSystemExit (fromFlag $ configVerbosity flags) "/bin/sh" ["-c", "cd rust && cargo build --release"]
  return buildInfo

myConfHook ::
  (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
  ConfigFlags ->
  IO LocalBuildInfo
myConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  dir <- getCurrentDirectory
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
      -- NOTE: this has to point to the Rust library release directory.
      rustLibDir = joinPath [dir, "rust/target/release"]
  return
    localBuildInfo
      { localPkgDescr =
          packageDescription
            { PD.library =
                Just $
                  library
                    { PD.libBuildInfo =
                        (PD.libBuildInfo library)
                          { PD.extraLibDirs = rustLibDir : PD.extraLibDirs (PD.libBuildInfo library),
                            PD.extraLibs = "rust" : PD.extraLibs (PD.libBuildInfo library)
                          }
                    }
            }
      }

myPostClean :: Args -> CleanFlags -> PD.PackageDescription -> () -> IO ()
myPostClean args flags description _ = do
  rawSystemExit (fromFlag $ cleanVerbosity flags) "/bin/sh" ["-c", "cd rust && cargo clean"]
  postClean simpleUserHooks args flags description ()
