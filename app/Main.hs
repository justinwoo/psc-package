{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Foldl as Foldl
import           Control.Concurrent.Async (forConcurrently_, mapConcurrently)
import           Data.Foldable (fold, foldMap, traverse_)
import qualified Data.Graph as G
import           Data.List (maximumBy)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.HashMap.Strict.InsOrd as HM
import           Data.Ord (comparing)
import qualified Data.Set as Set
import           Data.Text (pack)
import qualified Data.Text as T
import           Data.Version (Version(..), parseVersion, showVersion)
import qualified Dhall as Dhall
import qualified Dhall.Core as Dhall.Core
import qualified Dhall.Parser as Dhall.Parser
import qualified Dhall.Format as Dhall.Format
import qualified Filesystem.Path.CurrentOS as Path
import qualified Options.Applicative as Opts
import qualified Paths_psc_package as Paths
import           System.Environment (getArgs)
import qualified System.IO as IO
import qualified System.Process as Process
import qualified Text.ParserCombinators.ReadP as Read
import           Turtle hiding (arg, fold, s, x)
import qualified Turtle
import           Types (PackageName, mkPackageName, runPackageName, untitledPackageName, preludePackageName, packageNameDhallType)

echoT :: Text -> IO ()
echoT = Turtle.printf (Turtle.s % "\n")

exitWithErr :: Text -> IO a
exitWithErr errText = errT errText >> exit (ExitFailure 1)
  where errT = traverse Turtle.err . textToLines

packageFile :: Path.FilePath
packageFile = "psc-package.dhall"

data PackageConfig = PackageConfig
  { name     :: PackageName
  , depends  :: [PackageName]
  , packages :: PackageSet
  } deriving (Show)

packageConfigDhallType :: Dhall.Type PackageConfig
packageConfigDhallType =
  Dhall.record $ PackageConfig
    <$> Dhall.field "name" Types.packageNameDhallType
    <*> Dhall.field "depends" (Dhall.list Types.packageNameDhallType)
    <*> Dhall.field "packages" packageSetDhallType

pathToTextUnsafe :: Turtle.FilePath -> Text
pathToTextUnsafe = either (error "Path.toText failed") id . Path.toText

shellToIOText :: Turtle.Shell Line -> IO [Text]
shellToIOText shellLines = Turtle.fold (fmap lineToText shellLines) Foldl.list

readPackageFile :: IO PackageConfig
readPackageFile = do
  exists <- testfile packageFile
  unless exists $ exitWithErr "psc-package.dhall does not exist. Maybe you need to run psc-package init?"
  Dhall.input packageConfigDhallType =<< readTextFile packageFile

writeNewPackageFile :: PackageName -> [PackageName] -> Text -> IO ()
writeNewPackageFile name depends packagesUrl = do
  case Dhall.Parser.exprFromText "writeNewPackageFile Dhall input" contents of
    Left errMsg -> exitWithErr $ T.append "Failed to convert writeNewPackageFile Dhall input to Dhall Expr: " (T.pack $ show errMsg)
    Right expr -> writeTextFile packageFile $ Dhall.Core.pretty expr
  Dhall.Format.format . Just . T.unpack . pathToTextUnsafe $ packageFile
  where
    contents
      = T.replace "%name" (runPackageName name)
      . T.replace "%packages" packagesUrl
      . T.replace "%depends" ("[" `T.append` T.intercalate "," (quote. runPackageName <$> depends) `T.append` "]")
      $ "{name=\"%name\",packages=%packages,depends=%depends}"
    quote s = "\"" `T.append` s `T.append` "\""

data PackageInfo = PackageInfo
  { repo         :: Text
  , version      :: Text
  , dependencies :: [PackageName]
  } deriving (Show, Eq)

packageInfoDhallType :: Dhall.Type PackageInfo
packageInfoDhallType =
  Dhall.record $ PackageInfo
    <$> Dhall.field "repo" Dhall.auto
    <*> Dhall.field "version" Dhall.auto
    <*> Dhall.field "dependencies" (Dhall.list Types.packageNameDhallType)

type PackageSet = Map.Map PackageName PackageInfo

packageSetDhallType :: Dhall.Type PackageSet
packageSetDhallType = Dhall.Type { extract, expected }
  where
    extract (Dhall.Core.RecordLit hm) = do
      hm' <- HM.traverseWithKey decodeExpr hm
      m <- traverse decodeKey $ HM.toList hm'
      return $ Map.fromList m
    extract _ = Nothing

    expected = Dhall.Core.Record (HM.fromList [])

    decodeExpr _key value = case Dhall.extract packageInfoDhallType value of
      Just (x :: PackageInfo) -> return x
      Nothing -> Nothing
    decodeKey (key, value) = case mkPackageName key of
      Right name -> return (name, value)
      Left _ -> Nothing

cloneShallow
  :: Text
  -- ^ repo
  -> Text
  -- ^ branch/tag
  -> Turtle.FilePath
  -- ^ target directory
  -> IO ExitCode
cloneShallow from ref into =
  proc "git"
       [ "clone"
       , "-q"
       , "-c", "advice.detachedHead=false"
       , "--depth", "1"
       , "-b", ref
       , from
       , pathToTextUnsafe into
       ] empty .||. exit (ExitFailure 1)

listRemoteTags
  :: Text
  -- ^ repo
  -> Turtle.Shell Text
listRemoteTags from = let gitProc = inproc "git"
                                    [ "ls-remote"
                                    , "-q"
                                    , "-t"
                                    , from
                                    ] empty
                      in lineToText <$> gitProc

readPackageSet :: PackageConfig -> IO PackageSet
readPackageSet PackageConfig{ packages } = do
  return packages

handleReadPackageSet :: Path.FilePath -> IO PackageSet
handleReadPackageSet dbFile = do
  exists <- testfile dbFile
  unless exists $ exitWithErr $ format (fp%" does not exist") dbFile
  expr <- Dhall.inputExpr =<< readTextFile dbFile
  case expr of
    (Dhall.Core.RecordLit hm) -> do
      hm' <- HM.traverseWithKey decodeExpr hm
      m <- traverse decodeKey $ HM.toList hm'
      return $ Map.fromList m
    errMsg -> exitWithErr $
      T.append
        "The database was not of the correct type, it should have been a record of packages: "
        (T.pack $ show errMsg)

  where
    decodeExpr key value = case Dhall.extract packageInfoDhallType value of
      Just (x :: PackageInfo) -> return x
      Nothing -> exitWithErr $ T.append "Could not extract PackageInfo from " key
    decodeKey (key, value) = case mkPackageName key of
      Left errMsg -> exitWithErr $ T.append (T.append "Error in package " key) (T.pack $ show errMsg)
      Right name -> return (name, value)

performInstall :: Text -> PackageName -> PackageInfo -> IO Turtle.FilePath
performInstall set pkgName PackageInfo{ repo, version } = do
  let pkgDir = ".psc-package" </> fromText set </> fromText (runPackageName pkgName) </> fromText version </> "src"
  exists <- testdir pkgDir
  unless exists . void $ do
    echoT ("Installing " <> runPackageName pkgName)
    cloneShallow repo version pkgDir
  pure pkgDir

getReverseDeps  :: PackageSet -> PackageName -> IO [(PackageName, PackageInfo)]
getReverseDeps db dep =
    List.nub <$> foldMap go (Map.toList db)
  where
    go pair@(packageName, PackageInfo {dependencies}) =
      case List.find (== dep) dependencies of
        Nothing -> return mempty
        Just _ -> do
          innerDeps <- getReverseDeps db packageName
          return $ pair : innerDeps

getTransitiveDeps :: PackageSet -> [PackageName] -> IO [(PackageName, PackageInfo)]
getTransitiveDeps db deps =
    Map.toList . fold <$> traverse (go Set.empty) deps
  where
    go seen pkg
      | pkg `Set.member` seen =
          exitWithErr ("Cycle in package dependencies at package " <> runPackageName pkg)
      | otherwise =
        case Map.lookup pkg db of
          Nothing ->
            exitWithErr ("Package " <> runPackageName pkg <> " does not exist in package set")
          Just info@PackageInfo{ dependencies } -> do
            m <- fold <$> traverse (go (Set.insert pkg seen)) dependencies
            return (Map.insert pkg info m)

installImpl :: PackageConfig -> IO ()
installImpl config@PackageConfig{ depends } = do
  db <- readPackageSet config
  trans <- getTransitiveDeps db depends
  echoT ("Installing " <> pack (show (length trans)) <> " packages...")
  forConcurrently_ trans . uncurry $ performInstall $ (const "set") config

getPureScriptVersion :: IO Version
getPureScriptVersion = do
  let pursProc = inproc "purs" [ "--version" ] empty
  outputLines <- shellToIOText pursProc
  case outputLines of
    [onlyLine]
      | results@(_ : _) <- Read.readP_to_S parseVersion (T.unpack onlyLine) ->
           pure (fst (maximumBy (comparing (length . versionBranch . fst)) results))
      | otherwise -> exitWithErr "Unable to parse output of purs --version"
    _ -> exitWithErr "Unexpected output from purs --version"

initialize :: Maybe Text -> IO ()
initialize packages = do
    exists <- testfile "psc-package.dhall"
    when exists $ exitWithErr "psc-package.dhall already exists"
    echoT "Initializing new project in current directory"
    pkgName <- packageNameFromPWD . pathToTextUnsafe . Path.filename <$> pwd
    (name, depends, packagesUrl) <- case packages of
      Nothing -> do
        pursVersion <- getPureScriptVersion
        echoT ("Using the default package set for PureScript compiler version " <>
          fromString (showVersion pursVersion))
        echoT "(Use --source / --set to override this behavior)"
        pure ( pkgName
             , [ preludePackageName ]
             , "https://raw.githubusercontent.com/justinwoo/spacchetti/8d53f91d9ee32ffc4fc2f755163b80f9026388cd/src/packages.dhall"
             )
      Just packagesUrl ->
        pure ( pkgName
             , [ preludePackageName ]
             , packagesUrl
             )

    writeNewPackageFile name depends packagesUrl
    installImpl =<< readPackageFile
  where
    packageNameFromPWD =
      either (const untitledPackageName) id . mkPackageName

install :: IO ()
install = do
  pkg <- readPackageFile
  installImpl pkg
  echoT "Install complete"

packageNameFromString :: String -> IO PackageName
packageNameFromString str =
  case mkPackageName (pack str) of
    Right pkgName ->
      pure pkgName
    Left _ -> exitWithErr $ "Invalid package name: " <> pack (show str)

listDependencies :: IO ()
listDependencies = do
  pkg@PackageConfig{ depends } <- readPackageFile
  db <- readPackageSet pkg
  trans <- getTransitiveDeps db depends
  traverse_ (echoT . runPackageName . fst) trans

listPackages :: Bool -> IO ()
listPackages sorted = do
  pkg <- readPackageFile
  db <- readPackageSet pkg
  if sorted
    then traverse_ echoT (fmt <$> inOrder (Map.assocs db))
    else traverse_ echoT (fmt <$> Map.assocs db)
  where
  fmt :: (PackageName, PackageInfo) -> Text
  fmt (name, PackageInfo{ version, repo }) =
    runPackageName name <> " (" <> version <> ", " <> repo <> ")"

  inOrder xs = fromNode . fromVertex <$> vs where
    (gr, fromVertex) =
      G.graphFromEdges' [ (pkg, name, dependencies pkg)
                        | (name, pkg) <- xs
                        ]
    vs = G.topSort (G.transposeG gr)
    fromNode (pkg, name, _) = (name, pkg)

getSourcePaths :: PackageConfig -> PackageSet -> [PackageName] -> IO [Turtle.FilePath]
getSourcePaths PackageConfig{..} db pkgNames = do
  trans <- getTransitiveDeps db pkgNames
  let paths = [ ".psc-package"
                </> fromText "set"
                </> fromText (runPackageName pkgName)
                </> fromText version
                </> "src" </> "**" </> "*.purs"
              | (pkgName, PackageInfo{ version }) <- trans
              ]
  return paths

getPaths :: IO [Turtle.FilePath]
getPaths = do
  pkg@PackageConfig{..} <- readPackageFile
  db <- readPackageSet pkg
  getSourcePaths pkg db depends

listSourcePaths :: IO ()
listSourcePaths = do
  paths <- getPaths
  traverse_ (echoT . pathToTextUnsafe) paths

-- | Helper for calling through to @purs@
--
-- Extra args will be appended to the options
exec :: [String] -> Bool -> [String] -> IO ()
exec execNames onlyDeps passthroughOptions = do
  pkg <- readPackageFile
  installImpl pkg

  paths <- getPaths
  let cmdParts = tail execNames
      srcParts = [ "src" </> "**" </> "*.purs" | not onlyDeps ]
  exit
    =<< Process.waitForProcess
    =<< Process.runProcess
          (head execNames)
          (cmdParts <> passthroughOptions
                    <> map Path.encodeString (srcParts <> paths))
          Nothing -- no special path to the working dir
          Nothing -- no env vars
          Nothing -- use existing stdin
          Nothing -- use existing stdout
          Nothing -- use existing stderr

data VerifyArgs a = Package a | VerifyAll (Maybe a) deriving (Functor, Foldable, Traversable)

verify :: VerifyArgs Text -> IO ()
verify arg = do
  pkg <- readPackageFile
  db  <- readPackageSet pkg
  case traverse mkPackageName arg of
    Left pnError -> echoT . pack $ "Error while parsing arguments to verify: " <> show pnError
    Right (Package pName) -> case Map.lookup pName db of
      Nothing -> echoT . pack $ "No packages found with the name " <> show (runPackageName pName)
      Just _  -> do
        reverseDeps <- map fst <$> getReverseDeps db pName
        let packages = pure pName <> reverseDeps
        verifyPackages packages db pkg

    Right (VerifyAll pName) -> verifyPackages packages db pkg
      where
        packages = Map.keys $ maybe db pFilter pName
        pFilter name = Map.filterWithKey (\k _ -> runPackageName k >= runPackageName name) db

  where
    verifyPackages :: [PackageName] -> PackageSet -> PackageConfig -> IO ()
    verifyPackages names db pkg = do
      echoT $ "Verifying " <> pack (show $ length names) <> " packages."
      echoT "Warning: this could take some time!"
      traverse_ (verifyPackage db pkg) names

    verifyPackage :: PackageSet -> PackageConfig -> PackageName -> IO ()
    verifyPackage db _pkg name = do
      let
        dirFor pkgName =
          case Map.lookup pkgName db of
            Nothing -> error ("verifyPackageSet: no directory for " <> show pkgName)
            Just pkgInfo -> performInstall "set" pkgName pkgInfo
      echoT ("Verifying package " <> runPackageName name)
      dependencies <- map fst <$> getTransitiveDeps db [name]
      dirs <- mapConcurrently dirFor dependencies
      let srcGlobs = map (pathToTextUnsafe . (</> ("src" </> "**" </> "*.purs"))) dirs
      procs "purs" ("compile" : srcGlobs) empty

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    join $ Opts.handleParseResult . execParserPure opts =<< getArgs
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "Manage package dependencies"
    footerInfo  = Opts.footer $ "psc-package " ++ showVersion Paths.version

    -- | Displays full command help when invoked with no arguments.
    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $
      Opts.parserFailure Opts.defaultPrefs pinfo Opts.ShowHelpText mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg (showVersion Paths.version)) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Parser (IO ())
    commands = (Opts.subparser . fold)
        [ Opts.command "init"
            (Opts.info (initialize <$> optional (fromString <$> set)
                                   Opts.<**> Opts.helper)
            (Opts.progDesc "Create a new psc-package.dhall file"))
        , Opts.command "install"
            (Opts.info (pure install)
            (Opts.progDesc "Install/update the named package and add it to 'depends' if not already listed. If no package is specified, install/update all dependencies."))
        , Opts.command "build"
            (Opts.info (exec ["purs", "compile"]
                        <$> onlyDeps "Compile only the package's dependencies"
                        <*> passthroughArgs "purs compile"
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Install dependencies and compile the current package"))
        , Opts.command "repl"
            (Opts.info (exec ["purs", "repl"]
                        <$> onlyDeps "Load only the package's dependencies"
                        <*> passthroughArgs "purs repl"
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Open an interactive environment for PureScript"))
        , Opts.command "dependencies"
            (Opts.info (pure listDependencies)
            (Opts.progDesc "List all (transitive) dependencies for the current package"))
        , Opts.command "sources"
            (Opts.info (pure listSourcePaths)
            (Opts.progDesc "List all (active) source paths for dependencies"))
        , Opts.command "available"
            (Opts.info (listPackages <$> sorted Opts.<**> Opts.helper)
            (Opts.progDesc "List all packages available in the package set"))
        , Opts.command "verify"
            (Opts.info (verify <$>
                        ((Package . fromString <$> pkg)
                         <|> (VerifyAll <$> optional (fromString <$> after)))
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Verify that the named package builds correctly. If no package is specified, verify that all packages in the package set build correctly."))
        ]
      where
        pkg = Opts.strArgument $
             Opts.metavar "PACKAGE"
          <> Opts.help "The name of the package to install"

        set = Opts.strOption $
             Opts.long "set"
          <> Opts.help "The package set tag name"

        onlyDeps help = Opts.switch $
             Opts.long "only-dependencies"
          <> Opts.short 'd'
          <> Opts.help help

        passthroughArgs cmd = many $ Opts.strArgument $
             Opts.help ("Options passed through to " <> cmd <> "; use -- to separate")
          <> Opts.metavar ("`" <> cmd <> "`" <> "-options")

        sorted = Opts.switch $
             Opts.long "sort"
          <> Opts.short 's'
          <> Opts.help "Sort packages in dependency order"

        after = Opts.strOption $
             Opts.long "after"
          <> Opts.help "Skip packages before this package during verification"
