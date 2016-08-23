import qualified System.Console.ANSI as A
import Control.Monad (filterM, when)
import Control.Monad.Extra (concatMapM)
import Data.List (intercalate, stripPrefix)
import Data.Maybe (listToMaybe, isJust, fromJust, fromMaybe, catMaybes)
import Data.String.Utils (strip)
import GHC.IO.Handle (hGetContents)
import System.Directory (getCurrentDirectory, doesDirectoryExist, findFile)
import System.Directory.PathWalk (pathWalkLazy)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>), takeDirectory, addTrailingPathSeparator)
import System.Process (runInteractiveProcess, waitForProcess)

main = do
  parts <- promptParts
  putStrLn $ "\n[" ++ (unwords parts) ++ "]"

promptParts :: IO [String]
promptParts = fmap catMaybes . sequence $ [fmap Just cwdPart, gitPart]

cwdPart :: IO String
cwdPart = do
  cwd <- getCurrentDirectory
  return $ dullColor A.Blue cwd

gitPart :: IO (Maybe String)
gitPart = gitRoot >>= mapM gitBranch

gitBranch :: FilePath -> IO String
gitBranch dir = do
  head <- gitHead dir
  let stripped = stripPrefix "ref: refs/heads/" head in do
    msg <- if isJust stripped
           then return $ fromJust stripped
           else matchingRefMsg head dir
    clean <- isRepoClean dir
    return $ dullColor (if clean then A.Green else A.Red) msg

isRepoClean :: FilePath -> IO Bool
isRepoClean dir = do
  (_, stdout, stderr, proc') <- runInteractiveProcess
    "git" ["status", "--short"] (Just dir) Nothing
  out <- hGetContents stdout
  err <- hGetContents stderr
  code <- waitForProcess proc'
  onFailure code (\x -> error . concat $ [
    "'git status --short' failed with exit code ", show x, ":\n", out, err])
  return . null . concatMap strip $ [out, err]

matchingRefMsg :: String -> FilePath -> IO String
matchingRefMsg commitId dir = fmap (parenWrap . unwords)
                                   (fullMatchingRefs commitId dir)

fullMatchingRefs :: String -> FilePath -> IO [String]
fullMatchingRefs commitId dir = do
  refs <- concatMapM (\x -> matchingRefs commitId (dir </> ".git/refs" </> x))
                     ["heads", "remotes", "tags"]
  return $ if null refs then [commitId] else refs

matchingRefs :: String -> FilePath -> IO [String]
matchingRefs commitId dir = do
  files <- findFiles dir >>= filterM (isRefMatching commitId)
  return $ map (stripPrefixSafe $ addTrailingPathSeparator dir) files

isRefMatching :: String -> String -> IO Bool
isRefMatching commitId path = fmap (== commitId) (readFileStrip path)

findFiles :: FilePath -> IO [FilePath]
findFiles dir = do
  steps <- pathWalkLazy dir
  return $ concatMap (\(dir, dirs, files) -> map (dir </>) files) steps

gitHead :: FilePath -> IO String
gitHead dir = readFileStrip $ dir </> ".git/HEAD"

readFileStrip :: FilePath -> IO String
readFileStrip = fmap strip . readFile

gitRoot :: IO (Maybe FilePath)
gitRoot = do
  cwd <- getCurrentDirectory
  dirs <- filterM isGitRoot (selfAndParents cwd)
  return $ listToMaybe dirs

isGitRoot :: FilePath -> IO Bool
isGitRoot dir = doesDirectoryExist $ dir </> ".git"


-------------------------------------------------------------------------------
--
-- data helpers
--

stripPrefixSafe :: String -> String -> String
stripPrefixSafe prefix s = fromMaybe s (stripPrefix prefix s)

parenWrap :: String -> String
parenWrap s = "(" ++ s ++ ")"

selfAndParents :: FilePath -> [FilePath]
selfAndParents dir = let p = takeDirectory dir in
  if p == dir then [dir] else dir : selfAndParents p

dullColor :: A.Color -> String -> String
dullColor color msg = (A.setSGRCode [A.SetColor A.Foreground A.Dull color]) ++
                      msg ++ (A.setSGRCode [A.Reset])

-------------------------------------------------------------------------------
--
-- plumbing
--

-- maybeApply :: (a -> Maybe a) -> a -> a
-- maybeApply func x = fromMaybe x (func x)

onFailure :: ExitCode -> (Int -> IO ()) -> IO ()
onFailure (ExitFailure x) func = func x
onFailure ExitSuccess _ = return ()

-------------------------------------------------------------------------------
--
-- dev utils
--

printList :: Show a => [a] -> IO ()
printList = mapM_ print
