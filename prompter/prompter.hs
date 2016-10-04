import qualified System.Console.ANSI as A
import Control.Monad (filterM)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (listToMaybe, isJust, fromJust, fromMaybe, catMaybes)
import Data.String.Utils (strip)
import System.Directory (getCurrentDirectory, doesDirectoryExist)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>), takeDirectory)
import System.IO (hGetContents)
import System.Process (runInteractiveProcess, waitForProcess)

main :: IO ()
main = do
  parts <- sequenceMaybes [cwdPart, gitPart]
  putStrLn $ "\n[" ++ (unwords parts) ++ "]"

cwdPart :: IO (Maybe String)
cwdPart = Just . dullColor A.Blue <$> getCurrentDirectory

gitPart :: IO (Maybe String)
gitPart = gitRoot >>= mapM gitBranchMsg

gitBranchMsg :: FilePath -> IO String
gitBranchMsg dir = do
  commitId <- gitHead dir
  let stripped = stripPrefix "ref: refs/heads/" commitId in do
    msg <- if isJust stripped
           then return $ fromJust stripped
           else matchingRefMsg commitId
    clean <- isRepoClean dir
    return $ dullColor (if clean then A.Green else A.Red) msg

isRepoClean :: FilePath -> IO Bool
isRepoClean dir = do
  (out, err) <- getCmdOutErr "git" ["status", "--short"] (Just dir)
  return . null . concatMap strip $ [out, err]

gitShowRef :: IO [(String, String)]
gitShowRef = do
  out <- getCmdOut "git" ["show-ref", "--dereference"] Nothing
  return . fmap (split1 " ") . lines $ out

matchingRefs :: String -> IO [String]
matchingRefs commitId = doFiltering <$> gitShowRef where
  doFiltering = fmap cleanupRef .
                fmap snd .
                filter (\x -> fst x == commitId)

cleanupRef :: String -> String
cleanupRef = tryStripPrefix "refs/heads/" .
             tryStripPrefix "refs/remotes/" .
             tryStripPrefix "refs/tags/" .
             tryStripSuffix "^{}"

matchingRefMsg :: String -> IO String
matchingRefMsg commitId = do
  matching <- matchingRefs commitId
  return . parenWrap $ if matching == [] then commitId else (unwords matching)

gitHead :: FilePath -> IO String
gitHead dir = readFileStrip $ dir </> ".git/HEAD"

readFileStrip :: FilePath -> IO String
readFileStrip = fmap strip . readFile

gitRoot :: IO (Maybe FilePath)
gitRoot = do
  cwd <- getCurrentDirectory
  dirs <- filterM isGitRoot (dirAndParents cwd)
  return $ listToMaybe dirs

isGitRoot :: FilePath -> IO Bool
isGitRoot dir = doesDirectoryExist $ dir </> ".git"


-------------------------------------------------------------------------------
--
-- data helpers
--

tryStripPrefix :: Eq a => [a] -> [a] -> [a]
tryStripPrefix = tryMaybe . stripPrefix

tryStripSuffix :: Eq a => [a] -> [a] -> [a]
tryStripSuffix prefix s = reverse (tryStripPrefix (reverse prefix) (reverse s))

parenWrap :: String -> String
parenWrap s = "(" ++ s ++ ")"

dirAndParents :: FilePath -> [FilePath]
dirAndParents dir = let p = takeDirectory dir in
  if p == dir then [dir] else dir : dirAndParents p

dullColor :: A.Color -> String -> String
dullColor color msg = (A.setSGRCode [A.SetColor A.Foreground A.Dull color]) ++
                      msg ++ (A.setSGRCode [A.Reset])

split1 :: Eq a => [a] -> [a] -> ([a], [a])
split1 delim s = (take i s, drop (i + (length delim)) s) where
  i = fromJust (findSublist delim s)

findSublist :: Eq a => [a] -> [a] -> Maybe Int
findSublist sublist whole = findSublist' 0 whole where
  maxI = (length whole) - (length sublist)
  findSublist' i wh
    | i >= maxI               = Nothing
    | sublist `isPrefixOf` wh = Just i
    | otherwise               = findSublist' (i + 1) (tail wh)


-------------------------------------------------------------------------------
--
-- plumbing
--

-- maybeApply :: (a -> Maybe a) -> a -> a
-- maybeApply func x = fromMaybe x (func x)

onFailure :: ExitCode -> (Int -> IO ()) -> IO ()
onFailure (ExitFailure x) func = func x
onFailure ExitSuccess _ = return ()

tryMaybe :: (a -> Maybe a) -> a -> a
tryMaybe func x = fromMaybe x (func x)

sequenceMaybes :: Monad m => [m (Maybe a)] -> m [a]
sequenceMaybes = fmap catMaybes . sequence


-------------------------------------------------------------------------------
--
-- IO helpers
--

getCmdOutErr :: FilePath -> [String] -> Maybe FilePath -> IO (String, String)
getCmdOutErr cmd args workingDir = do
  (_, stdout, stderr, proc') <- runInteractiveProcess
    cmd args workingDir Nothing
  out <- hGetContents stdout
  err <- hGetContents stderr
  code <- waitForProcess proc'
  onFailure code (\x -> error . concat $ [
    cmd, " ", (unwords args), " failed with exit code ",
    show x, ":\n", out, err
    ])
  return (out, err)

getCmdOut :: FilePath -> [String] -> Maybe FilePath -> IO String
getCmdOut cmd args workingDir = fst <$> getCmdOutErr cmd args workingDir


-------------------------------------------------------------------------------
--
-- dev utils
--

-- printList :: Show a => [a] -> IO ()
-- printList = mapM_ print
