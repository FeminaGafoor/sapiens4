import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.Info (os)
import System.Process

main :: IO ()
main = do
 root <- getCurrentDirectory
 let output = root </> ".build/spec-modules"
     runtime = root </> ".build/spec"
     dependencies = root </> "build/dependencies/node_modules"
 removePathForcibly runtime
 createDirectoryIfMissing True output
 when (os=="mingw32") $ copyFile (dependencies </> "purescript/purs.bin") (output </> "purs.exe")
 path <- getEnv "PATH"
 setEnv "PATH" (output ++ [searchPathSeparator] ++ (dependencies </> ".bin") ++ [searchPathSeparator] ++ path)
 forM_ ["app","corpora",".build/domain"] $ \path -> copyTree (root </> path) (runtime </> path)
 let web = runtime </> ".build/web"
 createDirectoryIfMissing True web
 listDirectory (root </> ".build/web") >>= mapM_ (\name -> do
  let source = root </> ".build/web" </> name
  file <- doesFileExist source
  when file $ copyFile source (web </> name))
 copyTree (root </> "app") (runtime </> "src")
 copyTree (root </> "spec/browser") (runtime </> "src/spec")
 forM_ ["spago.yaml","spago.lock"] $ \name -> copyFile (root </> "build/purescript" </> name) (runtime </> name)
 withCurrentDirectory runtime $ callProcess "node"
  [dependencies </> "spago/bin/bundle.js","bundle","--pure","--quiet","--force",
   "--module","Spec.Browser","--output",output,"--outfile",web </> "app.js","--platform","browser"]
 template <- readFile (root </> "app/shell/index.html")
 fixture <- readFile (root </> "spec/browser/controls.html")
 setup <- readFile (root </> "app/shell/bootstrap.html")
 let beforeBody = takeWhile (/= "</body>") (lines template)
     setupBody = takeWhile (/= "</main>") $ dropWhile (/= "<main hidden>") (lines setup)
 writeFile (runtime </> "app/shell/index.html") $ unlines (filter (/= "  <script src=\"/workspace/app.js\"></script>") beforeBody) ++ fixture ++ unlines setupBody ++ "</main>\n<script src=\"/workspace/app.js\"></script></body></html>\n"
 where
 copyTree source target = do
  createDirectoryIfMissing True target
  listDirectory source >>= mapM_ (\name -> do
   let from = source </> name; to = target </> name
   directory <- doesDirectoryExist from
   if directory then copyTree from to else copyFile from to)
