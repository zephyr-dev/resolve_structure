import Data.List (isPrefixOf, isInfixOf, sort, intersperse)
import Text.Regex.Posix
import qualified System.IO.Strict as S

data Version = Version {unVersion :: String} deriving (Eq, Ord, Show)

schemaPrefix = "INSERT INTO schema_migrations (version) VALUES ('"
schemaSuffix = "');"
conflictMarker = "<<<"
migrationVersionPattern = "[0-9]+"
structureFilename = "db/structure.sql"

main :: IO ()
main = do
  s <- S.readFile structureFilename
  case resolve s of
    Just rs ->
      writeFile structureFilename rs
    Nothing -> 
      putStrLn "Merge conflicts found in structure that cannot be auto-resolved, please resolve manually"
  where 
    resolve :: String -> Maybe String
    resolve s = 
      let
        structurePrefix :: String -> [String]
        structurePrefix = takeWhile (\e -> (not $ schemaPrefix `isPrefixOf` e)) . lines

        resolveVersions :: String -> [String]
        resolveVersions = addSpacing . map writeVersion . sort . uniqVersions . map readVersion . filterJunk . lines

        mergeConflictInSchema :: [String] -> Bool
        mergeConflictInSchema = any (conflictMarker `isInfixOf`)
      in
        if mergeConflictInSchema $ structurePrefix s
          then Nothing
          else Just $ unlines $ (structurePrefix s) ++ (resolveVersions s)

    filterJunk :: [String] -> [String]
    filterJunk = filter $ isPrefixOf schemaPrefix

    readVersion :: String -> Version
    readVersion l = Version $ l =~ migrationVersionPattern

    uniqVersions :: Eq a => [a] -> [a]
    uniqVersions = foldl (\acc v -> if v `elem` acc then acc else v:acc) []

    addSpacing :: [String] -> [String]
    addSpacing = intersperse ""

    writeVersion :: Version -> String
    writeVersion v = schemaPrefix ++ unVersion v ++ schemaSuffix
