import Data.List (isPrefixOf, sort, intersperse)
import Text.Regex.Posix

data Version = Version {unVersion :: String} deriving (Eq, Ord, Show)


main :: IO ()
main = do
  s <- readFile "db/structure.sql" 
  writeFile "db/resolved_structure.sql" $ resolve s
  where 
    resolve :: String -> String
    resolve s = 
      let
        structurePrefix :: String -> [String]
        structurePrefix = takeWhile (\e -> (not $ schemaPrefix `isPrefixOf` e)) . lines

        resolveVersions :: String -> [String]
        resolveVersions = addSpacing . map writeVersion . sort . uniqVersions . map readVersion . filterJunk . lines
      in
        unlines $ (structurePrefix s) ++ (resolveVersions s)

    schemaPrefix = "INSERT INTO schema_migrations (version) VALUES ('"
    schemaSuffix = "');"

    filterJunk :: [String] -> [String]
    filterJunk = filter $ isPrefixOf schemaPrefix

    readVersion :: String -> Version
    readVersion l = Version $ l =~ "[0-9]+"

    uniqVersions :: Eq a => [a] -> [a]
    uniqVersions vs = foldl (\acc v -> if v `elem` acc then acc else v:acc) [] vs

    addSpacing :: [String] -> [String]
    addSpacing ss = intersperse "" ss

    writeVersion :: Version -> String
    writeVersion v = schemaPrefix ++ unVersion v ++ schemaSuffix
