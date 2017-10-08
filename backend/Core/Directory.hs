module Core.Directory
  ( FilePath
  , (</>)
  , findFiles
  , normalize
  , dropExtension
  , doesFileExist
  , removeFile
  )
  where

import Core.Basics
import Core.Bool (Bool)
import Core.List (List)
import Core.Maybe (Maybe(..))
import qualified Core.List as List
import qualified System.Directory
import qualified System.FilePath


type FilePath =
    System.FilePath.FilePath


(</>) =
    (System.FilePath.</>)


infixr 5 </>


normalize =
    System.FilePath.normalise


dropExtension =
    System.FilePath.dropExtension


doesFileExist =
    System.Directory.doesFileExist


removeFile =
    System.Directory.removeFile


findFiles :: FilePath -> IO (List FilePath)

findFiles "" =
    findFiles "."
    & fmap (List.map (List.dropLeft 2))

findFiles dir = do
    dirs <- findDirectories dir
    fmap (fmap List.concat) $ forM (dir:dirs) $ \dir ->
        System.Directory.listDirectory dir
        & fmap (
            List.map ( \ fn -> do
              fileExists <- System.Directory.doesFileExist (dir </> fn)
              if fileExists then
                  return (Just (dir </> fn))
              else
                  return Nothing
            )
          )
        & join . fmap sequence
        & fmap (List.filterMap identity)

findDirectories :: FilePath -> IO (List FilePath)
findDirectories =
    findWith System.Directory.doesDirectoryExist


findWith :: (FilePath -> IO Bool) -> FilePath -> IO (List FilePath)

findWith pred "" = do
    findWith pred "."

findWith pred dir = do
    let
        find_ dir =
            System.Directory.listDirectory dir
            & fmap (
                List.map ( \ fn -> do
                  isTrue <- pred (dir </> fn)
                  if isTrue then
                      return (Just (dir </> fn))
                  else
                      return Nothing
                )
              )
            & join . fmap sequence
            & fmap (List.filterMap identity)
    dirs <- find_ dir
    (++) dirs <$> (fmap List.concat $ mapM find_ dirs)
