{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( PackageName
  , packageNameDhallType
  , mkPackageName
  , runPackageName
  , preludePackageName
  , untitledPackageName
  ) where

import           Control.Category ((>>>))
import           Data.Char (isAscii, isLower, isDigit)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Dhall as Dhall
import qualified Dhall.Core as Dhall.Core

newtype PackageName
  = PackageName Text
  deriving (Show, Eq, Ord)

packageNameDhallType :: Dhall.Type PackageName
packageNameDhallType = Dhall.Type { Dhall.extract = extract, Dhall.expected = expected }
  where
    extract (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) = case mkPackageName t of
      Right pkgName -> return pkgName
      Left _ -> Nothing
    extract _ = Nothing
    expected = Dhall.Core.Text

data PackageNameError
  = NotEmpty
  | TooLong Int
  | InvalidChars String
  | RepeatedSeparators
  | MustNotBeginSeparator
  | MustNotEndSeparator
  deriving (Show, Eq, Ord)

-- | Smart constructor for package names. Based on Bower's requirements for
-- | package names.
mkPackageName :: Text -> Either PackageNameError PackageName
mkPackageName = fmap PackageName . validateAll validators
  where
  dashOrDot = ['-', '.']
  validateAll vs x = mapM_ (validateWith x) vs >> return x
  validateWith x (p, err)
    | p x       = Right x
    | otherwise = Left (err x)
  validChar c = isAscii c && (isLower c || isDigit c || c `elem` dashOrDot)
  validators =
      [ (not . T.null, const NotEmpty)
      , (T.all validChar, InvalidChars . T.unpack . T.filter (not . validChar))
      , (firstChar (`notElem` dashOrDot), const MustNotBeginSeparator)
      , (lastChar (`notElem` dashOrDot), const MustNotEndSeparator)
      , (not . T.isInfixOf "--", const RepeatedSeparators)
      , (not . T.isInfixOf "..", const RepeatedSeparators)
      , (T.length >>> (<= 50), TooLong . T.length)
      ]
  firstChar p str = not (T.null str) && p (T.index str 0)
  lastChar p = firstChar p . T.reverse

runPackageName :: PackageName -> Text
runPackageName (PackageName t) = t

preludePackageName :: PackageName
preludePackageName = PackageName "prelude"

untitledPackageName :: PackageName
untitledPackageName = PackageName "untitled"
