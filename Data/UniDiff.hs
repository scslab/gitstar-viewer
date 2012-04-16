{-# LANGUAGE CPP #-}
#if PRODUCTION
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
-- | Simple unified format diff parser
module Data.UniDiff ( LineType(..)
                    , DiffLine(..)
                    , parseDiff
                    ) where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as S8

type S = S8.ByteString

-- | A range contains a line number and count.
data Range = Range { lineNo    :: !Int
                   , lineCount :: !Int
                   } deriving (Eq, Show)

-- | Line type
data LineType = Remove !Int
              | Insert !Int
              | Common !Int
              | Hunk
              deriving Eq

instance Show LineType where
  show (Remove i) = show i
  show (Insert i) = show i
  show (Common i) = show i
  show  Hunk      = "..."

-- | A diff line
data DiffLine = DiffLine { dlineType :: LineType
                         , dlineCont :: S
                         } deriving (Eq, Show)
                         

--
--
--

-- | Given current (remove, insert) line numbers and line content
-- return the new line numbers and \"parsed\" line.
s8ToLine :: (Int, Int) -> S -> (Int, Int, DiffLine)
s8ToLine (r,i) l = case () of
  _ | isRemove l -> (r+1,i  , DiffLine { dlineType = Remove r, dlineCont = l })
  _ | isInsert l -> (r  ,i+1, DiffLine { dlineType = Insert i, dlineCont = l })
  _ | isHunk l   -> (r  ,i  , DiffLine { dlineType = Hunk    , dlineCont = l })
  _              -> (r+1,i+1, DiffLine { dlineType = Common (max r i)
                                                             , dlineCont = l })

-- | Parse all lines of a file
parseLines :: (Int, Int) -> [S] -> Maybe [DiffLine]
parseLines _  [] = return []
parseLines ri (s:ss) = 
  let (r, i, l) = s8ToLine ri s
  in do riNew <- if dlineType l == Hunk
                   then do (rangeR, rangeI) <- parseRanges $ dlineCont l
                           return (lineNo rangeR, lineNo rangeI)
                   else return (r,i)
        dls <- parseLines riNew ss
        return (l:dls)

-- | Parse a file
parseDiff :: S -> Maybe [DiffLine]
parseDiff file = let ls = dropWhile (not . isHunk) $ S8.lines file
                 in parseLines (0,0) ls

--
--

-- | Check remove
isRemove :: S -> Bool
isRemove = (== "-") . (S8.take 1)

-- | Check insert
isInsert :: S -> Bool
isInsert = (== "+") . (S8.take 1)

-- | Check for start of hunk
isHunk :: S -> Bool
isHunk = (== "@@") . (S8.take 2)

-- Parses change chunk of the form:
--
-- > @@ l1[,s1] +l2[,s2] @@ optional section heading
--
parseRanges :: S -> Maybe (Range, Range)
parseRanges line | not (isHunk line) = Nothing
                 | otherwise = 
  let line1 = safeTail $ S8.dropWhile (/='-') line
      (l1,line2) = S8.span (\c -> (not (isSpace c)) && c /= ',') line1
      (s1,line3) =
        if safeHead line2 == ','
          then S8.span (\c -> not (isSpace c) && c /= '+') $ safeTail line2
          else ("1",safeTail line2)
      line4 = S8.dropWhile (\c -> isSpace c || c == '+') line3
      (l2,line5) = S8.span (\c -> not (isSpace c) && c `notElem` [',','@']) line4
      s2 = if safeHead line5 == ','
          then S8.takeWhile (\c -> not (isSpace c) && c /= '@') $ safeTail line5
          else "1"
  in do r1 <- readRange l1 s1
        r2 <- readRange l2 s2
        return (r1, r2)
    where safeHead s = if S8.null s then '\0' else S8.head s
          safeTail s = if S8.null s then S8.empty else S8.tail s
          readRange lS sS = do 
            l <- maybeRead $ S8.unpack lS
            s <- maybeRead $ S8.unpack sS
            return Range { lineNo = l, lineCount = s}

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                [(x, "")] -> Just x
                _         -> Nothing

