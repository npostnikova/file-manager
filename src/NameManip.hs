module NameManip
 ( slicePath
 , guessDotDot
 ) where

 -- | Мне вбросили, что ее лучше не подключать.

import Data.List (intercalate, unfoldr)
import System.FilePath (isPathSeparator, pathSeparator, (</>))

{- | Split a path in components. Repeated \"@\/@\" characters don\'t lead to empty
components. \"@.@\" path components are removed. If the path is absolute, the first component
will start with \"@\/@\". \"@..@\" components are left intact. They can't be simply
removed, because the preceding component might be a symlink. In this case,
'realpath' is probably what you need.

The case that the path is empty, is probably an error. However, it is
treated like \"@.@\", yielding an empty path components list.

Examples:

>slicePath "/"        = ["/"]
>slicePath "/foo/bar" = ["/foo","bar"]
>slicePath "..//./"   = [".."]
>slicePath "."        = []

See 'unslicePath', 'realpath', 'realpath_s'.
-}
slicePath :: String    -- ^ The path to be broken to components.
          -> [String]  -- ^ List of path components.
slicePath "" = []
slicePath (c:cs) = if isPathSeparator c
                       then case slicePath' cs of
                           []     -> [[c]]
                           (p:ps) -> (c:p):ps
                       else slicePath' (c:cs)
    where
      slicePath' o = filter (\c -> c /= "" && c /= ".") (split o)

      split xs = unfoldr f xs
        where
          f "" = Nothing
          f xs = Just $ fmap tail' $ break isPathSeparator xs
          tail' [] = []
          tail' xs = tail xs
          


{- | Form a path from path components. This isn't the inverse
of 'slicePath', since @'unslicePath' . 'slicePath'@
normalises the path.

See 'slicePath'.
-}
unslicePath :: [String]        -- ^ List of path components
             -> String          -- ^ The path which consists of the supplied path components
unslicePath [] = "."
unslicePath cs = intercalate [pathSeparator] cs

{- | Guess the @\"..\"@-component free form of a path, specified as a list of path components, by syntactically removing them, along with the preceding
   path components. This will produce
   erroneous results when the path contains symlinks. If the path contains leading @\"..\"@ components, or more @\"..\"@ components than preceeding normal
   components, then the @\"..\"@ components can't be normalised away. In this case, the result is @Nothing@.
-}
guessDotDotComps :: [String]          -- ^ List of path components
                 -> Maybe [String]    -- ^ In case the path could be transformed, the @\"..\"@-component free list of path components.
guessDotDotComps = guessDotDotComps' []
   where
      guessDotDotComps' schon [] = Just schon
      guessDotDotComps' [] ("..":_) = Nothing
      guessDotDotComps' schon ("..":teile) = guessDotDotComps' (reverse . tail . reverse $ schon) teile
      guessDotDotComps' schon (teil:teile) = guessDotDotComps' (schon ++ [teil]) teile


{- | Guess the @\"..\"@-component free, normalised form of a path. The transformation is purely syntactic. @\"..\"@ path components will be removed, along
   with their preceding path components. This will produce
   erroneous results when the path contains symlinks. If the path contains leading @\"..\"@ components, or more @\"..\"@ components than preceeding normal
   components, then the @\"..\"@ components can't be normalised away. In this case, the result is @Nothing@.

>guessDotDot = fmap unslicePath . guessDotDotComps . slicePath
-}
guessDotDot :: String                  -- ^ Path to be normalised
             -> Maybe String            -- ^ In case the path could be transformed, the normalised, @\"..\"@-component free form of the path.
guessDotDot =
   fmap unslicePath . guessDotDotComps . slicePath