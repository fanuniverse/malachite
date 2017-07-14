module Scraper.Internal.DOM ( (<@)
                            , hasAttr
                            , hasClass
                            , firstText
                            , firstAttr
                            , firstMetaContent) where

import Data.Text (Text, isInfixOf)

import Text.HTML.TagSoup (Tag, isTagOpen, isTagText, fromAttrib, fromTagText)

infixr 5 <@
(<@) :: (a -> Bool) -> [a] -> [a]
f <@ xs = dropWhile (not . f) xs

hasAttr :: Text -> Text -> Tag Text -> Bool
hasAttr attr content tag
  | isTagOpen tag = content `isInfixOf` (fromAttrib attr tag)
  | otherwise     = False

hasClass :: Text -> Tag Text -> Bool
hasClass = hasAttr "class"

firstText :: [Tag Text] -> Text
firstText ts = fromTagText (head (isTagText <@ ts))

firstAttr :: Text -> [Tag Text] -> Text
firstAttr attr ts = fromAttrib attr (head ts)

firstMetaContent :: Text -> [Tag Text] -> Text
firstMetaContent prop ts =
  firstAttr "content" (hasAttr "property" prop <@ ts)
