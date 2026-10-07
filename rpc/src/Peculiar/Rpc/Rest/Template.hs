module Peculiar.Rpc.Rest.Template
  ( Template (..)
  , Segment (..)
  , parseTemplate
  , matchTemplate
  , openApiPath
  , variables
  ) where

import Data.Text qualified as T

data Segment
  = Literal T.Text
  | One
  | Rest
  | Variable [T.Text] [Segment]
  deriving stock (Eq, Show)

data Template = Template
  { segments :: [Segment]
  , verb :: Maybe T.Text
  }
  deriving stock (Eq, Show)

parseTemplate :: T.Text -> Maybe Template
parseTemplate text = do
  body <- T.stripPrefix "/" text
  let (path, verb) = case T.breakOnEnd ":" body of
        (before, after)
          | not (T.null before) && not (T.isInfixOf "}" after) && not (T.isInfixOf "/" after) -> (T.dropEnd 1 before, Just after)
        _ -> (body, Nothing)
  segments <- parseSegments path
  pure Template{segments, verb}

parseSegments :: T.Text -> Maybe [Segment]
parseSegments = go . T.unpack
 where
  go = \case
    [] -> Just []
    '{' : rest -> do
      let (inside, after) = break (== '}') rest
      closing <- case after of
        '}' : more -> Just more
        _ -> Nothing
      let (fieldPath, shape) = break (== '=') inside
      nested <- case shape of
        '=' : sub -> parseSegments (T.pack sub)
        _ -> Just [One]
      (Variable (T.splitOn "." (T.pack fieldPath)) nested :) <$> continue closing
    input ->
      let (piece, after) = break (== '/') input
       in (segment piece :) <$> continue after
  continue = \case
    [] -> Just []
    '/' : more -> go more
    _ -> Nothing
  segment = \case
    "*" -> One
    "**" -> Rest
    piece -> Literal (T.pack piece)

matchTemplate :: Template -> [T.Text] -> Maybe [([T.Text], T.Text)]
matchTemplate template pieces = do
  withoutVerb <- case (template.verb, reverse pieces) of
    (Nothing, _) -> Just pieces
    (Just verb, lastPiece : before) -> do
      stem <- T.stripSuffix (":" <> verb) lastPiece
      Just (reverse (stem : before))
    (Just _, []) -> Nothing
  (bound, remaining) <- walk template.segments withoutVerb
  if null remaining then Just bound else Nothing
 where
  walk segments input = case segments of
    [] -> Just ([], input)
    segment : more -> case (segment, input) of
      (Literal literal, piece : rest) | literal == piece -> walk more rest
      (One, _ : rest) -> walk more rest
      (Rest, rest) -> walk more (drop (length rest - minimum' more) rest)
      (Variable field nested, _) -> do
        let width = spanOf nested input more
        let (taken, rest) = splitAt width input
        _ <- walk nested taken >>= \(_, left) -> if null left then Just () else Nothing
        (bound, left) <- walk more rest
        Just ((field, T.intercalate "/" taken) : bound, left)
      _ -> Nothing
  minimum' more = length [() | segment <- more, segment /= Rest]
  spanOf nested input more
    | Rest `elem` nested = length input - minimum' more
    | otherwise = length nested

openApiPath :: Template -> T.Text
openApiPath template = "/" <> T.intercalate "/" (fmap render template.segments) <> maybe "" (":" <>) template.verb
 where
  render = \case
    Literal literal -> literal
    One -> "{_}"
    Rest -> "{__}"
    Variable field _ -> "{" <> T.intercalate "." field <> "}"

variables :: Template -> [([T.Text], [Segment])]
variables template = [(field, nested) | Variable field nested <- template.segments]
