{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Skell.Keymap (
                     mkKeymap,
                     -- defaultEmacsKeymap
                    ) where

import           Control.Applicative ((<$), (<$>))
import           Data.List
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as S
import           Text.Parsec

import           Skell.Types
-- import           Skell.Debug


modifiersKeys :: Stream s m Char => ParsecT s () m Keys
modifiersKeys =
    KCtrl  <$ char 'C' <|>
    KMeta  <$ char 'M' <|>
    KShift <$ char 'S' <|>
    KAlt   <$ char 'A'

normalKeys :: Stream s m Char => ParsecT s () m Keys
normalKeys = choice
      [ KEsc <$ string "Esc"
      , KBS  <$ string "BackSpace"
      , KEnter <$ string "Enter"
      , KUpLeft <$ string "UpLeft"
      , KUpRight <$ string "UpRight"
      , KDownLeft <$ string "Downleft"
      , KDownRight <$ string "DownRight"
      , KLeft <$ string "Left"
      , KRight <$ string "Right"
      , KUp <$ string "Up"
      , KDown <$ string "Down"
      , KCenter <$ string "Center"
      , KBackTab <$ string "BackTab" -- shift+tab
      , KPrtScr <$ string "PrtScr"
      , KPause <$ string "Pause"
      , KIns <$ string "Ins"
      , KHome <$ string "Home"
      , KPageUp <$ string "PageUp"
      , KDel <$ string "Del"
      , KEnd <$ string "End"
      , KPageDown <$ string "PageDown"
      , KBegin <$ string "Begin"
      , KMenu <$ string "Menu"
      , do _ <- char 'F'
           r <- optionMaybe $ many1 digit
           case r of
             Nothing -> return $ KChar 'F'
             Just x  -> return $ KFun $ read x
      , KChar <$> anyChar
      ]

-- | Parsea una cadena dada, devolviendo su equivalente en teclas presionadas
-- * C == Ctrl
-- * M == Windows' key
-- * S == Shift
-- * A == Alt
-- La forma de combinarlos en mediante \'-\'; Ej \"C-S-d\"
parseKeys :: String -> Either ParseError (Seq  Keys)
parseKeys s = let parser = many1 $ do
                    mods <- many modif
                    key  <- normalKeys
                    _ <- char '-' <|> return '\0'
                    return(mods++[key]) -- TODO:
              in S.fromList . concat <$> runParser parser () "KeyParse" s
    where
        modif = do
            key <- modifiersKeys
            _ <- char '-'
            return key


mkKeymap :: [(String, IOSkell Bool)] -> Seq Keys -> IOSkell Bool
mkKeymap ls sq = do
  ls' <- convert ls
  -- if ls' isEmpty?
  -- defaultKeymap >> debug "A empty keymap as input, be proceded with an defaultKeymap"
  case find ((==sq) . fst) ls' of
    Just (_,act) -> act
    Nothing -> return False

convert :: [(String, IOSkell Bool)] -> IOSkell [(Seq Keys, IOSkell Bool)]
convert [] = return []
convert ((x,act):xs) = case parseKeys x of
                         Left _ -> convert xs -- debug "No parse key" ++ err
                         Right b -> fmap  ((b,act):) (convert xs)

-- defaultEmacsKeymap :: Seq Keys -> IOSkell Bool
-- defaultEmacsKeymap sq
--     | S.drop (S.length sq - 1) sq == ctrlG = return True
--     | otherwise = mkKeymap
--                   [ ("C-x-s", exit .= True >> return True) -- TODO
--                   , ("Left", edit.buffers._1._1 %= moveLeftChar >> return True)
--                   , ("Right", edit.buffers._1._1 %= moveRightChar >> return True)
--                   , ("C-Left", edit.buffers._1._1 %= prevToken >> return True)
--                   , ("C-Right", edit.buffers._1._1 %= nextToken >> return True)] sq

--     where Right ctrlG = parseKeys "C-g"
