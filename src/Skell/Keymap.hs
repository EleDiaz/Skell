{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Skell.Keymap (
                     mkKeymap,
                     parseKeys,
                     -- defaultEmacsKeymap
                    ) where

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


mkKeymap :: [(String, IOSkell ())] -> Seq Keys -> IOSkell ()
mkKeymap ls sq = do
  ls' <- convert ls
  -- if ls' isEmpty?
  -- defaultKeymap >> debug "A empty keymap as input, be proceded with an defaultKeymap"
  case find ((==sq) . fst) ls' of
    Just (_,act) -> act
    Nothing -> return ()

convert :: [(String, IOSkell ())] -> IOSkell [(Seq Keys, IOSkell ())]
convert [] = return []
convert ((x,act):xs) = case parseKeys x of
                         Left _ -> convert xs -- debug "No parse key" ++ err
                         Right b -> fmap  ((b,act):) (convert xs)


