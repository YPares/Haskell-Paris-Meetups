{-# LANGUAGE OverloadedStrings #-}

module Main where
import MFlow.Wai.Blaze.Html.All
import Control.Applicative

main = do
  addMessageFlows [("sum", transient . runFlow $ sum_it_one_page)]
  wait $ run 8081 waiMessageFlow

sum_it = do
  let get msg = ask $     p (msg :: Html)
                      ++> getInt Nothing
                      <* submitButton "Voila"
  setHeader $ html . body
  x <- get "Donne-moi un nombre :"
  y <- get "Maintenant un autre :"
  ask $     p (toHtml $ "Resultat : " ++ show (x+y))
        ++> wlink () "Recommencer"



sum_it_one_page = do
  setHeader $ html . body
  (x, y) <- ask $ (,) <$> p "Donne deux nombres"
                      ++> getInt Nothing <++ br
                      <*> getInt Nothing <++ br
                      <* submitButton "Voila"
  ask $     p (toHtml $ "Resultat : " ++ show (x+y))
        ++> wlink () "Recommencer"

