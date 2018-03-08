module Main where
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console(CONSOLE,logShow)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Button (myButton)

main :: Eff (console :: CONSOLE | HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myButton unit body
