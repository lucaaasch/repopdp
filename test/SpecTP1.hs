module SpecTP1 where
import PdePreludat

import TP1
import Test.Hspec
import Control.Exception (evaluate)

spec = do
  describe "Es mes" $ do
    it "El número 1 es un mes" $ do
      esMes 1 `shouldBe` True
    it "El número 7 es un mes" $ do
      esMes 7 `shouldBe` True
    it "El número 12 es un mes" $ do
      esMes 12 `shouldBe` True
    it "El número 0 no es un mes" $ do
      esMes 0 `shouldBe` False
    it "El número 13 no es un mes" $ do
      esMes 13 `shouldBe` False
  describe "Mes anterior y siguiente" $ do
    it "El mes siguiente de enero (1) es febrero (2)" $ do
      mesSiguiente 1 `shouldBe` 2
    it "El mes anterior de enero (1) es diciembre (12)" $ do
      mesAnterior 1 `shouldBe` 12
    it "El mes siguiente de agosto (8) es septiembre (9)" $ do
      mesSiguiente 8 `shouldBe` 9
    it "El mes anterior de agosto (8) es julio (7)" $ do
      mesAnterior 8 `shouldBe` 7
    it "El mes siguiente de diciembre (12) es enero (1)" $ do
      mesSiguiente 12 `shouldBe` 1
    it "El mes anterior de diciembre (12) es noviembre (11)" $ do
      mesAnterior 12 `shouldBe` 11
    it "La función mesAnterior no está definida para números que no sean meses" $ do
      evaluate (mesAnterior 0) `shouldThrow` anyException
    it "La función mesSiguiente no está definida para números que no sean meses" $ do
      evaluate (mesSiguiente 13) `shouldThrow` anyException
  describe "Estaciones" $ do
    it "En enero es verano" $ do
      estacion 1 `shouldBe` "verano"
    it "En febrero es verano" $ do
      estacion 2 `shouldBe` "verano"
    it "En marzo es verano y otoño" $ do
      estacion 3 `shouldBe` "verano/otonio"
    it "En abril es otoño" $ do
      estacion 4 `shouldBe` "otonio"
    it "En mayo es otoño" $ do
      estacion 5 `shouldBe` "otonio"
    it "En junio es otoño e invierno" $ do
      estacion 6 `shouldBe` "otonio/invierno"
    it "En julio es invierno" $ do
      estacion 7 `shouldBe` "invierno"
    it "En agosto es invierno" $ do
      estacion 8 `shouldBe` "invierno"
    it "En septiembre es invierno y primavera" $ do
      estacion 9 `shouldBe` "invierno/primavera"
    it "En octubre es primavera" $ do
      estacion 10 `shouldBe` "primavera"
    it "En noviembre es primavera" $ do
      estacion 11 `shouldBe` "primavera"
    it "En diciembre es primavera y verano" $ do
      estacion 12 `shouldBe` "primavera/verano"
    it "La función estacion no está definida para números que no sean meses" $ do
      evaluate (estacion 15) `shouldThrow` anyException