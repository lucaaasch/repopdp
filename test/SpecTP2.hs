module SpecTP2 where
import PdePreludat

import TP2
import Test.Hspec

spec = do
  describe "Introducción a las fichas" $ do 
    it "Invertir la ficha 1-1 queda 1-1" $ do
      invertir (Ficha 1 1) `shouldBe` (Ficha 1 1)
    it "Invertir la ficha 1-4 queda 4-1" $ do
      invertir (Ficha 1 4) `shouldBe` (Ficha 4 1)
    
    it "Las fichas 4-2 y 4-2 son la misma ficha" $ do
      esLaMismaFicha (Ficha 4 2) (Ficha 4 2) `shouldBe` True
    it "Las fichas 4-2 y 2-4 son la misma ficha" $ do
      esLaMismaFicha (Ficha 4 2) (Ficha 2 4) `shouldBe` True
    it "Las fichas 1-1 y 2-2 no son la misma ficha" $ do
      esLaMismaFicha (Ficha 1 1) (Ficha 2 2) `shouldBe` False
    it "Las fichas 1-2 y 1-3 no son la misma ficha" $ do
      esLaMismaFicha (Ficha 1 2) (Ficha 1 3) `shouldBe` False
    it "Las fichas 2-1 y 3-1 no son la misma ficha" $ do
      esLaMismaFicha (Ficha 2 1) (Ficha 3 1) `shouldBe` False
    it "Las fichas 1-2 y 2-3 no son la misma ficha" $ do
      esLaMismaFicha (Ficha 1 2) (Ficha 2 3) `shouldBe` False
    it "Las fichas 1-2 y 6-1 no son la misma ficha" $ do
      esLaMismaFicha (Ficha 1 2) (Ficha 6 1) `shouldBe` False
      
  describe "\nJugar Ronda" $ do 
    it "Al jugar una ronda en la que ambos jugadores pueden conectar sus fichas en extremos diferentes, ambos extremos cambian" $ do
      jugarRonda (Ficha 1 5) (Ficha 6 3) (Ficha 3 2, Ficha 3 5) `shouldBe` (Ficha 6 3, Ficha 5 1)
    it "Al jugar una ronda en la que el primero puede conectar su ficha pero no el segundo, sólo un extremo cambia" $ do
      jugarRonda (Ficha 5 1) (Ficha 5 6) (Ficha 3 2, Ficha 3 5) `shouldBe` (Ficha 3 2, Ficha 5 1)
    it "Al jugar una ronda en la que el segundo puede conectar su ficha pero no el primero, sólo un extremo cambia" $ do
      jugarRonda (Ficha 4 4) (Ficha 2 5) (Ficha 3 2, Ficha 3 5) `shouldBe` (Ficha 3 2, Ficha 5 2)
    it "Al jugar una ronda en la que nadie puede conectar su ficha, el tablero queda igual" $ do
      jugarRonda (Ficha 4 4) (Ficha 2 6) (Ficha 3 2, Ficha 3 5) `shouldBe` (Ficha 3 2, Ficha 3 5)
    it "El segundo jugador puede colocar una ficha conectándola con la jugada por el primero de la ronda" $ do
      jugarRonda (Ficha 3 1) (Ficha 1 6) (Ficha 3 2, Ficha 3 5) `shouldBe` (Ficha 6 1, Ficha 3 5)
    it "En caso de poder jugar una ficha en ambos extremos, la misma se conecta sólo en el primer extremo" $ do
      jugarRonda (Ficha 4 4) (Ficha 3 6) (Ficha 3 2, Ficha 5 3) `shouldBe` (Ficha 6 3, Ficha 5 3)

  -- Agrego mis casos de prueba:
  -- La idea de esto es tratar de hacer que una jugada dependa de otra anterior, para ver si despues de tantas jugadas se mantiene la logica

  describe "\nCasos de prueba agregados:" $ do 

    it "En caso de jugar dos veces en el mismo tablero" $ do
      jugarRonda (Ficha 3 2) (Ficha 2 3) (jugarRonda (Ficha 1 5) (Ficha 6 3) (Ficha 6 1, Ficha 6 1)) `shouldBe` (Ficha 3 2, Ficha 1 5)

    -- Declaro algunos estados del tablero que seran usados posteriormente

    let tableroPostDosJugadas = jugarRonda (Ficha 3 2) (Ficha 2 3) (jugarRonda (Ficha 1 5) (Ficha 6 3) (Ficha 6 1, Ficha 6 1))
    let tableroPostTresJugadas = jugarRonda (Ficha 6 6) (Ficha 6 5) tableroPostDosJugadas
    let tableroPostCuatroJugadas = jugarRonda (Ficha 3 1) (Ficha 1 1) tableroPostTresJugadas

    it "En caso de jugar tres veces en el mismo tablero" $ do
      jugarRonda (Ficha 6 6) (Ficha 6 5) tableroPostDosJugadas `shouldBe` (Ficha 3 2, Ficha 5 6)
    
    it "En caso de jugar cuatro veces en el mismo tablero" $ do
      jugarRonda (Ficha 3 1) (Ficha 1 1) tableroPostTresJugadas `shouldBe` (Ficha 1 1, Ficha 5 6)

    it "En caso de jugar cinco veces en el mismo tablero y con ambas fichas invertidas" $ do
      jugarRonda (invertir (Ficha 1 2)) (invertir (Ficha 6 5)) tableroPostCuatroJugadas `shouldBe` (Ficha 2 1, Ficha 6 5)

    it "En caso de jugar cinco veces, pero el tablero esta invertido" $ do
      jugarRonda (Ficha 6 1) (Ficha 5 5) tableroPostCuatroJugadas `shouldBe` (invertir(Ficha 1 6), invertir(Ficha 6 5))

    it "En caso de jugar cinco veces, pero ninguna pieza conecta" $ do
      jugarRonda (Ficha 0 3) (Ficha 4 4) tableroPostCuatroJugadas `shouldBe` tableroPostCuatroJugadas