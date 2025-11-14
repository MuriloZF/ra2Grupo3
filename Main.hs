{-# LANGUAGE ScopedTypeVariables #-}



-- ============================================================

-- VERSAO ARQUIVO UNICO - Sistema de Inventario

-- Para usar no Online GDB ou Repl.it

-- Cole este arquivo completo e execute!

-- ============================================================



import qualified Data.Map as Map

import Data.Map (Map)

import Data.Time (UTCTime, getCurrentTime)

import Control.Exception (catch, IOException)

import System.IO

import Text.Read (readMaybe)

import Data.List (sortBy, group, sort, isInfixOf, isPrefixOf, tails)

import Data.Ord (comparing, Down(..))



-- Configura encoding para UTF-8 (evita erros no Windows)

setupEncoding :: IO ()

setupEncoding = do

  hSetEncoding stdout utf8

  hSetEncoding stdin utf8

  hSetEncoding stderr utf8



-- ============================================================

-- LÓGICA PURA (Funções de Negócio)

-- ============================================================



-- Adiciona um novo item ao inventario

addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao

addItem time item inv =

  if Map.member (itemID item) inv

    then Left "Item ja existe"

    else Right (novoInv, logEntry)

  where

    novoInv = Map.insert (itemID item) item inv

    logEntry = LogEntry time Add ("Item ID[" ++ itemID item ++ "] Nome[" ++ nome item ++ "] adicionado") Sucesso



-- Remove quantidade de um item do inventario

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao

removeItem time itemId qtd inv =

  case Map.lookup itemId inv of

    Nothing -> Left "Item nao encontrado"

    Just item ->

      if quantidade item < qtd

        then Left "Estoque insuficiente"

        else Right (novoInv, logEntry)

      where

        novoItem = item { quantidade = quantidade item - qtd }

        novoInv = Map.insert itemId novoItem inv

        logEntry = LogEntry time Remove ("Removido " ++ show qtd ++ " do Item ID[" ++ itemId ++ "] Nome[" ++ nome item ++ "]") Sucesso



-- Deleta um item completamente do inventario

deleteItem :: UTCTime -> String -> Inventario -> Either String ResultadoOperacao

deleteItem time itemId inv =

  case Map.lookup itemId inv of

    Nothing -> Left "Item nao encontrado"

    Just item -> Right (novoInv, logEntry)

      where

        novoInv = Map.delete itemId inv

        logEntry = LogEntry time Remove ("Item ID[" ++ itemId ++ "] Nome[" ++ nome item ++ "] deletado completamente") Sucesso



-- Atualiza a quantidade de um item no inventario

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao

updateQty time itemId novaQtd inv =

  case Map.lookup itemId inv of

    Nothing -> Left "Item nao encontrado"

    Just item ->

      if novaQtd < 0

        then Left "Quantidade nao pode ser negativa"

        else Right (novoInv, logEntry)

      where

        novoItem = item { quantidade = novaQtd }

        novoInv = Map.insert itemId novoItem inv

        logEntry = LogEntry time Update ("Atualizado Item ID[" ++ itemId ++ "] Nome[" ++ nome item ++ "] para " ++ show novaQtd) Sucesso
