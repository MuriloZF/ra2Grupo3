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

-- TIPOS DE DADOS

data Item = Item {
  itemID :: String,
  nome :: String,
  quantidade :: Int,
  categoria :: String
} deriving (Show, Read)

type Inventario = Map String Item

data AcaoLog = Add | Remove | Update | QueryFail
  deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
  deriving (Show, Read)

data LogEntry = LogEntry {
  timestamp :: UTCTime,
  acao :: AcaoLog,
  detalhes :: String,
  status :: StatusLog
} deriving (Show, Read)

type ResultadoOperacao = (Inventario, LogEntry)

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


-- PERSISTENCIA (I/O)

-- Lê inventário do disco (com leitura estrita para evitar file lock)
lerInventario :: IO Inventario
lerInventario = catch leitura (\(_ :: IOException) -> return mempty)  -- Se der erro (arquivo não existe), retorna mapa vazio
  where
    leitura = do
      handle <- openFile "Inventario.dat" ReadMode  -- Abre o arquivo pra leitura
      content <- hGetContents handle  -- Lê todo o conteúdo
      let resultado = case readMaybe content of  -- Tenta converter o texto em Inventario
                        Just inv -> inv  -- Deu certo, Usa o inventário
                        Nothing  -> mempty  -- Falhou, Retorna vazio
      length content `seq` hClose handle  -- Truque pra forcar ler tudo antes de fechar (evita file lock)
      return resultado  -- Retorna o inventário

-- Salva inventário no disco (sobrescreve arquivo)
salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile "Inventario.dat" (show inv)  -- Escreve o inventário no arquivo (sobrescreve tudo)

-- Registra log no arquivo de auditoria (append)
registrarLog :: LogEntry -> IO ()
registrarLog logEntry = appendFile "Auditoria.log" (show logEntry ++ "\n")  -- Adiciona o log no final do arquivo (não apaga nada)

-- Lê logs do disco (com leitura estrita para evitar file lock)
lerLogs :: IO [LogEntry]
lerLogs = catch leitura (\(_ :: IOException) -> return [])  -- Se der erro, retorna lista vazia
  where
    leitura = do
      handle <- openFile "Auditoria.log" ReadMode  -- Abre arquivo de log
      content <- hGetContents handle  -- Lê tudo
      let ls = filter (not . null) (lines content)  -- Quebra em linhas e remove linhas vazias
          resultado = [entry | line <- ls, Just entry <- [readMaybe line]]  -- Converte cada linha em LogEntry (ignora linhas inválidas)
      length content `seq` hClose handle  -- Força leitura completa antes de fechar
      return resultado  -- Retorna lista de logs

--RELATORIOS (Analise de Logs)







-- FUNÇÕES AUXILIARES (I/O)

-- Mostra um prompt e lê a entrada do usuário
lerInput :: String -> IO String
lerInput prompt = putStr prompt >> hFlush stdout >> getLine

-- Lê um inteiro, repetindo até o usuário digitar algo válido
lerInt :: String -> IO Int
lerInt prompt = do
  input <- lerInput prompt
  case readMaybe input of
    Just n  -> return n
    Nothing -> putStrLn "Entrada invalida! Digite um numero." >> lerInt prompt

-- Trata o resultado das operações (erro ou sucesso)
processarResultado :: Either String (Inventario, LogEntry) -> IO (Inventario, [LogEntry])
processarResultado (Left erro) = do
  putStrLn ("Erro: " ++ erro)            -- Exibe o erro
  inv <- lerInventario                   -- Inventário continua igual
  logs <- lerLogs                        -- Carrega logs atuais
  tempo <- getCurrentTime
  let logErro = LogEntry tempo QueryFail erro (Falha erro)  -- Log de falha
  registrarLog logErro                   -- Salva no arquivo de log
  return (inv, logErro : logs)

processarResultado (Right (novoInv, logEntry)) = do
  salvarInventario novoInv               -- Atualiza Inventario.dat
  registrarLog logEntry                  -- Registra log da operação
  putStrLn "Operacao realizada com sucesso!"
  logs <- lerLogs
  return (novoInv, logEntry : logs)





-- LOOP PRINCIPAL


loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
  cmd <- lerInput "\nComando (add/remove/delete/update/listar/report/historico/sair): "  -- Pede pro usuário digitar um comando
  case cmd of  -- Verifica qual comando foi digitado
    "add" -> do  -- Comando pra adicionar item
      itemId <- lerInput "ID: "
      nomeItem <- lerInput "Nome: "
      qtd <- lerInt "Quantidade: "
      cat <- lerInput "Categoria: "
      tempo <- getCurrentTime  -- Pega o horário atual
      let item = Item itemId nomeItem qtd cat  -- Monta o item com os dados
      (novoInv, novosLogs) <- processarResultado (addItem tempo item inv)  -- Chama a função pura e processa o resultado
      loop novoInv novosLogs  -- Chama o loop de novo com os dados atualizados

    "remove" -> do  -- Comando pra remover quantidade
      itemId <- lerInput "ID do item: "
      qtd <- lerInt "Quantidade a remover: "
      tempo <- getCurrentTime  -- Pega a hora
      (novoInv, novosLogs) <- processarResultado (removeItem tempo itemId qtd inv)  -- Chama função pura e processa
      loop novoInv novosLogs  -- Continua o loop

    "delete" -> do  -- Comando pra deletar item completamente
      itemId <- lerInput "ID do item a deletar completamente: "
      tempo <- getCurrentTime  -- Pega a hora
      (novoInv, novosLogs) <- processarResultado (deleteItem tempo itemId inv)  -- Deleta e processa
      loop novoInv novosLogs  -- Continua

    "update" -> do  -- Comando pra atualizar quantidade
      itemId <- lerInput "ID do item: "
      novaQtd <- lerInt "Nova quantidade: "  
      tempo <- getCurrentTime
      (novoInv, novosLogs) <- processarResultado (updateQty tempo itemId novaQtd inv)  -- Atualiza e processa
      loop novoInv novosLogs  -- Continua o loop

    "listar" -> do  -- Comando pra mostrar todos os itens
      putStrLn "\n=== Itens no Inventario ==="  -- Título
      if Map.null inv  -- Verifica se o inventário tá vazio
        then putStrLn "Inventario vazio"
        else mapM_ (\item -> putStrLn ("ID: " ++ itemID item ++ " | Nome: " ++ nome item ++  -- Se não, mostra cada item
                                       " | Qtd: " ++ show (quantidade item) ++ 
                                       " | Categoria: " ++ categoria item)) (Map.elems inv)  -- Pega todos os itens do mapa
      loop inv logs  -- Continua nao mudou nada so mostrou

    "report" -> do
      -- Recarrega logs do disco para garantir dados atualizados
      logsAtualizados <- lerLogs
      
      putStrLn "\n=== Relatorio Completo ==="
      putStrLn ("\nTotal de itens no inventario: " ++ show (Map.size inv))
      putStrLn ("Total de operacoes registradas: " ++ show (length logsAtualizados))
      
      putStrLn "\n--- Itens no Inventario ---"
      if Map.null inv
        then putStrLn "Inventario vazio"
        else mapM_ (\item -> putStrLn ("ID: " ++ itemID item ++ " | Nome: " ++ nome item ++ 
                                       " | Qtd: " ++ show (quantidade item) ++ 
                                       " | Categoria: " ++ categoria item)) (Map.elems inv)
      
      putStrLn "\n--- Logs de Sucesso ---"
      let sucessos = logsDeSucesso logsAtualizados
      if null sucessos
        then putStrLn "Nenhuma operacao de sucesso registrada"
        else do
          putStrLn ("Total de operacoes bem sucedidas: " ++ show (length sucessos))
          mapM_ print sucessos
      
      putStrLn "\n--- Logs de Erros ---"
      let erros = logsDeErro logsAtualizados
      if null erros
        then putStrLn "Nenhum erro registrado"
        else do
          putStrLn ("Total de erros: " ++ show (length erros))
          mapM_ print erros
      
      putStrLn "\n--- Item Mais Movimentado ---"
      case itemMaisMovimentado inv logsAtualizados of
        Nothing -> putStrLn "Nenhum item movimentado ainda"
        Just (itemId, nomeItem, count) -> 
          putStrLn ("Item: ID=" ++ itemId ++ " | Nome=" ++ nomeItem ++ " | Operacoes=" ++ show count)
      loop inv logsAtualizados
    
    "historico" -> do
      itemId <- lerInput "ID do item para ver historico: "
      logsAtualizados <- lerLogs
      let historico = historicoPorItem itemId logsAtualizados
      putStrLn ("\n=== Historico do Item " ++ itemId ++ " ===")
      if null historico
        then putStrLn "Nenhuma operacao registrada para este item"
        else do
          putStrLn ("Total de operacoes: " ++ show (length historico))
          mapM_ print historico
      loop inv logsAtualizados

    "sair" -> putStrLn "Encerrando. Ate logo!"

    _ -> putStrLn "Comando invalido. Use: add, remove, delete, update, listar, report, historico, sair" >> loop inv logs


-- MAIN (Ponto de Entrada)


main :: IO ()
main = do
  setupEncoding  -- Configura UTF-8 pra não dar problema com acentos no Windows
  inv <- lerInventario  -- Carrega o inventário do disco (se não existir, começa vazio)
  logs <- lerLogs  -- Carrega os logs do disco (se não existir, lista vazia)
  putStrLn "=== Sistema de Inventario ==="  -- Mensagem de boas-vindas
  putStrLn ("Inventario carregado com " ++ show (Map.size inv) ++ " itens")  -- Mostra quantos itens tem
  loop inv logs  -- Começa o loop interativo


