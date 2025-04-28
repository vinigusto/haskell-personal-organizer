module Persistencia where

import Funcoes
import Tipos
import System.IO
import Control.Exception (catch, IOException)

-- Função para salvar tarefas em arquivos após serializar a informação
salvarEmArquivo :: FilePath -> [Tarefa] -> IO()
salvarEmArquivo path tarefas = do
    withFile path WriteMode (\h -> hPutStr h (show tarefas))

-- Função para carregar tarefas de um arquivo
carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo path = do
    conteudo <- readFile path `catch` handler
    if null conteudo 
       then return []
       else case reads conteudo of
            [(tarefas, "")] -> return tarefas
            _ -> return []
  where
    handler :: IOException -> IO String
    handler _ = return "[]"
