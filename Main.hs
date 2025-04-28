module Main (main) where

import Tipos
import Funcoes
import FuncoesInterface
import Persistencia
import System.IO
import Control.Monad (unless)

main :: IO()
main = do hSetBuffering stdout NoBuffering
          listaPreExistente <- carregarDeArquivo "tarefas.txt"
          mainLoop listaPreExistente
           
mainLoop :: [Tarefa] -> IO ()
mainLoop lista = do
   lista <- carregarDeArquivo "tarefas.txt"
   mostrarCabecalho
   menuPrincipal
   opcao <- getLine
   unless (opcao == "17") $ do
     case opcao of
       "1"  -> adicionarTarefaIO lista >>= mainLoop
       "2"  -> listarTarefasIO lista >>= mainLoop
       "3"  -> removerTarefaIO lista >>= mainLoop
       "4"  -> marcarComoConcluídaIO lista >>= mainLoop 
       "5"  -> listarCategoriaIO lista >>= mainLoop
       "6"  -> listarPrioridadeIO lista >>= mainLoop
       "7"  -> ordenarPrioridadeIO lista >>= mainLoop
       "8"  -> listarStatusIO lista >>= mainLoop
       "9"  -> buscarPalavraChaveIO lista >>= mainLoop
       "10" -> tarefasAtrasadasIO lista >>= mainLoop
       "11" -> mostrarDiasRestantesIO lista >> mainLoop lista
       "12" -> listarPorTagIO lista >>= mainLoop
       "13" -> mostrarNuvemTagsIO lista >> mainLoop lista
       "14" -> listarRelatorioIO lista >> mainLoop lista
       "15" -> limparListaIO lista >> mainLoop lista
       "16" -> preencherListaIO lista >> mainLoop lista
       _    -> do putStrLn "Opção inválida, tente novamente"
                  mainLoop lista
          
          
mostrarCabecalho :: IO()
mostrarCabecalho = do
          putStrLn "=========================================="
          putStrLn "       SISTEMA DE GESTÃO DE TAREFAS       "
          putStrLn "=========================================="
          
          
menuPrincipal :: IO()
menuPrincipal = do
        putStrLn "╔════════════════════════════════════════╗"
        putStrLn "║         MENU PRINCIPAL - TAREFAS       ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 1  │ Adicionar Tarefa                  ║"
        putStrLn "║ 2  │ Listar Todas as Tarefas           ║"
        putStrLn "║ 3  │ Remover Tarefa                    ║"
        putStrLn "║ 4  │ Marcar como Concluída             ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 5  │ Filtrar por Categoria             ║"
        putStrLn "║ 6  │ Filtrar por Prioridade            ║"
        putStrLn "║ 7  │ Ordenar por Prioridade            ║"
        putStrLn "║ 8  │ Filtrar por Status                ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 9  │ Buscar por Descrição              ║"
        putStrLn "║ 10 │ Tarefas Atrasadas                 ║"
        putStrLn "║ 11 │ Prazo Restante                    ║"
        putStrLn "║ 12 │ Buscar por TAG                    ║"
        putStrLn "║ 13 │ Mostrar Estatística de TAGs       ║"
        putStrLn "║ 14 │ Criar Relatório                   ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 15 │ Esvaziar Lista de Tarefas         ║"
        putStrLn "║ 16 │ Preencher Lista com Testes        ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 17 │ Sair                              ║"
        putStrLn "╚════════════════════════════════════════╝"
        putStrLn "Digite o número da opção desejada: "
         
