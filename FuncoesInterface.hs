module FuncoesInterface where

import Tipos
import Funcoes
import Persistencia
import Data.Char
import Data.Time.Calendar
import Data.Time (getCurrentTime, utctDay, Day)
import Data.List (find, intercalate)
import Text.Read (readMaybe)
import Text.Printf (printf)

listarRelatorioIO  :: [Tarefa] -> IO [Tarefa]
listarRelatorioIO tarefas = do
                   let total = length tarefas
                       totalFloat = fromIntegral total :: Float
                       pendentes = calculaPendentes tarefas
                       concluidas = total - pendentes
                       trabalhos = calculaTrab tarefas
                       estudos = calculaEst tarefas
                       pessoal = calculaPessoal tarefas
                       outros = total - (trabalhos+estudos+pessoal)
                       trabalhosFloat = fromIntegral trabalhos :: Float
                       estudosFloat = fromIntegral estudos :: Float
                       pessoalFloat = fromIntegral pessoal :: Float
                       outrosFloat = fromIntegral outros :: Float
                       trabalhoPerc = (trabalhosFloat/totalFloat)* 100
                       estudosPerc = (estudosFloat/totalFloat) * 100
                       pessoalPerc = (pessoalFloat/totalFloat) * 100
                       outrosPerc = (outrosFloat/totalFloat) * 100
                       
                   putStrLn "\n=== RELATÓRIO RESUMIDO ==="
                   putStrLn "Relatório Resumido:"
                   putStrLn ("- Total de tarefas: " ++ show total)
                   putStrLn ("- Pendentes: " ++ show pendentes ++ "| Concluídas:" ++ show concluidas)
                   putStrLn ("- Distribuição por categoria: ")
                   putStrLn (" * Trabalho: " ++ show trabalhos ++ " tarefas (" ++ formatarPercentual trabalhoPerc ++ "%)")
                   putStrLn (" * Estudos: " ++ show estudos ++ " tarefas (" ++ formatarPercentual estudosPerc ++ "%)")
                   putStrLn (" * Pessoal: " ++ show pessoal ++ " tarefas (" ++ formatarPercentual pessoalPerc ++ "%)")
                   putStrLn (" * Outros: " ++ show outros ++ " tarefas (" ++ formatarPercentual outrosPerc ++ "%)")
                   putStrLn "\n"
                   return tarefas
                     where
                       formatarPercentual :: Float -> String
                       formatarPercentual = printf "%.1f"
                   
mostrarNuvemTagsIO :: [Tarefa] -> IO ()
mostrarNuvemTagsIO tarefas = do
    putStrLn "\n=== FREQUÊNCIA DE TAGS ==="
    mapM_ mostrarTag (nuvemDeTags tarefas)
    putStrLn "========================="
      where
        mostrarTag (tag, freq) = 
          putStrLn $ "- " ++ tag ++ ": " ++ show freq ++ " usos"


listarPorTagIO :: [Tarefa] -> IO [Tarefa]
listarPorTagIO tarefas = do
         putStrLn "Tag:"
         tag <- getLine
         let tarefasFiltradas = filtrarPorTag tag tarefas
         putStrLn "\n=== LISTA FILTRADA ==="
         if null tarefasFiltradas
                    then putStrLn "Nenhuma tarefa registrada com essa tag.\n"
                    else mapM_ mostrarTarefa tarefasFiltradas
         putStrLn "\n"
         return tarefas
            where
               mostrarTarefa tarefa = do
                     putStrLn $ "ID: "            ++ show (idTarefa tarefa) ++
                                " | Descrição: "   ++ descricao tarefa ++
                                " | Status: "     ++ show (status tarefa) ++
                                " | Categoria: " ++ show (categoria tarefa)
                                
        
mostrarDiasRestantesIO :: [Tarefa] -> IO ()
mostrarDiasRestantesIO lista = do
    putStrLn "\nDigite o ID da tarefa:"
    idInput <- getLine
    case readMaybe idInput of
        Just id -> 
            case find (\t -> idTarefa t == id) lista of
                Just tarefa -> auxiliarDiasRestantes tarefa
                Nothing -> putStrLn "Tarefa não encontrada!\n"
        Nothing -> putStrLn "ID inválido!\n"
        

auxiliarDiasRestantes :: Tarefa -> IO ()
auxiliarDiasRestantes tarefa = do
    hoje <- utctDay <$> getCurrentTime
    case calcularDiasRestantes tarefa hoje of
        Nothing       -> putStrLn "Esta tarefa não possui prazo definido.\n"
        Just dias 
            | dias < 0  -> putStrLn $ "Tarefa ATRASADA! (" ++ show (abs dias) ++ " dias)\n"
            | dias == 0 -> putStrLn "Tarefa VENCE HOJE!\n"
            | otherwise -> putStrLn $ "Dias restantes: " ++ show dias ++"\n"


tarefasAtrasadasIO :: [Tarefa] -> IO [Tarefa]
tarefasAtrasadasIO tarefas = do
         dataAtual <- getCurrentTime
         let hoje = utctDay dataAtual  
         let tarefasAtrasadas = verificarAtrasos tarefas hoje
         
         putStrLn "\n=== TAREFAS ATRASADAS ==="
         if null tarefasAtrasadas
                    then putStrLn "Nenhuma tarefa atrasada foi encontrada.\n"
                    else mapM_ mostrarTarefa tarefasAtrasadas
         putStrLn "\n"
         
         return tarefasAtrasadas
            where
               mostrarTarefa tarefa = putStrLn $ 
                                "ID: "            ++ show (idTarefa tarefa) ++
                                " | Descrição: "   ++ descricao tarefa ++
                                " | Prioridade: " ++ show (prioridade tarefa) ++
                                " | Prazo: "      ++ maybe "Sem prazo" show (prazo tarefa)


buscarPalavraChaveIO :: [Tarefa] -> IO [Tarefa]
buscarPalavraChaveIO tarefas = do
         putStrLn "Digite a Palavra Chave da Descrição:"
         palavra <- getLine
         let tarefasFiltradas = buscarPorPalavraChave palavra tarefas
         putStrLn "\n=== LISTA FILTRADA ==="
         if null tarefasFiltradas
                    then putStrLn "Nenhuma tarefa registrada com esse status.\n"
                    else mapM_ mostrarTarefa tarefasFiltradas
         putStrLn "\n"
         return tarefas
            where
               mostrarTarefa tarefa = do
                     putStrLn $ "ID: "            ++ show (idTarefa tarefa) ++
                                " | Descrição: "   ++ descricao tarefa ++
                                " | Categoria: "  ++ show (categoria tarefa) ++
                                " | Prioridade: " ++ show (prioridade tarefa) ++
                                " | Status: "     ++ show (status tarefa)


ordenarPrioridadeIO :: [Tarefa] -> IO [Tarefa]
ordenarPrioridadeIO tarefas = do
    putStrLn "\n=== LISTA ORDENADA ==="
    mapM_ listarGrupo [Alta, Media, Baixa]
    putStrLn "\n"
    return tarefas
      where
        listarGrupo prio = do
          putStrLn $ "\n" ++ show prio ++ ":"
          let grupo = listarPorPrioridade prio tarefas
          if null grupo 
            then putStrLn "(Nenhuma tarefa)"
            else mapM_ mostrarTarefa grupo
        mostrarTarefa tarefa = do
          putStrLn $ "ID: " ++ show (idTarefa tarefa) ++
                     " | Descrição: "   ++ descricao tarefa ++
                     " | Categoria: "  ++ show (categoria tarefa) ++
                     " | Prioridade: " ++ show (prioridade tarefa)


listarStatusIO :: [Tarefa] -> IO [Tarefa]
listarStatusIO tarefas = do
         putStrLn "Escolha o Status (Pendente/Concluído):"
         stat <- getLine
         let statStr = case stat of
                    "Pendente" -> Pendente
                    _          -> Concluída
         let tarefasFiltradas = filtrarPorStatus statStr tarefas
         putStrLn "\n=== LISTA FILTRADA ==="
         if null tarefasFiltradas
                    then putStrLn "Nenhuma tarefa registrada com esse status.\n"
                    else mapM_ mostrarTarefa tarefasFiltradas
         putStrLn "\n"
         return tarefas
            where
               mostrarTarefa tarefa = do
                     putStrLn $ "ID: "            ++ show (idTarefa tarefa) ++
                                " | Descrição: "   ++ descricao tarefa ++
                                " | Categoria: "  ++ show (categoria tarefa) ++
                                " | Prioridade: " ++ show (prioridade tarefa)
                                
                                
listarPrioridadeIO :: [Tarefa] -> IO [Tarefa]
listarPrioridadeIO tarefas = do
         putStrLn "Escolha a Prioridade (Baixa/Media/Alta):"
         prio <- getLine
         let prioStr = case prio of
                    "Baixa" -> Baixa
                    "Media" -> Media
                    "Alta"  -> Alta
                    _       -> Media
         let tarefasFiltradas = listarPorPrioridade prioStr tarefas
         putStrLn "\n=== LISTA FILTRADA ==="
         if null tarefasFiltradas
                    then putStrLn "Nenhuma tarefa registrada nessa categoria.\n"
                    else mapM_ mostrarTarefa tarefasFiltradas
         putStrLn "\n"
         return tarefas
            where
               mostrarTarefa tarefa = do
                     putStrLn $ "ID: "            ++ show (idTarefa tarefa) ++
                                " | Descrição: "   ++ descricao tarefa ++
                                " | Status: "     ++ show (status tarefa) ++
                                " | Categoria: " ++ show (categoria tarefa)


listarCategoriaIO :: [Tarefa] -> IO [Tarefa]
listarCategoriaIO tarefas = do
         putStrLn "Escolha a Categoria (Trabalho/Estudos/Pessoal/Outro):"
         cat <- getLine
         let catStr = case cat of
                    "Trabalho" -> Trabalho
                    "Estudos"  -> Estudos
                    "Pessoal"  -> Pessoal
                    _          -> Outro
         let tarefasFiltradas = listarPorCategoria catStr tarefas
         putStrLn "\n=== LISTA FILTRADA ==="
         if null tarefasFiltradas
                    then putStrLn "Nenhuma tarefa registrada nessa categoria."
                    else mapM_ mostrarTarefa tarefasFiltradas
         putStrLn "\n"
         return tarefas
            where
               mostrarTarefa tarefa = do
                     putStrLn $ "ID: "            ++ show (idTarefa tarefa) ++
                                " | Descrição: "   ++ descricao tarefa ++
                                " | Status: "     ++ show (status tarefa) ++
                                " | Prioridade: " ++ show (prioridade tarefa)
                     
                     
marcarComoConcluídaIO :: [Tarefa] -> IO [Tarefa]
marcarComoConcluídaIO l = do
          putStrLn "ID da tarefa concluida:"
          idTarefa <- getLine
          case readMaybe idTarefa of
            Just id1 -> do
              let listaAtual = marcarConcluída id1 l
              --print listaAtual
              putStrLn "\n✔ Tarefa atualizada com sucesso!\n"
              salvarEmArquivo "tarefas.txt" listaAtual
              return listaAtual
            Nothing -> do
              putStrLn "\nID inválido!\n"
              return l
          
          
listarTarefasIO :: [Tarefa] -> IO [Tarefa]
listarTarefasIO tarefas = do
         putStrLn "\n=== LISTA DE TAREFAS ==="
         mapM_ mostrarTarefa tarefas
         putStrLn "\n"
         return tarefas
            where
               mostrarTarefa tarefa = do
                     putStrLn $ "ID: " ++ show (idTarefa tarefa) ++
                                " | Status: "   ++ show (status tarefa) ++
                                " | Prioridade: " ++ show (prioridade tarefa) ++
                                " | Categoria: " ++ show (categoria tarefa) ++
                                " | Descrição: " ++ descricao tarefa ++
                                " | Prazo: " ++ maybe "Nenhum" show (prazo tarefa) ++
                                " | Tags: " ++ intercalarTags (tags tarefa)
               intercalarTags [] = "Nenhuma"
               intercalarTags ts = intercalate ", " ts
                     
removerTarefaIO :: [Tarefa] -> IO [Tarefa]
removerTarefaIO l = do
          putStrLn "ID da tarefa:"
          idTarefa <- getLine
          case readMaybe idTarefa of
            Just id1 -> do
              let listaAtual = removerTarefa id1 l
              --print listaAtual
              putStrLn "\n✔ Tarefa removida com sucesso!\n"
              salvarEmArquivo "tarefas.txt" listaAtual
              return listaAtual
            Nothing -> do
              putStrLn "\nID inválido!\n"
              return l
          
          
adicionarTarefaIO :: [Tarefa] -> IO [Tarefa]
adicionarTarefaIO l = do
          putStrLn "Descrição:"
          descricao <- getLine
          putStrLn "Status (Concluída/Pendente):"
          status <- getLine
          putStrLn "Prioridade (Baixa/Media/Alta):"
          prioridade <- getLine
          putStrLn "Categoria (Trabalho/Estudos/Pessoal/Outro):"
          categoria <- getLine
          putStrLn "Prazo (YYYY-MM-DD ou deixe vazio):"
          prazoStr <- getLine
          putStrLn "Informe as TAGs (separadas por espaço):"
          tags <- getLine
          
          let status1     = case status of
                           "Pendente" -> Pendente
                           _          -> Concluída
              prioridade1 = case prioridade of
                           "Baixa" -> Baixa
                           "Media" -> Media
                           _       -> Alta
              categoria1 = case categoria of
                           "Trabalho" -> Trabalho
                           "Estudos"  -> Estudos
                           "Pessoal"  -> Pessoal
                           _          -> Outro
              tags1 = words tags
              prazo1 = parsePrazo prazoStr
              id1 = case l of
                    [] -> 1
                    _  -> acharMaiorID l + 1
              t1 = Tarefa id1 descricao status1 prioridade1 categoria1 prazo1 tags1
              listaAtual = adicionarTarefa t1 l
              
          --print listaAtual
          putStrLn "\n✔ Tarefa adicionada com sucesso!\n"   
          salvarEmArquivo "tarefas.txt" listaAtual
          return listaAtual
       where
          parsePrazo "" = Nothing
          parsePrazo str = case reads str of
                           [(d, "")] -> Just d
                           _ -> Nothing
              
              
limparListaIO :: [Tarefa] -> IO [Tarefa]
limparListaIO _ = do
    putStrLn "\n✔ Lista de tarefas foi limpa e arquivo atualizado!\n"
    salvarEmArquivo "tarefas.txt" []
    return []


preencherListaIO :: [Tarefa] -> IO [Tarefa]
preencherListaIO _ = do
    salvarEmArquivo "tarefas.txt" tarefasTeste
    putStrLn "\n✔ Lista preenchida com tarefas de teste e arquivo atualizado!\n"
    return tarefasTeste
