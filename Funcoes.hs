module Funcoes where

import Tipos
import Data.Time.Calendar (Day, diffDays, fromGregorian)

-----------------------------------
-------- FUNÇÕES BÁSICAS ----------
-----------------------------------

-- Insere no início da lista (O(1))
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]	
adicionarTarefa novaTarefa tarefas = novaTarefa : tarefas

-- Remove primeira tarefa com ID correspondente
removerTarefa :: Int -> [Tarefa] -> [Tarefa]
removerTarefa _ [] = []
removerTarefa id (x:xs) | idTarefa x == id = xs
                        | otherwise = x : removerTarefa id xs

-- Altera status de tarefa para Concluída por ID
marcarConcluída :: Int -> [Tarefa] -> [Tarefa]
marcarConcluída id [] = []
marcarConcluída id (x:xs) | idTarefa x == id = x {status = Concluída} : xs
                          | otherwise = x : marcarConcluída id xs

-----------------------------------
-------- FUNÇÕES AVANÇADAS --------
-----------------------------------

-- Filtra tarefas por categoria
listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria cat ts = filter condicao ts
 where
  condicao = (\ts -> categoria ts == cat)

-- Filtra tarefas por prioridade
listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade prior ts = filter condicao ts
 where
  condicao = (\ts -> prioridade ts == prior)

-- Filtra tarefas por status
filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus stat ts = filter condicao ts
 where
  condicao = (\ts -> status ts == stat)

-- Ordena por prioridade (Alta > Média > Baixa)
ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade ts = listarPorPrioridade Alta ts  ++
                          listarPorPrioridade Media ts ++
                          listarPorPrioridade Baixa ts

-- Busca tarefas contendo palavra na descrição, usa words para separá-la palavra a palavra
buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave palavra ts = filter condicao ts
 where
  condicao = (\ts -> procurarString palavra (words(descricao ts)) == palavra)

-----------------------------------
------- FUNÇÕES ADICIONAIS --------
-----------------------------------

-- Construtor rápido com valores padrão
-- Tarefa: idTarefa, descrição, status, prioridade, caregoria, prazo, tags
criarTarefaPadrão :: Int -> String -> Prioridade -> Categoria -> Tarefa
criarTarefaPadrão id desc prio cat = Tarefa id desc Pendente prio cat Nothing []

-- Auxiliar: verifica se palavra está na lista
procurarString :: String -> [String] -> String
procurarString _ [] = ""
procurarString palavra (x:xs) | palavra == x = palavra
                              | otherwise    = procurarString palavra xs

-- Contar quantidade de tarefas
contarTarefas :: [Tarefa] -> Int
contarTarefas [] = 0
contarTarefas tarefas = length tarefas

-- Achar maior ID
acharMaiorID :: [Tarefa] -> Int
acharMaiorID [] = 0
acharMaiorID [x] = idTarefa x
acharMaiorID (x:y:xs) | idTarefa x > idTarefa y = max (idTarefa x) (acharMaiorID (x:xs))
                      | otherwise = max (idTarefa y) (acharMaiorID (y:xs))

-----------------------------------
-------- GESTÃO DE PRAZOS ---------
-----------------------------------

-- Retorna tarefas com prazos expirados
verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos ts dataAtual = filter condicao ts
 where
  condicao ts = case prazo ts of
   Nothing -> False
   Just dataPrazo -> dataPrazo < dataAtual

-- Calcula quantos dias faltam para o prazo de uma tarefa
calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
calcularDiasRestantes tarefa dataAtual =
 case prazo tarefa of
  Just dataPrazo -> Just (fromInteger $ diffDays dataPrazo dataAtual)
  Nothing        -> Nothing

-------------------------------------
--------- SISTEMA DE TAGS -----------
-------------------------------------

-- Retorna tarefas que possuem uma respectiva tag
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
filtrarPorTag tag ts = filter condicao ts
 where
  condicao = (\ts -> procurarString tag (tags ts) == tag)

-- Gera uma lista de tags e suas frequências de uso (Lista de Tuplas)
nuvemDeTags :: [Tarefa] -> [(String, Int)]
nuvemDeTags tarefas = [(tag, length (filter (== tag) todasTags))
                      |tag <- removerRepetidos (quickSort todasTags)]
                      where
                       todasTags = formarListaDeTags tarefas

-- Forma uma lista com as tags de todas as tarefas
formarListaDeTags :: [Tarefa] -> [String]
formarListaDeTags ts = [ tag | t <- ts, tag <- tags t]

-- Remove elementos repetidos de uma lista
removerRepetidos :: Eq a => [a] -> [a]
removerRepetidos [] = []
removerRepetidos [x] = [x]
removerRepetidos (x:y:xs) | x == y = removerRepetidos (y:xs)
                          | otherwise = x:removerRepetidos(y:xs)

-- Ordena uma lista
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort menores ++ [x] ++ quickSort maiores
 where
  menores = filter (< x) xs
  maiores = filter (>= x) xs
  
-------------------------------------
------- GERADOR DE RELATÓRIO --------
-------------------------------------

--Calcula quantas tarefas estão pendentes para gerar relatório
calculaPendentes :: [Tarefa] -> Int
calculaPendentes [] = 0
calculaPendentes (x:xs) |status x == Pendente = 1 + calculaPendentes xs
                        |otherwise = calculaPendentes xs
                        
--Calcula quantas tarefas são da categoria trabalho gerar relatório
calculaTrab :: [Tarefa] -> Int
calculaTrab [] = 0
calculaTrab (x:xs) |categoria x == Trabalho = 1 + calculaTrab xs
                   |otherwise = calculaTrab xs

--Calcula quantas tarefas são da categoria estudos para gerar relatório
calculaEst :: [Tarefa] -> Int
calculaEst [] = 0
calculaEst (x:xs) |categoria x == Estudos = 1 + calculaEst xs
                  |otherwise = calculaEst xs

--Calcula quantas tarefas são da categoria pessoal para gerar relatório
calculaPessoal :: [Tarefa] -> Int
calculaPessoal [] = 0
calculaPessoal (x:xs) |categoria x == Pessoal = 1 + calculaPessoal xs
                      |otherwise = calculaPessoal xs
                    
                    
--------------------------------------
-- TESTES RETIRAR ANTES DE COMPILAR --
--------------------------------------

hojeTeste :: Day
hojeTeste = fromGregorian 2025 04 20

t1 = Tarefa 1 "Reuniao importante" Pendente Alta Trabalho (Just $ fromGregorian 2025 04 15) ["Trabalho", "Urgencia"]
t2 = Tarefa 2 "Ir no mercado" Pendente Media Pessoal (Just $ fromGregorian 2025 04 25) ["Casa", "Comida"]
t3 = Tarefa 3 "Estudar Haskell" Concluída Baixa Estudos Nothing ["PF", "Faculdade"]
t4 = Tarefa 4 "Enviar Trabalho PF" Pendente Alta Estudos Nothing ["PF", "Trabalho"]
t5 = Tarefa 5 "Mandar mensagem professor" Pendente Media Estudos Nothing ["SD", "Faculdade"]
tarefas = [t1,t2,t3,t4]

tarefasTeste :: [Tarefa]
tarefasTeste = [
    Tarefa 1 "Entregar relatório" Pendente Alta Trabalho (Just (fromGregorian 2024 05 15)) ["urgente", "cliente"],
    Tarefa 2 "Reunião de equipe" Pendente Media Trabalho (Just (fromGregorian 2025 04 23)) ["reunião"],
    Tarefa 3 "Estudar Haskell" Concluída Alta Estudos Nothing ["programação", "prova"],
    Tarefa 4 "Comprar presentes" Pendente Baixa Pessoal (Just (fromGregorian 2024 12 20)) ["natal"],
    Tarefa 5 "" Pendente Media Outro Nothing [],
    Tarefa 6 "Revisar documento" Pendente Alta Trabalho (Just (fromGregorian 2024 06 10)) ["relatório", "urgente", "cliente"],
    Tarefa 7 "Enviar proposta" Concluída Media Trabalho (Just (fromGregorian 2024 03 10)) ["cliente"],
    Tarefa 8 "Preparar apresentação" Pendente Alta Estudos (Just (fromGregorian 2024 05 05)) ["prova", "slides"],
    Tarefa 9 "Ir à academia" Pendente Baixa Pessoal Nothing [],
    Tarefa 10 "Organizar armário" Concluída Baixa Outro Nothing ["organização"]
    ]

