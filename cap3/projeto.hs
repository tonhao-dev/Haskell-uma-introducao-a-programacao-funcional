module Projeto where

data Cargo = Estagiario | Programador | Coordenador | Gerente deriving Show
data Pessoa = Funcionario {cargo :: Cargo, nome :: String} deriving Show

verSalario :: Pessoa -> Double
verSalario (Funcionario Estagiario _) = 1500
verSalario (Funcionario Programador _) = 5750.15
verSalario (Funcionario Coordenador _) = 8000
verSalario (Funcionario Gerente _) = 10807.20

verFolha :: Pessoa -> String
verFolha funcionario = "{nome: \"" ++ (nome funcionario) ++
    "\", cargo: \"" ++ show (cargo funcionario) ++
    "\", salario: " ++ show (verSalario funcionario) ++ "}"

    
