-- Dia: sum type
-- Segunda, terça, quarta...: values constructors
import Data.Text.Internal.Unsafe.Char (ord)
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo

agenda :: Dia -> String
agenda Domingo = "TV..."
agenda Sabado = "Festa"
agenda _ = "Trabalho"


data Pessoa = Fisica String Int | Juridica String

verPessoa :: Pessoa -> (String, String)
verPessoa (Fisica nome idade) = ("Nome: " ++ nome, "Idade:" ++ show idade)
verPessoa (Juridica razao_social) = ("Juridica: " ++ razao_social, "Não há idade")


newtype UUID = UUID String
uuidVersion :: UUID -> Int
uuidVersion (UUID x) = ord (head x)
