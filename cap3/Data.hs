-- Dia: sum type
-- Segunda, terça, quarta...: values constructors
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo

agenda :: Dia -> String
agenda Domingo = "TV..."
agenda Sabado = "Festa"
agenda _ = "Trabalho"