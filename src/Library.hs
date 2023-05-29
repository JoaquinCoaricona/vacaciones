module Library where
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero

--Datas del ejemplo
beto :: Turista
beto = UnTurista { nivelDeCansancio = 15, nivelDeEstres = 15, viajaSolo = True, idiomasQueHabla = ["aleman"]}

ana :: Turista
ana = UnTurista {viajaSolo = False,nivelDeCansancio = 0,nivelDeEstres=21,idiomasQueHabla = ["español"] }

cathi ::Turista
cathi = UnTurista{nivelDeCansancio = 15, nivelDeEstres = 15,viajaSolo=False,idiomasQueHabla = ["aleman","catalan"]}



type Idioma = String
data Turista = UnTurista{
    
    nivelDeEstres::Number,
    nivelDeCansancio::Number,
    viajaSolo::Bool,
    idiomasQueHabla::[Idioma]

}deriving Show 

type Excursion = (Turista->Turista)

reducirEstresPorcentual::Turista->Turista
reducirEstresPorcentual turista = turista{nivelDeEstres=round((nivelDeEstres turista)*0.9)}


cambiarCansancio::Number->Turista->Turista
cambiarCansancio valor turista = turista{nivelDeCansancio=nivelDeCansancio turista +valor}

cambiarEstres::Number->Turista->Turista
cambiarEstres valor turista= turista{nivelDeEstres=nivelDeEstres turista+valor}

playa::Excursion
playa persona |viajaSolo(persona)=cambiarCansancio (-5)persona
              |otherwise = cambiarEstres (-1) persona


nombrePaisaje::String->Number
nombrePaisaje lugar = length(lugar)

--apreciarPaisaje::Excursion 
--apreciarPaisaje turista = cambiarEstres turista   
            
apreciar :: String -> Excursion
apreciar algo = cambiarEstres (-length algo)


idiomaEnelVector::String->String->Bool
idiomaEnelVector idioma1 idioma2 |idioma1==idioma2 = True
                                 |otherwise = False

yaSabeIdioma::String->Turista->Bool
yaSabeIdioma idioma turista = any(idiomaEnelVector idioma) (idiomasQueHabla turista)


acompaniado::Turista->Turista
acompaniado turista = turista{viajaSolo=False} 

agregarIdioma::String->Turista->Turista
agregarIdioma idioma turista = turista{idiomasQueHabla= idioma:(idiomasQueHabla turista)    }




--Estos dos son diferentes porque en el 2 no hay casos, directamente es el resultado
--pero en el primero hay casos y entonces no puedo pasar turista por consola tengo que pasarlo
--en la funcion para que se aplique en la composicion, en el primero es un caso especial
--en el segundo en un caso directo y por eso lo puedo aplicar parcialmente

--porque si cumple la conidcion del primer if entonces ahi no le puedo pasar especificamente
-- el parametros que falta por eso lo pongo directamente ahi, abajo lo puedo pasar al inicio
--porque no hay ninguna condicion que cumplir

salirConGente :: Idioma -> Excursion
salirConGente idioma turista
  | yaSabeIdioma idioma turista = (acompaniado . agregarIdioma idioma) turista
  | otherwise = acompaniado turista


--Estoy en realidad esta devolviendo una funcion, pero a la funcion que devuelve es como que le aplico
--el otro parametro que le paso el de el turista que es el parametro que falta, es el que le falta a 
--agregar idioma entonces si bien falta uno, al llamarla se lo paso y devuelve el el resultado de la funcion
--excursion que es la funcion que en si deberia devolver, directamente devuelve el resultado de esa funcion
--porque le pase el parametro a esa funcion y ahora ya devuelve el rsultado de la funcon directamente


salirConGente2 :: Idioma -> Excursion
salirConGente2 idioma  = acompaniado . agregarIdioma idioma


-- cambiarCansancio::Number->Turista->Turista
-- cambiarCansancio valor turista= turista{nivelDeCansancio=nivelDeCansancio turista +valor}

-- cambiarEstres::Number->Turista->Turista
-- cambiarEstres valor turista= turista{nivelDeEstres=nivelDeEstres turista+valor}

-- type Excursion = (Turista->Turista)
type Nivel = String 

fuerte::Nivel
fuerte="fuerte"

moderada::Nivel
moderada="moderada"

tranquila::Nivel
tranquila="tranquila"

caminar::Number->Excursion
caminar minutos turista =   cambiarCansancio  (div minutos 4) (cambiarEstres (div minutos 4) turista) 

caminar2::Number->Excursion
caminar2 minutos = cambiarCansancio (div minutos 4).cambiarEstres (div minutos 4) 


--aca en moderada esta devolviendo la funcion id  y como la funcion id seria la funcion excursion que es lo que
--se devuelve entonces id seria Excursion y entonces Excursion recibe el parametro que seria turista
--ejemplo: beto y entonces si devuelvo beto lo deuvelvo sin modificar nada por eso se devuelve eso
paseoEnBarco::Nivel->Excursion
paseoEnBarco nivel |nivel == fuerte = cambiarCansancio 10 .cambiarEstres 6 
                   |nivel == moderada = id
                   |nivel == tranquila = caminar2 10.apreciar "mar".salirConGente2 "aleman" 

hacerExcursion :: Excursion -> Turista ->Turista
hacerExcursion excursion = reducirEstresPorcentual.excursion 

--SE USAN ASI LA FUNCION DE ARRIBA: las excursiones que devuelven excursion les paso el primer parametro
--para que devuelven excursion que es lo que necesita que entre por esta funcion
-- *Spec> hacerExcursion (paseoEnBarco fuerte) beto
-- UnTurista {nivelDeEstres = 19, nivelDeCansancio = 25, viajaSolo = True, idiomasQueHabla = ["aleman"]}
-- *Spec> hacerExcursion (caminar2 10) beto        
-- UnTurista {nivelDeEstres = 15, nivelDeCansancio = 17, viajaSolo = True, idiomasQueHabla = ["aleman"]}
-- *Spec>

estres::Turista->Number
estres turista = nivelDeEstres turista 

cansancion::Turista->Number
cansancion turista = nivelDeCansancio turista 

idiomasTurista::Turista->Number
idiomasTurista turista = length (idiomasQueHabla turista)

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2


deltaExcursionSegun :: (Turista -> Number) ->Turista ->Excursion-> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista 


--paso la excursion como parcialmente aplicada
esEducativa::Turista->Excursion->Bool
esEducativa turista  = (>0).deltaExcursionSegun idiomasTurista turista   


excursionesDesestresantes::Turista->[Excursion]->[Excursion]
excursionesDesestresantes turista = filter(esDesestresante turista)

esDesestresante::Turista->Excursion->Bool
esDesestresante turista = (<=(-3)).deltaExcursionSegun estres turista

--Aca uso el flip para poder invertir el orden en que piden los parametros
--Ademas de esta forma dejo el parametro que voy a pasar como parcialmente aplicado
--lo dejo sin poner y seria como el que falta el ultimo que falta uso el flip para que lo pida al finlal
dejaAcompaniado::Turista->Excursion->Bool
dejaAcompaniado turista  = viajaSolo.((flip hacerExcursion) turista)

type Tour = [Excursion]

completo::Tour
completo =  [caminar2 20,apreciar"Cascada",caminar2 40,playa,salirConGente2 "melmacquiano"]

ladob:: Excursion -> Tour
ladob excursion = [paseoEnBarco tranquila,excursion,caminar2 120]

islaVecina::Nivel->Tour
islaVecina nivel |nivel==fuerte =[paseoEnBarco nivel,apreciar "lago",paseoEnBarco nivel]
                 |otherwise = [paseoEnBarco nivel,playa,paseoEnBarco nivel]


--uso foldr por la forma que tiene de recibir los parametros la funcion hacer excursion
hacerTour::Tour->Turista->Turista
hacerTour tour turista = foldr hacerExcursion (cambiarEstres (length tour)turista) tour
 

--3B
--Uso flip para dejar dentro de la funcion del any la funcion con los parametros que va comparar 
--con el vector que se le pasa yo le paso un parametro y asi devuelve una funcion que es del tipo
--que pide el any
cumpleParaSerConvincente ::Excursion->Turista->Bool
cumpleParaSerConvincente excursion turista = esDesestresante turista excursion   &&  dejaAcompaniado turista excursion

tourConvincente :: Tour->Turista->Bool
tourConvincente tour turista = any(flip cumpleParaSerConvincente turista) tour

esConvincente:: Turista->[Tour]->Bool 
esConvincente turista tours = any (flip tourConvincente turista) tours

infplayas :: [Excursion]
infplayas = playa:infplayas 




