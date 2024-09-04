sangre(harry,mestiza).
sangre(draco,pura).
sangre(hermione,impura).
caracter(harry,[corajudo,amistoso,orgulloso]).
caracter(draco,[inteligente,orgulloso]).
caracter(hermione,[inteligente,orgulloso,responsable]).
noLeGustaria(harry,slytherin).
noLeGustaria(draco,hufflepuff).
requisitoCasa(gryffindor,coraje).
requisitoCasa(slytherin,orgulloso).
requisitoCasa(slytherin,inteligente).
requisitoCasa(ravenclaw,inteligente).
requisitoCasa(ravenclaw,responsable).
requisitoCasa(hufflepuff,amistoso).

%1
permiteEntrar(Mago,Casa):-
    sangre(Mago,_),
    requisitoCasa(Casa,_),
    Casa\=slytherin.
permiteEntrar(Mago1,slytherin):-
    sangre(Mago1,pura).

%2
esAptoCasa(Magoo,Casaa):-
    caracter(Magoo,Cualidades),
    forall((requisitoCasa(Casaa,Requisito)),member(Requisito, Cualidades)).
%3
podriaQuedar(Mago2,Casa2):-
    permiteEntrar(Mago2,Casa2),
    esAptoCasa(Mago2,Casa2),
    not(noLeGustaria(Mago2,Casa2)).
%4
cadenaDeAmistades(Magos):-
forall(member(Mago,Magos),sonAmistosos(Mago)),
estaEnlasiguiente(Magos).

estaEnlasiguiente([PrimerMago,SegundoMago|OtrosMagos]):-
    podriaQuedar(SegundoMago,CasaSegundo),
    podriaQuedar(PrimerMago,CasaSegundo),
    estaEnlasiguiente([SegundoMago|OtrosMagos]).
sonAmistosos(MagoAmisotoso):-
caracter(MagoAmisotoso,CaractersDelMago),
member(amistoso,CaractersDelMago).

esDe(hermione, gryffindor).

esDe(ron, gryffindor).

esDe(harry, gryffindor).

esDe(draco, slytherin).

esDe(luna, ravenclaw).

%********************PARTE 2************************
malasAcciones(andarDeNoche).
malasAcciones(bosque).
malasAcciones(seccionRestringidaBiblioteca).
malasAcciones(tercerPiso).

lugarProhibido(bosque,50).
lugarProhibido(seccionRestringidaBiblioteca,10).
lugarProhibido(tercerPiso,75).
acciones(harry,[andarDeNoche,bosque,tercerPiso]).
acciones(hermione,[tercerPiso,seccionRestringidaBiblioteca]).
acciones(draco,[mazmorras]).
buenasAcciones(harry,60).
buenasAcciones(ron,50).
buenasAcciones(hermione,50).
%2.1
esBuenAlumno(Alumno):-
acciones(Alumno,Acciones),
buenasAcciones(Alumno,_),
forall((member(Accion,Acciones)),not(malasAcciones(Accion))).
%accionEsRecurrente(AccionRecurrente):-
%*******************MIT2017************************

herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).
%1
tiene(egon,aspiradora(200)).
tiene(egon,trapeador).
tiene(piter,trapeador).
tiene(winston,varitaDeNeutrones).
%2
integranteSatisfase(Integrante,Herrameinta):-
    tiene(Integrante,Herrameinta).
integranteSatisfase(Integrante,aspiradora(Potencia)):-
    tiene(Integrante,aspiradora(Potencia1)),
    between(0, Potencia, Potencia1).
%3

puedeRealizarTarea(Persona,Tarea):-
tiene(Persona,varitaDeNeutrones),
herramientasRequeridas(Tarea,_).

puedeRealizarTarea(Persona,Tarea):-
    herramientasRequeridas(Tarea,Tools),
    tiene(Persona,_),
    forall((member(Tool,Tools)),integranteSatisfase(Persona,Tool)).
%4
tareaPedida(marce,cortarPasto,50).
tareaPedida(marce,limpiarTecho,40).
precio(cortarPasto,100).
precio(limpiarTecho,30).

cobrar(ClientePideTarea,DineroAcobrar):-
    findall(Dinero,montoTarea(ClientePideTarea,Dinero),Monto),
    sum_list(Monto,DineroAcobrar).


montoTarea(Cliente1,Monto1):-
    tareaPedida(Cliente1,Tarea1,Superficie),
    precio(Tarea1,PrecioXSuperficie),
    Monto1 is Superficie*PrecioXSuperficie.
%5

aceptaPedido(Integrante,ClienteConPedido):-
    puedeRealizarTodasTareas(Integrante,ClienteConPedido),
    quiereHacerlasTareas(Integrante,ClienteConPedido).
quiereHacerlasTareas(ray,ClienteConPedido):-
   forall(tareaPedida(ClienteConPedido,Tarea,_),Tarea\=limpiarTecho).
quiereHacerlasTareas(winston,ClienteConPedido):-
    cobrar(ClienteConPedido,DineroAcobrar1),
    DineroAcobrar1>=500.
quiereHacerlasTareas(peter,ClienteConPedido2):-
    tareaPedida(ClienteConPedido2,_,_).
quiereHacerlasTareas(egon,ClienteConPedido3):-
    not((tareaPedida(ClienteConPedido3,Tareas3,_),tareaEsCompleja(Tareas3))).

puedeRealizarTodasTareas(In,Cli):-
    tareaPedida(Cli,_,_),
    tiene(In,_),
    forall(tareaPedida(Cli,Tar,_),puedeRealizarTarea(In,Tar)).
tareaEsCompleja(limpiarTecho).
tareaEsCompleja(TareaCompleja):-
    herramientasRequeridas(TareaCompleja, HerramientasNecesarias),
    length(HerramientasNecesarias, CantidadDeHerramientas),
    CantidadDeHerramientas>=2.
    
%******************************************************************************PULP***************************************
personaje(pumkin,     ladron([licorerias, estacionesDeServicio])).
personaje(honeyBunny, ladron([licorerias, estacionesDeServicio])).
personaje(vincent,    mafioso(maton)).
personaje(jules,      mafioso(maton)).
personaje(marsellus,  mafioso(capo)).
personaje(winston,    mafioso(resuelveProblemas)).
personaje(mia,        actriz([foxForceFive])).
personaje(butch,      boxeador).

pareja(marsellus, mia).
pareja(pumkin,    honeyBunny).

%trabajaPara(Empleador, Empleado)
trabajaPara(marsellus, vincent).
trabajaPara(marsellus, jules).
trabajaPara(marsellus, winston).
%1

esPeligroso(Personaje):-
    personaje(Personaje,Actividad),
    actividadPeligrosa(Actividad).
esPeligroso(Personaje):-
    personaje(Personaje,_),
    tieneEmpleadosPeligrosos(Personaje).
actividadPeligrosa(ladron(Tareas)):-
    member(licorerias,Tareas).
actividadPeligrosa(mafioso(maton)).
tieneEmpleadosPeligrosos(Personaje):-
    trabajaPara(Personaje,Empleado),
    esPeligroso(Empleado).
%2
amigo(vincent, jules).
amigo(jules, jimmie).
amigo(vincent, elVendedor).

duoTemible(Amigo1,Amigo2):-
    personaje(Amigo1,_),
    personaje(Amigo2,_),
    sonPeligrosos(Amigo1,Amigo2),
    sonAmigosOsonPareja(Amigo1,Amigo2).
sonPeligrosos(Amigo1,Amigo2):-
    esPeligroso(Amigo1),
    esPeligroso(Amigo2).
sonAmigosOsonPareja(Amigo1,Amigo2):-
    amigo(Amigo1,Amigo2).
sonAmigosOsonPareja(Amigo1,Amigo2):-
    pareja(Amigo2,Amigo1).
%3
%encargo(Solicitante, Encargado, Tarea). 
%las tareas pueden ser cuidar(Protegido), ayudar(Ayudado), buscar(Buscado, Lugar)
encargo(marsellus, vincent,   cuidar(mia)).
encargo(vincent,  elVendedor, cuidar(mia)).
encargo(marsellus, winston, ayudar(jules)).
encargo(marsellus, winston, ayudar(vincent)).
encargo(marsellus, vincent, buscar(butch, losAngeles)).



estaEnProblemas(Personaje):-
    trabajaPara(Jefe,Personaje),
    esPeligroso(Jefe),
    pareja(Jefe,Pareja),
    encargo(Jefe,Personaje,cuidar(Pareja)).
estaEnProblemas(Personaje):-
    personaje(Personaje,_),
    encargo(_,Personaje,buscar(_,boxeador)).
estaEnProblemas(butch).
%4
sanCayetano(Alguien):-
    personaje(Alguien,_),
    forall(todosLosQueTieneCerca(Alguien,Cercano),lesDaTrabajo(Alguien,Cercano)).
todosLosQueTieneCerca(Alguien,Cercano):-
    %personaje(Alguien,_),
    %personaje(Cercano,_),
    amigo(Alguien,Cercano).
todosLosQueTieneCerca(Alguien,Cercano):-
    %personaje(Cercano,_),
    %personaje(Alguien,_),
    trabajaPara(Cercano,Alguien).
lesDaTrabajo(Alguien,Cercano):-
    encargo(Alguien,Cercano,_).
%5



masAtareado(Atareado):-
    personaje(Otropersonaje,_),
    personaje(Atareado,_),
    forall(personaje(Otropersonaje,_),Otropersonaje\=Atareado),tieneMasEncargosQueCualquiera(Atareado,Otropersonaje).
tieneMasEncargosQueCualquiera(UnChabon,ElotroChabon):-
    findall(TareaDelUnChabon, encargo(_,UnChabon,TareaDelUnChabon), TareasUnChabon),
    findall(TareaDelOtroChabon,encargo(_,ElotroChabon,TareaDelOtroChabon),TareasOtroChabon),
    length(TareasUnChabon, Int),
    length(TareasOtroChabon,Int2),
    Int>Int2.   
    
%6
personajesRespetables(PersonajesRespetables):-
    
    findall(PersonajeMuestra,(respetable(PersonajeMuestra,ValorDeRespeto),ValorDeRespeto>9),PersonajesRespetables).
respetable(Person,Valor):-
    personaje(Person,actriz(Peliculas)),
    length(Peliculas,Cantidad),
    Valor is Cantidad//10.
respetable(Person,10):-
    personaje(Person,mafioso(resuelveProblemas)).
respetable(Person,1):-
    personaje(Person,mafioso(maton)).
respetable(Person,20):-
    personaje(Person,mafioso(capo)).
%7
hartoDe(Personaje,PersonajeInsoportable):-
    encargo(_, Personaje, _),
    personaje(PersonajeInsoportable, _),
    forall((encargo(_,Personaje,Tareas),Personaje\=PersonajeInsoportable),tieneQuevercon(Tareas,PersonajeInsoportable)).

tieneQuevercon(cuidar(Personajeins),Personajeins).
    
tieneQuevercon(buscar(Personajeins,_),Personajeins).
    
tieneQuevercon(ayudar(Personajeins),Personajeins).
    
tieneQuevercon(cuidar(AmigoDAmigo),Amigo):-
    amigo(Amigo,AmigoDAmigo).
tieneQuevercon(buscar(AmigoDAmigo,_),Amigo):-
    amigo(Amigo,AmigoDAmigo).
tieneQuevercon(ayudar(AmigoDAmigo),Amigo):-
    amigo(Amigo,AmigoDAmigo).
%8
caracteristicas(vincent,  [negro, muchoPelo, tieneCabeza]).
caracteristicas(jules,    [tieneCabeza, muchoPelo]).
caracteristicas(marvin,   [negro]).


duoDiferenciable(Integrante1, Integrante2) :-
    sonAmigosOsonPareja(Integrante1, Integrante2),
    caracteristicas(Integrante1, Caracteristicas1),
    caracteristicas(Integrante2, Caracteristicas2),
    member(Caracteristica, Caracteristicas1),
    not(member(Caracteristica, Caracteristicas2)).

%************************Festivales de Rock***************************
anioActual(2015).
%festival(nombre, lugar, bandas, precioBase).
%lugar(nombre, capacidad).
festival(lulapaluza, lugar(hipodromo,40000), [miranda, paulMasCarne, muse], 500).
festival(mostrosDelRock, lugar(granRex, 10000), [kiss, judasPriest, blackSabbath], 1000).
festival(personalFest, lugar(geba, 5000), [tanBionica, miranda, muse, pharrellWilliams], 300).
festival(cosquinRock, lugar(aerodromo, 2500), [erucaSativa, laRenga], 400).

%banda(nombre, año, nacionalidad, popularidad).

banda(paulMasCarne,1960, uk, 70).
banda(muse,1994, uk, 45).
banda(kiss,1973, us, 63).
banda(erucaSativa,2007, ar, 60).
banda(judasPriest,1969, uk, 91).
banda(tanBionica,2012, ar, 71).
banda(miranda,2001, ar, 38).
banda(laRenga,1988, ar, 70).
banda(blackSabbath,1968, uk, 96).
banda(pharrellWilliams,2014, us, 85).
%entradasVendidas(nombreDelFestival, tipoDeEntrada, cantidadVendida).
% tipos de entrada: campo, plateaNumerada(numero de fila), plateaGeneral(zona).
entradasVendidas(lulapaluza,campo, 600).
entradasVendidas(lulapaluza,plateaGeneral(zona1), 200).
entradasVendidas(lulapaluza,plateaGeneral(zona2), 300).
entradasVendidas(mostrosDelRock,campo,20000).
entradasVendidas(mostrosDelRock,plateaNumerada(1),40).
entradasVendidas(mostrosDelRock,plateaNumerada(2),0).

% … y asi para todas las filas

entradasVendidas(mostrosDelRock,plateaNumerada(10),25).
entradasVendidas(mostrosDelRock,plateaGeneral(zona1),300).
entradasVendidas(mostrosDelRock,plateaGeneral(zona2),500).

plusZona(hipodromo, zona1, 55).
plusZona(hipodromo, zona2, 20).
plusZona(granRex, zona1, 45).
plusZona(granRex, zona2, 30).
plusZona(aerodromo, zona1, 25).


%11)  estaDeModa/1. Se cumple para las bandas recientes (que surgieron en
% los últimos 5 años) que tienen una popularidad mayor a 70.
estaDeModa(Banda) :-
    banda(Banda, Anio, _, Popularidad),
    esReciente(Anio),
    esPopular(Popularidad).
esReciente(Anio) :-
    anioActual(AnioActual),
    AnioPiso is AnioActual - 5,
    between(AnioPiso, AnioActual, Anio).
esPopular(Popularidad) :-
    Popularidad > 70.
%2
esCareta(Festival) :-
    festival(Festival, _, Bandas, _),
    bandasDeModa(Bandas).
esCareta(Festival):-
    festival(Festival, _, Bandas, _),
    member(miranda,Bandas).
%esCareta(Festival):-
%    festival(Festival, _, Bandas, _),
 %   not(entradasRazonables(Festival)).

bandasDeModa(Bandas) :-
    member(Banda1, Bandas),
    estaDeModa(Banda1),
    member(Banda2, Bandas),
    estaDeModa(Banda2),
    Banda1 \= Banda2.

%3
%entradasRazonables(Festival,EntradaDeEseFest):-
 %   festival(_,_,_,PrecioBase),
  %  plusZona(Festival,Zona,PrecioPlus),
   % entradasVendidas(Festival,plateaGeneral(Zona),_).
%5
recaudacion(Festival, Recaudacion):-
    festival(Festival, _, _, _),
    findall(PrecioxTipo, precioPorFest(Festival, PrecioxTipo), ListaDeGananciasPorFest),
    sum_list(ListaDeGananciasPorFest, Recaudacion).

precioPorFest(Festival, PrecioxTipo):-
    festival(Festival, lugar(Lugar, _), _, PrecioBase),
    entradasVendidas(Festival, plateaGeneral(Zona), Cantidad),
    plusZona(Lugar, Zona, Plus),
    P is PrecioBase + Plus,
    PrecioxTipo is Cantidad * P.

precioPorFest(Festival, PrecioxTipo):-
    festival(Festival, _, _, PrecioBase),
    entradasVendidas(Festival, plateaNumerada(Nfila), Cantidad),
    K is (PrecioBase + 200 )// Nfila,
    PrecioxTipo is Cantidad * K.

precioPorFest(Festival, PrecioxTipo):-
    festival(Festival, _, _, PrecioBase),
    entradasVendidas(Festival, campo, Cantidad),
    PrecioxTipo is Cantidad * PrecioBase.

%***************Vigilantes*************

 %tarea(agente, tarea, ubicacion)
%tareas:
%  ingerir(descripcion, tamaño, cantidad)
%  apresar(malviviente, recompensa)
%  asuntosInternos(agenteInvestigado)
%  vigilar(listaDeNegocios)

tarea(vigilanteDelBarrio, ingerir(pizza, 1.5, 2),laBoca).
tarea(vigilanteDelBarrio, vigilar([pizzeria, heladeria]), barracas).
tarea(canaBoton, asuntosInternos(vigilanteDelBarrio), barracas).
tarea(sargentoGarcia, vigilar([pulperia, haciendaDeLaVega, plaza]),puebloDeLosAngeles).
tarea(sargentoGarcia, ingerir(vino, 0.5, 5),puebloDeLosAngeles).
tarea(sargentoGarcia, apresar(elzorro, 100), puebloDeLosAngeles). 
tarea(vega, apresar(neneCarrizo,50),avellaneda).
tarea(jefeSupremo, vigilar([congreso,casaRosada,tribunales]),laBoca).


ubicacion(puebloDeLosAngeles).
ubicacion(avellaneda).
ubicacion(barracas).
ubicacion(marDelPlata).
ubicacion(laBoca).
ubicacion(uqbar).


%jefe(jefe, subordinado)
jefe(jefeSupremo,vega ).
jefe(vega, vigilanteDelBarrio).
jefe(vega, canaBoton).
jefe(jefeSupremo,sargentoGarcia).
   
%1
frecuenta(Agente,Ubicacion):-
    tarea(Agente,_,Ubicacion).
frecuenta(Agente,buenosAires):-
    tarea(Agente,_,_).
frecuenta(vega,quilmes).
frecuenta(Agente,marDelPlata):-
    tarea(Agente,vigilar(Lugares),_),
    member(negocioDeAlfajores,Lugares).
%2
lugarInaccesible(Lugar):-
    ubicacion(Lugar),
    forall(tarea(Agente,_,Lugar),esInaccesible(Agente,Lugar)).
esInaccesible(Agente,Lugar):-
    not(frecuenta(Agente,Lugar)).
%3
afincado(Agente):-
    tarea(Agente,_,Ubicacion1),
    forall((tarea(Agente,_,Ubicacion),Ubicacion\=Ubicacion1),Ubicacion=Ubicacion1).

%4
cadenaDeMando([_]).
cadenaDeMando([PrimerAgente,SegundoAgente|Agentes]):-

    jefe(PrimerAgente,SegundoAgente),
    cadenaDeMando([SegundoAgente|Agentes]).

%5
agentePremio(Agente):-
    puntosDeUnAgente(Agente,PuntosAgente),
    forall((puntosDeUnAgente(OtroAgente,PuntosOtroAgente),OtroAgente \= Agente), PuntosAgente>PuntosOtroAgente).
puntosDeUnAgente(Agente,Puntos):-
    tarea(Agente,vigilar(Lugares),_),
    length(Lugares,CantidadDeNegoscios),
    Puntos is 5*CantidadDeNegoscios.
puntosDeUnAgente(Agente,Puntos):-
    tarea(Agente,ingerir(_,Tam,Cant),_),
    Unidades is  Tam*Cant,
    Puntos is -10*Unidades.
puntosDeUnAgente(Agente,Puntos):-
    tarea(Agente,apresar(_,Recompensa),_),
    Puntos is Recompensa//2.
puntosDeUnAgente(Agente,Puntos):-
    tarea(Agente,asuntosInternos(Vigilado),_),
    puntosDeUnAgente(Vigilado,PuntosVigilado),
    Puntos is 2*PuntosVigilado.
%6
%El polimorfismo nos sirve para no cambiar el predicado principal del predicado 
%al cual estamos usando el concepto, es decir  se puede agregar varias formas
%de tareas o tipos de tareas y todas sus variantes sin necesidad de cambiar el 
%predicado "tareas".Solo alcanza con agregar las nuevas definiciones para el nuevo requerimiento.
% solo agregarias nueva deficion de tareas pero los predicados que usan tareas 
%NO se modifican.
%INVERSIBILIDAD:nos sirve para que un predicado pueda no solo dar respuestas 
%de exisitencia que dan true o false sino nos permite saber los casos para los
%cuales es verdadero o falso.

    


    



    
    
   



