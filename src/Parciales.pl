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



    
    
   



