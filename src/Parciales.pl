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
forall((member(Accio_:-n,Acciones)),not(malasAcciones(Accion))).
accionEsRecurrente(AccionRecurrente):-
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
    herramientasRequeridas(_,Tools),
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
    
%6

    



