unit Language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  Message : array[0..11,0..4] of string ;
  i : integer ;
implementation

Initialization
i := 0 ;
Message[i,0] := 'Jouer' ;        // 0
Message[i,1] := 'Play' ;
Message[i,2] := 'Giocare' ;
Message[i,3] := 'Spielen';
Message[i,4] := 'Jugar';
inc(i);

Message[i,0] := 'Quitter' ;          // 1
Message[i,1] := 'Exit' ;
Message[i,2] := 'Uscire' ;
Message[i,3] := 'Verlassen';
Message[i,4] := 'Irse';
inc(i);

Message[i,0] := 'Instructions' ;    // 2
Message[i,1] := 'Instructions' ;
Message[i,2] := 'Instruczioni' ;
Message[i,3] := 'Anweisungen';
Message[i,4] := 'Instrucciones';
inc(i);

Message[i,0] := 'Bonus de vitesse activé' ;   // 3
Message[i,1] := 'Speed bonus activated' ;
Message[i,2] := 'Velocità bonus attivato' ;
Message[i,3] := 'Speed bonus aktiviert';
Message[i,4] := 'Bono de velocidad activa';
inc(i);

Message[i,0] := 'Sauvetage' ;     // 4
Message[i,1] := 'Rescue' ;
Message[i,2] := 'Salvataggio' ;
Message[i,3] := 'Rettung';
Message[i,4] := 'Rescate';
inc (i);

Message[i,0] := 'GAUCHE' ;        // 5
Message[i,1] := 'LEFT' ;
Message[i,2] := 'SINISTRA' ;
Message[i,3] := 'LINKS';
Message[i,4] := 'IZQUIERDA';
inc (i);

Message[i,0] := 'DROITE' ;        // 6
Message[i,1] := 'RIGHT' ;
Message[i,2] := 'DESTRA' ;
Message[i,3] := 'RECHTS';
Message[i,4] := 'DERECHA';
inc (i);

Message[i,0] := 'ACCELERER';        // 7
Message[i,1] := 'ACCELERATE';
Message[i,2] := 'ACCELERARE';
Message[i,3] := 'BESCHLEUNIGEN';
Message[i,4] := 'ACELERAR';
inc (i);

Message[i,0] := 'FREIN';        // 8
Message[i,1] := 'BRAKE';
Message[i,2] := 'FRENO';
Message[i,3] := 'BREMSE';
Message[i,4] := 'FRENO';
inc (i);

Message[i,0] := 'SAUTER';        // 9
Message[i,1] := 'JUMP';
Message[i,2] := 'SALTARE';
Message[i,3] := 'SPRINGEN';
Message[i,4] := 'SALTAR';
inc (i);

Message[i,0] := 'QUITTER';        // 10
Message[i,1] := 'QUIT';
Message[i,2] := 'LASCIARE';
Message[i,3] := 'VERLASSEN';
Message[i,4] := 'ABANDONAR';
inc (i);

Message[i,0] := 'Langue';        // 11
Message[i,1] := 'Language';
Message[i,2] := 'Lingua';
Message[i,3] := 'Sprache';
Message[i,4] := 'Idioma';
inc (i);

end.

