-module(btree).
-export([isEmptyBT/1, initBT/0, insertBT/2, findBT/2, inOrderBT/1, equalBT/2, deleteBT/2]).

% Gibt einen Leeren Baum zurueck
% visibilty: public
% return: tupel
initBT() ->
		{}.

% Prueft, ob ein Baum Leer ist
% visibilty: public
% params: BTree
% return: boolean
isEmptyBT(BTree) ->
		BTree == {}.

% Prueft auf semantische Gleichheit
% visibilty: public
% params: BTree, BTree
% return: boolean
equalBT({}, {}) ->
		true;
equalBT(_FirstTree, {}) ->
		false;
equalBT({}, _SecondTree) ->
		false;
equalBT(FirstTree, SecondTree) ->
		inOrderBT(FirstTree) == inOrderBT(SecondTree).

% Private Hilfsmethode
% Gibt die Hoehe eines Baumes zurueck
% visibilty: private
% params: Tupel
% return: Integer Atom
getHeight({}) -> 0;
getHeight({_Atom, Grad, _LinkerTeilBaum, _RechterTeilBaum}) ->
	Grad.

% Fuegt ein Element in einen Baum ein
% params: BTree, Integer
% return: BTree
insertBT({}, Element) ->
		% Jedes neue Blatt hat erstmal die Hoehe 1
		{Element, 1, {}, {}};
insertBT(BTree, Element) ->
		{Atom, Grad, LinkerTeilBaum, RechterTeilBaum} = BTree,
			 % alle Zahlen im linken Teilbaum sind kleiner seine Zahl
		if Element < Atom ->
			NeueLinkerTeil = insertBT(LinkerTeilBaum, Element),
			% Ist die Hoehe des Linken Teils kleiner gleich der Hoehe des Rechten Baumes
			% oder die Differenz aus der AKtuellen Hoehe und der Hoehe des Linken und Rechten Teils groesser 0
			% Dann muss zur Hohe nichts addiert werden,da sich dieses Element keine Neue Ebene darstellt
			case getHeight(NeueLinkerTeil) =< getHeight(RechterTeilBaum) orelse (Grad-getHeight(NeueLinkerTeil) > 0) of
			true ->
				{Atom, Grad, NeueLinkerTeil, RechterTeilBaum};
			false ->
				{Atom, Grad+1,NeueLinkerTeil, RechterTeilBaum}
			end;

			% Alle Zahlen im rechten Teilbaum eines Knotens sind größer seine Zahl
			Element > Atom ->
			NeuerRechterTeil = insertBT(RechterTeilBaum, Element),
			% Ist die Hoehe des Linken Teils groesser gleich der Hoehe des Rechten Baumes
			% oder die Differenz aus der AKtuellen Hoehe und der Hoehe des Linken und Rechten Teils groesser 0
			% Dann muss zur Hohe nichts addiert werden,da sich dieses Element keine Neue Ebene darstellt
			case getHeight(LinkerTeilBaum) >= getHeight(NeuerRechterTeil) orelse (Grad-getHeight(NeuerRechterTeil) > 0) of
			true  ->
				{Atom, Grad, LinkerTeilBaum, NeuerRechterTeil};
			false ->
				{Atom, Grad+1, LinkerTeilBaum, NeuerRechterTeil}
			end;

		% "Else" Konstrukt
		% Falls das Element weder Kleiner noch Groesser ist, ist es bereits vorhanden und muss nicht hinzugefuegt werden
		true ->
			BTree
		end.

% Sucht ein Element in einem Baum und gibt die Hoehe zurueck
% params: BTree, Atom
% return: Integer
findBT({}, _Element) -> 0;
findBT(BTree, Element) ->
		{Atom, Grad, LinkerTeilBaum, RechterTeilBaum} = BTree,
		if Atom == Element ->
			Grad;
		Atom > Element ->
			findBT(LinkerTeilBaum, Element);
		Atom < Element ->
			findBT(RechterTeilBaum, Element)
		end.

% Gibt einen Baum in sortierter Reiehnfolge als Liste aus
% params: BTree
% return: List
inOrderBT({Atom, _Grad, {}, {}}) ->
  		[Atom];

inOrderBT({Atom, _Grad, {}, RechterTeilBaum}) ->
		[Atom] ++ inOrderBT(RechterTeilBaum);

inOrderBT({Atom, _Grad, LinkerTeilBaum, {}})->
		inOrderBT(LinkerTeilBaum) ++ [Atom];

inOrderBT({Atom, _Grad, LinkerTeilBaum, RechterTeilBaum}) ->
		inOrderBT(LinkerTeilBaum) ++ [Atom] ++ inOrderBT(RechterTeilBaum).

% Private Hilfsmethode um den niedringsten Wert herauszufinden.
% da der Linke Teilbaum immer kleiner Seiner Zahl ist, muss nur die Linke Seite durchsucht werden
% Diese Methode wird beim Loeschen benoetigt
% visibilty: Private
% params: btree
% return: Integer
findTheSmalest({}) ->
		'this is empty';
findTheSmalest(BTree) ->
		{Atom, _Grad, LinkerTeilBaum, _RechterTeilBaum} = BTree,
		if LinkerTeilBaum == {} ->
			Atom;
		true ->
			findTheSmalest(LinkerTeilBaum)
		end.

% Loescht ein Atom aus dem Baum
deleteBT({}, _Element) ->
		{};
deleteBT({Atom, Grad, LinkerTeilBaum, RechterTeilBaum}, Element) ->
		if Element == Atom ->
			% Falls im Baum gefunden wurde, sind folgende Faelle zu beruecksichtigen
			% sind der Linke und der Rechte Teil Leer, kann das Atom geloescht werden
			if LinkerTeilBaum == {}, RechterTeilBaum == {} ->
					{};
				% ist der Linke Teilbaum leer, bleibt der Rechte Uebrig
			  LinkerTeilBaum == {} ->
					RechterTeilBaum;
				% ist der Rechte Teilbaum leer, bleibt der Link uebrig
			  RechterTeilBaum == {} ->
					LinkerTeilBaum;
			% Ansonsten nimmt das Niedrigste Element des rechten Teilbaums den Platz ein
			true ->
				NeuesAtom = findTheSmalest(RechterTeilBaum),
				NeuerRechterTeil = deleteBT(RechterTeilBaum, NeuesAtom),
				case Grad-getHeight(NeuerRechterTeil) == 1 orelse Grad-getHeight(LinkerTeilBaum) == 1 of
				true ->
					{NeuesAtom, Grad, LinkerTeilBaum, NeuerRechterTeil};
				false ->
					{NeuesAtom, Grad-1, LinkerTeilBaum, NeuerRechterTeil}
				end
			end;
		% Ist das Element noch nicht gefunden, wird in den Teilbaeumen gesucht
		% Ist das Element kleiner dem Atom, muss es im linken Teilbaum sein
			Element < Atom ->
				NeueLinkerTeil = deleteBT(LinkerTeilBaum, Element),
				case Grad-getHeight(NeueLinkerTeil) == 1 orelse Grad-getHeight(RechterTeilBaum) == 1 of
				true ->
					{Atom, Grad, NeueLinkerTeil, RechterTeilBaum};
				false ->
					{Atom, Grad-1, NeueLinkerTeil, RechterTeilBaum}
				end;
			% Ist das Element groesser dem Atom, muss es im rechten Teilbaum sein
			Element > Atom ->
				NeuerRechterTeil = deleteBT(RechterTeilBaum, Element),
				case Grad-getHeight(NeuerRechterTeil) == 1 orelse Grad-getHeight(LinkerTeilBaum) == 1 of
				true ->
					{Atom, Grad, LinkerTeilBaum, NeuerRechterTeil};
				false ->
					{Atom, Grad-1, LinkerTeilBaum, NeuerRechterTeil}
				end
		end.
