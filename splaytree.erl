-module(splaytree).
-export([isEmptyBT/1, initBT/0, findBT/2, inOrderBT/1, equalBT/2, isBT/1, findTP/2, insertBT/2,printBT/2, eqBT/2, deleteBT/2]).
%http://users.informatik.haw-hamburg.de/~klauck/AlguDat/aufg3.html

% Gibt einen Leeren Baum zurueck
% visibilty: public
% return: tupel
initBT() ->
		{}.

%	Diese Funktion prüft ob der Übergebene Baum ein Binärbaum mit AVL Eigenschaft ist. Außerdem werden die Datentypen geprüft.
%	visibilty: public
% return: boolean
%
isBT(BTree) ->
% 1. Bekomme ein Baum übergeben. Setze das Maximum als Grenze zu einer sehr großen Zahl und das Minimum auf 0.
	isBT(BTree,0,134217728).
isBT({},_Min,_Max) -> true;
% 2. Prüfe, ob der übergebene Baum der Struktur {<Element>,<Höhe>,<linker Teilbaum>,<rechterTeilbaum>} entspricht oder ein leeres Tupel ist
% 4. Prüfe, ob das Element und die Höhe Integer Atome, und die Teilbäume Tupel sind
% 5. Prüfe, ob das Element kleiner dem Maximum und größer dem Minimum ist.
isBT({Atom, Height, {}, {}}, Min, Max)
	when Atom > Min andalso Height == 1 andalso Atom < Max ->
		is_integer(Atom) andalso is_integer(Height);

isBT({Atom, Height, {LAtom, LHeight, LLeftTree, LRightTree}, {}}, Min, Max)
% 6. Prüfe, ob die Kinder der Binärbaum Eigenschaft entsprechen (Linke Kinder kleiner, Rechtegrößer dem Element).
% 7. Prüfe, ob die Höhe korrekt gebildet wurde
	when Atom > Min andalso Atom < Max andalso Atom > LAtom andalso Height - LHeight == 1 ->
		case is_integer(Atom) andalso is_integer(Height) of
			true -> isBT({LAtom, LHeight, LLeftTree, LRightTree},Min,Atom);
			false -> false
		end;
isBT({Atom, Height, {}, {RAtom, RHeight, RLeftTree, RRightTree}}, Min, Max)
	when Atom > Min andalso Atom < Max andalso Atom < RAtom andalso Height - RHeight == 1->
		case is_integer(Atom) andalso is_integer(Height) of
			true -> isBT({RAtom, RHeight, RLeftTree, RRightTree},Atom,Max);
			false -> false
		end;
isBT({Atom, Height, {LAtom, LHeight, LLeftTree, LRightTree}, {RAtom, RHeight, RLeftTree, RRightTree}}, Min, Max)
	when Atom > Min andalso Atom < Max
	andalso (Atom > LAtom andalso Atom < RAtom) andalso (Height - LHeight == 1 orelse Height - RHeight == 1) andalso (Height > LHeight) andalso (Height > RHeight) ->
		case is_integer(Atom) andalso is_integer(Height) of
			% 8. Prüfe, ob die Teilbäume links und rechts ebenfalls Splay Bäume sind. Für den linken Teil, nehme
			%  das Maximum Atom als neues Maximum, für den Rechten, setze als neues Minimum das Atom.
			true -> isBT({LAtom, LHeight, LLeftTree, LRightTree},Min,Atom) andalso isBT({RAtom, RHeight, RLeftTree, RRightTree},Atom,Max);
			false -> false
		end;
isBT(_Tree, _Min, _Max) -> false.



% Diese Funktion fügt einem ABL Baum ein Blatt hinzu und gibt den Veränderten/Erweiterten AVL Baum zurück
% visibilty: public
% return: btree
insertBT(Tree, Element) ->
	{NewTree, Marker} = insertWithSplay(Tree, Element),
	case Marker of
		none -> NewTree;
		%zig
		left -> rotateRight(NewTree);
		%zag
		right -> rotateLeft(NewTree)
	end.

% 2. InsertWithSplay:
insertWithSplay({}, Element) ->
	% 2c. Wenn der Baum leer ist, setze hier das Element mit der Höhe 1. Gib diesen Leaf und den Marker «none» zurück.
	{{Element, 1, {}, {}}, none};
insertWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Element < Atom ->
	% 2b. Wenn das Element kleiner als das Atom ist, muss insertWithSplay mit dem Linken Baum durchgeführt werden.
	{{LeftAtom, LeftHeight, LeftLTree, LeftRTree}, Marker} = insertWithSplay(LeftTree, Element),
	% Wenn die Höhe des neuen Linken Baumes gleich der Höhe des Atoms ist, soll die Höhe des Baumes um 1 erhöht werden.
	if Height == LeftHeight ->
		NewHeight = Height+1;
		true ->
		NewHeight = Height
	end,
	% Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen linken Teil) und den Marker «left» zurückliefern.
	% Wenn der Marker «left» ist, wird die zig-zig Operation auf dem Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.
	% Wenn der Marker «right» ist, wird die zig-zag Operation auf dem Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.
	case Marker of
		none -> {{Atom, NewHeight, {LeftAtom, LeftHeight, LeftLTree, LeftRTree}, RightTree}, left};
		%zig-zig
		left -> {rotateRight(rotateRight({Atom, NewHeight, {LeftAtom, LeftHeight, LeftLTree, LeftRTree}, RightTree})), none};
		%zig-zag
		right -> {rotateRight({Atom, NewHeight, rotateLeft({LeftAtom, LeftHeight, LeftLTree, LeftRTree}), RightTree}), none}
	end;
insertWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Element > Atom ->
	% 2a. Wenn das Element größer als das Atom ist, muss insertWithSplay mit dem Rechten Baum durchgeführt werden.
	% Wenn die Höhe des neuen Rechten Baumes gleich der Höhe des Atoms ist, soll die Höhe des Baumes um 1 erhöht werden.
	{{RightAtom, RightHeight, RightLTree, RightRTree}, Marker} = insertWithSplay(RightTree, Element),
	if Height == RightHeight ->
		NewHeight = Height+1;
		true ->
		NewHeight = Height
	end,
	% Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen rechten Teil) und den Marker «right» zurückliefern.
	% Wenn der Marker «right» ist, wird die zag-zag Operation auf den Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.
	% Wenn der Marker «left» ist, wird die zag-zig Operation auf den Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.
	case Marker of
		none -> {{Atom, NewHeight, LeftTree, {RightAtom, RightHeight, RightLTree, RightRTree}}, right};
		%zag-zig
		left -> {rotateLeft({Atom, NewHeight, LeftTree, rotateRight({RightAtom, RightHeight, RightLTree, RightRTree})}), none};
		%zag-zag
		right -> {rotateLeft(rotateLeft({Atom, NewHeight, LeftTree, {RightAtom, RightHeight, RightLTree, RightRTree}})), none}
	end.


% Diese Funktion löscht ein Element aus einem Baum. Und zwar ist es wichtig, dass der Baum danach
% immer noch der Datenstrukturdefinition entspricht sowie AVL Eigenschaften aufweist.
% visibilty: public
% return: btree
deleteBT(Tree, Element) ->
	%Der erste Funktionsaufruf ruft deleteWithSplay auf, dessen Rückgabewerte der Baum und der Marker sind. Dieser bestimmt die Rotation.
	% 1a) Wenn der Marker «none» ist, soll nichts gemacht werden und die Höhe und der Baum, den dieses delete geliefert hat, werden zurückgegeben
	% 1b) Wenn der Marker «left» ist, dann ist das gelöschte Element in dem linken Teilbaum, und es muss die zig-Operation durchgeführt werden.
	% 1c) Wenn der Marker «right» zeigt, dann entsprechend zag.

	{NewTree, Marker} = deleteWithSplay(Tree, Element),
	case Marker of
		none -> UpdatedTree = NewTree;
		%zig
		left -> UpdatedTree = rotateRight(NewTree);
		%zag
		right -> UpdatedTree = rotateLeft(NewTree)
	end,
	{_Root, Height, LeftTree, RightTree} = UpdatedTree,
	if LeftTree == {} -> RightTree;
		true ->
			{NewRoot, NewLeftTree} = findAndDeleteTheBiggest(LeftTree),
			case Height - getHeight(NewLeftTree) > 1 andalso Height - getHeight(RightTree) > 1 of
				true -> {NewRoot, Height-1, NewLeftTree, RightTree};
				false -> {NewRoot, Height, NewLeftTree, RightTree}
			end
	end.

% 2. DeleteWithSplay:
deleteWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Atom == Element ->
	{{Atom, Height, LeftTree, RightTree}, none};
% 2b. Wenn das gesuchte Element kleiner als das Atom ist, muss findWithSplay mit dem Linken Baum durchgeführt werden.
% Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen linken Teil) und den Marker «left» zurückliefern.
% Wenn der Marker «left» ist, wird die zig-zig Operation auf dem Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben.
% Wenn der Marker «right» ist, wird die zig-zag Operation auf dem Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.
deleteWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Element < Atom ->
	{NewLeftTree, Marker} = deleteWithSplay(LeftTree, Element),
	case Marker of
		none -> {{Atom, Height, NewLeftTree, RightTree}, left};
		%zig-zig
		left -> {rotateRight(rotateRight({Atom, Height, NewLeftTree, RightTree})), none};
		%zig-zag
		right -> {rotateRight({Atom, Height, rotateLeft(NewLeftTree), RightTree}), none}
	end;
% 2a. Wenn das gesuchte Element größer als das Atom ist, muss deleteWithSplay mit dem RechtenBaum durchgeführt werden.
% Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen rechten Teil) und den Marker «right» zurückliefern.
% Wenn der Marker «right» ist, wird die zag-zag Operation auf den Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.
% Wenn der Marker «left» ist, wird die zag-zig Operation auf den Baum durchgeführt. Der veränderte Baum und der Marker «none» werden zurückgegeben.
deleteWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Element > Atom ->
	{NewRightTree, Marker} = deleteWithSplay(RightTree, Element),
	case Marker of
		none -> {{Atom, Height, LeftTree, NewRightTree}, right};
		%zag-zig
		left -> {rotateLeft({Atom, Height, LeftTree, rotateRight(NewRightTree)}), none};
		%zag-zag
		right -> {rotateLeft(rotateLeft({Atom, Height, LeftTree, NewRightTree})), none}
	end.

findAndDeleteTheBiggest({Atom, _Height, LeftTree, {}}) -> {Atom, LeftTree};
findAndDeleteTheBiggest({Atom, Height, LeftTree, RightTree}) ->
	{Biggest, NewRightTree} = findAndDeleteTheBiggest(RightTree),
	case Height - getHeight(NewRightTree) > 1 andalso Height - getHeight(LeftTree) > 1 of
		true -> {Biggest, {Atom, Height-1, LeftTree, NewRightTree}};
		false -> {Biggest, {Atom, Height, LeftTree, NewRightTree}}
	end.


% Diese Funktion wird intern für die Rechtsrotation eines (Teil-)Baumes verwendet und führt die im Kapitel Balancierung
% – mit einfacher Rotation beschriebene Rechtsrotation durch.
% visibilty: Private
% return: btree
% 1. Sei z die Wurzel des (Teil-)Baumes. Der Teilbaum T4 das rechte Kind und y das linke Kind von z.
% Sei T3 der rechte Teilbaum von y und x das linke Kind von y. x hat die Kinder T1 (links) und T2 (rechts)
rotateRight({ZAtom, _ZHoehe, Y, T4}) ->
	{YAtom, _YHoehe, X, T3} = Y,

	HoeheT3 = getHeight(T3),
	HoeheT4 = getHeight(T4),
	HoeheLinks = getHeight(X),

	% Die Hoehe muss abhaengig der unteren Baeume sein
	% Ist der Linke Tielbaum hoeher als der Rechte
	case HoeheT3 > HoeheT4 of % 4. Korrigiere die Höhe von z mit der maximalen Höhe der beiden Kinder +1
		true ->
			%3. Korrigiere die Höhe vom neuen Wurzelknoten y  mit der maximalen Höhe der beiden Kinder + 1
				HoeheRechts = HoeheT3+1,
				% J2. Verteile die Bäume nun so, dass T3 das linke Kind von z wird und y die neue Wurzel des (Teil-)Baumes mit x als linkes und z als rechtes Kind ist.
				case HoeheLinks > HoeheRechts of
					true  -> {YAtom, HoeheLinks +1, X, {ZAtom, HoeheRechts,T3,T4}};
					false -> {YAtom, HoeheRechts +1, X, {ZAtom, HoeheRechts,T3,T4}}
				end;

		false ->
				HoeheRechts = HoeheT4+1,
				% Je nach Egrgebnis des ersten case, die Hoehe des Atoms pruefen
				case HoeheLinks > HoeheRechts of
					true  -> {YAtom, HoeheLinks +1, X, {ZAtom, HoeheRechts,T3,T4}};
					false -> {YAtom, HoeheRechts +1, X, {ZAtom, HoeheRechts,T3,T4}}
				end
	end.

% Diese Funktion wird intern für die links Rotation eines (Teil-)Baumes verwendet
% und führt die im Kapitel Balancierung – mit einfacher Rotation beschriebene Linksrotation durch.
% visibilty: Private
% return: btree
% 1. Sei z die Wurzel des (Teil-)Baumes. Der Teilbaum T1 das linke
%		und y das rechte Kind von z. [...]
rotateLeft({ZAtom, _ZHoehe, T1, Y}) ->
	% 1. [...] Sei T2 der linke Teilbaum von y
	% 	und x das rechte Kind von y.
	%		x hat die Kinder T3 (links) und T4 (rechts)
	{YAtom, _YHoehe, T2, X} = Y,

	HoeheT1 = getHeight(T1),
	HoeheT2 = getHeight(T2),
	HoeheRechts = getHeight(X),

	case HoeheT1 > HoeheT2 of % 4. Korrigiere die Höhe von z mit der maximalen Höhe der beiden Kinder +1
		true ->
			% 3. Korrigiere die Höhe vom neuen Wurzelknoten y  mit der maximalen Höhe der beiden Kinder + 1
			HoeheLinks = HoeheT1 + 1,
			case HoeheLinks > HoeheRechts of
				% 2. Verteile die Bäume nun so, dass T2 das rechte Kind von z wird
				%		und y die neue Wurzel des (Teil-)Baumes mit x als rechtes
				%		und z als linkes Kind ist. Die Teilbäume von x bleiben in der Höhe unverändert
				true  -> {YAtom, HoeheLinks + 1, {ZAtom, HoeheLinks, T1, T2}, X};
				false -> {YAtom, HoeheRechts + 1, {ZAtom, HoeheLinks, T1, T2}, X}
			end;

		false ->
			% 3. Korrigiere die Höhe vom neuen Wurzelknoten y  mit der maximalen Höhe der beiden Kinder + 1
			HoeheLinks = HoeheT2 + 1,
			case HoeheLinks > HoeheRechts of
				% 2. Verteile die Bäume nun so, dass T2 das rechte Kind von z wird
				%		und y die neue Wurzel des (Teil-)Baumes mit x als rechtes
				%		und z als linkes Kind ist. Die Teilbäume von x bleiben in der Höhe unverändert
				true  ->	{YAtom, HoeheLinks + 1, {ZAtom, HoeheLinks, T1, T2}, X};
				false ->	{YAtom, HoeheRechts + 1, {ZAtom, HoeheLinks, T1, T2}, X}
			end
	end.



% Diese Funktion bekommt ein Baum in dem das übergebene Element gesucht ist und gibt die Höhe
% des Elementes innerhalb des Baumes zurück. Das gesuchte Element soll jetzt zur Wurzel gebracht
% werden. Da der Baum sich verändern wird, übergibt jeder Teil der Methode auch den (Teil-)Baum
% zurück.
%
% params: BTree, Atom
% return: Intege

findTP(Tree, Element) ->
	{Height, NewTree, _Marker} = findParentWithSplay(Tree, Element),
	{Height, NewTree}.
% 2. Ist das Element gleich dem Atom des aktuellen Baumknotens, gebe die Höhe des aktuellen Baumknotens und den unveränderten Baum zurück.
findParentWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Atom == Element ->
	{Height, {Atom, Height, LeftTree, RightTree}, found};

findParentWithSplay({Atom, Height, {}, RightTree}, Element) when Element < Atom ->
		{0, {Atom, Height, {}, RightTree}, found};
findParentWithSplay({Atom, Height, LeftTree, {}}, Element) when Element > Atom ->
		{0, {Atom, Height, LeftTree, {}}, found};
	% 3. Ist das Element kleiner dem Atom des aktuellen Baumknotens, rufe diese Funktion mit dem Linken Teilbaum auf. Wir erthalten einen neuen Linken Teilbaum und die gesuchte Höhe.
findParentWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Element < Atom ->
	{Result, NewLeftTree, Marker} = findParentWithSplay(LeftTree, Element),
	case Marker of
		found -> {Result, rotateRight({Atom, Height, NewLeftTree, RightTree}), stop};
		stop -> {Result, {Atom, Height, NewLeftTree, RightTree}, stop}
	end;
% 5. Ist das Element größer dem Atom des aktuellen Baumknotens, rufe diese Funktion mit dem rechten Teilbaum.
findParentWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Element > Atom ->
	{Result, NewRightTree, Marker} = findParentWithSplay(RightTree, Element),
	case Marker of
		found -> {Result, rotateLeft({Atom, Height, LeftTree, NewRightTree}), stop};
		stop -> {Result, {Atom, Height, LeftTree, NewRightTree}, stop}
	end.



findBT(Tree, Element) ->
% 1. Der erste Funktionsaufruf ruft findWithSplay auf, dessen Rückgabewerte der Baum, die Höhe und der Marker sind. Dieser bestimmt die Rotation.
	{Height, NewTree, Marker} = findWithSplay(Tree, Element),
%	1a) Wenn der Marker «none» ist, soll nichts gemacht werden und die Höhe und der Baum, den find geliefert hat wird einfach zurückgeben.
% 1b) Wenn der Marker «left» ist, dann ist das hinzugefügte Element in dem linken Teilbaum, und es muss die zig-Operation durchgeführt werden.
% 1c) Wenn der Marker «right» zeigt, dann entsprechend zag.
	case Marker of
		none -> {Height, NewTree};
		%zig
		left -> {Height, rotateRight(NewTree)};
		%zag
		right -> {Height, rotateLeft(NewTree)}
	end.
% 2. FindWithSplay:
% 2c. Wenn das gesuchte Element gleich dem Atom ist, gib den Baum ohne Änderung, die Höhe des Atoms und den Marker «none» zurück.
findWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Atom == Element ->
	{Height, {Atom, Height, LeftTree, RightTree}, none};
findWithSplay({Atom, Height, {}, RightTree}, Element) when Element < Atom ->
		{0, {Atom, Height, {}, RightTree}, none};
findWithSplay({Atom, Height, LeftTree, {}}, Element) when Element > Atom ->
		{0, {Atom, Height, LeftTree, {}}, none};
% 2b. Wenn das gesuchte Element kleiner als das Atom ist, muss findWithSplay mit dem Linken Baum durchgeführt werden.
findWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Element < Atom ->
	{Result, NewLeftTree, Marker} = findWithSplay(LeftTree, Element),
	% Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen linken Teil), die Höhe und den Marker «left» zurückliefern.
	% Wenn der Marker «left» ist, wird die zig-zig Operation auf dem Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben.
	% Wenn der Marker «right» ist, wird die zig-zag Operation auf dem Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben.
	case Marker of
		none -> {Result, {Atom, Height, NewLeftTree, RightTree}, left};
		%zig-zig
		left -> {Result, rotateRight(rotateRight({Atom, Height, NewLeftTree, RightTree})), none};
		%zig-zag
		right -> {Result, rotateRight({Atom, Height, rotateLeft(NewLeftTree), RightTree}), none}
	end;
% 2a. Wenn das gesuchte Element größer als das Atom ist, muss findWithSplay mit dem Rechten Baum durchgeführt werden.
findWithSplay({Atom, Height, LeftTree, RightTree}, Element) when Element > Atom ->
	{Result, NewRightTree, Marker} = findWithSplay(RightTree, Element),
	% Wenn der Marker «none» zurückgegeben wird, wird diese Funktion den veränderten Baum (mit dem neuen rechten Teil), die Höhe und den Marker «right» zurückliefern.
	% Wenn der Marker «right» ist, wird die zag-zag Operation auf den Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben.
	% Wenn der Marker «left» ist, wird die zag-zig Operation auf den Baum durchgeführt. Der veränderte Baum, die Höhe und der Marker «none» werden zurückgegeben.
	case Marker of
		none -> {Result, {Atom, Height, LeftTree, NewRightTree}, right};
		%zag-zig
		left -> {Result, rotateLeft({Atom, Height, LeftTree, rotateRight(NewRightTree)}), none};
		%zag-zag
		right -> {Result, rotateLeft(rotateLeft({Atom, Height, LeftTree, NewRightTree})), none}
	end.

% Hilfsmethode fuer die Lesbarkeit
toString(Atom) when is_integer(Atom) ->
	integer_to_list(Atom).

% Diese Funktion schreibt den Baum im dot Format 14 in die angegebene Datei. Dabei werden nur die
% für AVL Bäume notwendigen Notationen unterstützt.
% Die Dateistruktur für einen Graphen würde beispielsweise wie folgt aussehen:
% digraph avltree {
% 10 → 5 [label = 1];
% 10 → 15 [label = 1];
% }
% visibilty: public
% return: file
printBT(BTree, Filename) ->
	% 1. Schreibe die Graphbeschreibung und den Namen „avltree“ in die Datei1 und füge eine geschweifte Klammer ein, um die stmt_list2 zu öffnen.
	%    Die geschriebene Zeile sollte also wie folgt aussehen: digraph avltree {
	file:write_file(Filename, "digraph avltree\n{\n", [append]),
	printBTLine(BTree, Filename),
	%5. Füge als Letzte Zeile die schließende geschweifte Klammer ein um die stmt_list zu schließen
	file:write_file(Filename, "}", [append]).

% 3. Für leere Teilbäume wird nichts geschrieben.
printBTLine({_, _, {}, {}}, _Filename) ->
	ok;

% 3. Für leere Teilbäume wird nichts geschrieben.
% hier Teilbaum rechts leer
printBTLine({Atom, _, LinkerTeilBaum, {}}, Filename) ->
	{LAtom, LHoehe, _, _} = LinkerTeilBaum,
	% 2. Schreibe je eine Zeile für den linken und rechten Teilbaum wie folgt
	% <Element des Knotens> →  <Element des Kindknotens> [label = <Höhe des Kindknotens>];
	file:write_file(Filename, toString(Atom) ++ " -> " ++ toString(LAtom) ++ " [label = " ++ toString(LHoehe) ++ "];\n", [append]),
	% 4. Rufe diese Funktion für vorhandene Teilbäume wieder auf
	printBTLine(LinkerTeilBaum, Filename);

% 3. Für leere Teilbäume wird nichts geschrieben.
% hier Teilbaum links leer
printBTLine({Atom, _, {}, RechterTeilBaum}, Filename) ->
	{RAtom, RHoehe, _, _} = RechterTeilBaum,
	% 2. Schreibe je eine Zeile für den linken und rechten Teilbaum wie folgt
	% <Element des Knotens> →  <Element des Kindknotens> [label = <Höhe des Kindknotens>];
	file:write_file(Filename, toString(Atom) ++ " -> " ++ toString(RAtom) ++ " [label = " ++ toString(RHoehe) ++ "];\n", [append]),
	% 4. Rufe diese Funktion für vorhandene Teilbäume wieder auf
	printBTLine(RechterTeilBaum, Filename);

% Beide Teilbaueme vorhanden
printBTLine({Atom, _, LinkerTeilBaum, RechterTeilBaum}, Filename) ->
	{LAtom, LHoehe, _, _} = LinkerTeilBaum,
	{RAtom, RHoehe, _, _} = RechterTeilBaum,
	% 2. Schreibe je eine Zeile für den linken und rechten Teilbaum wie folgt
	% <Element des Knotens> →  <Element des Kindknotens> [label = <Höhe des Kindknotens>];
	file:write_file(Filename, toString(Atom) ++ " -> " ++ toString(LAtom) ++ " [label = " ++ toString(LHoehe) ++ "];\n", [append]),
	file:write_file(Filename, toString(Atom) ++ " -> " ++ toString(RAtom) ++ " [label = " ++ toString(RHoehe) ++ "];\n", [append]),
	% 4. Rufe diese Funktion für vorhandene Teilbäume wieder auf
	printBTLine(LinkerTeilBaum, Filename),
	printBTLine(RechterTeilBaum, Filename).




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

eqBT({}, {}) ->
		true;
eqBT(_FirstTree, {}) ->
		false;
eqBT({}, _SecondTree) ->
		false;
eqBT({FirstAtom, FirstHeight, FirstLeft, FirstRight}, {SecondAtom, SecondHeight, SecondLeft, SecondRight}) ->
		FirstAtom == SecondAtom andalso
		FirstHeight == SecondHeight andalso
		eqBT(FirstLeft, SecondLeft) andalso
		eqBT(FirstRight, SecondRight).

% Private Hilfsmethode
% Gibt die Hoehe eines Baumes zurueck
% visibilty: private
% params: Tupel
% return: Integer Atom
getHeight({}) -> 0;
getHeight({_Atom, Grad, _LinkerTeilBaum, _RechterTeilBaum}) ->
	Grad.



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
