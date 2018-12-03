-module(avltree).
-export([isEmptyBT/1, initBT/0, findBT/2, inOrderBT/1, equalBT/2, deleteBT/2, isBT/1, insertBT/2,printBT/2]).
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
% Implizite Struktur prüfung mit Patternmatching
% 2. Prüfe, ob der Übergebene Baum der Struktur {<Element>,<Höhe>,<linker Teilbaum>,<rechter Teilbaum>} entspricht oder ein leeres Tuepl ist
isBT({}) ->
	true;
% Wenn keine Teilbaum vorhanden
% 4. [...] und die Teilbäume Tupel sind
isBT({Atom, Hoehe, {}, {}}) ->
	% 4. Prüfe, ob das Element und die Höhe Integer Atome, und die Teilbäume Tupel sind
	is_integer(Atom) andalso is_integer(Hoehe)
	% 5. Prüfe, ob der Knoten AVL ausgeglichen ist
	andalso Hoehe == 1;

% 4. [...] und die Teilbäume Tupel sind
isBT({_Atom, _Hoehe, {}, RechterTeilBaum}) when not is_tuple(RechterTeilBaum) ->
	false;
% Wenn Linker Teilbaum leer
isBT({Atom, Hoehe, {}, RechterTeilBaum}) when is_tuple(RechterTeilBaum) ->
	{RTBAtom, RTBHoehe, _RTBLinks, _RTBRechts} = RechterTeilBaum,
	% 4. Prüfe, ob das Element und die Höhe Integer Atome, und die Teilbäume Tupel sind
	case is_integer(Atom) andalso is_integer(Hoehe)
		% 5. Prüfe, ob die Kinder der Binärbaum Eigenschaft entsprechen (Linke Kinder kleiner, Rechte größer dem Element)
		andalso RTBAtom > Atom
		% 6. Prüfe, ob die Höhe korrekt gebildet wurde
		andalso Hoehe >= 1 andalso RTBHoehe == 1 andalso Hoehe == 2
		of
		false -> false;
		true ->
			% 7. Prüfe, ob der Knoten AVL ausgeglichen ist: Ist einer der Teilbäume leer, nehme 0 als Höhe
			{_RTBAtom, RechterTBHoehe, _RTBLinks, _RTBRechts} = RechterTeilBaum,
			BF = RechterTBHoehe - 0,
			(BF  >= -1 andalso BF =< 1)
	end;

% 4. [...] und die Teilbäume Tupel sind
isBT({_Atom, _Hoehe, LinkerTeilBaum, {}}) when not is_tuple(LinkerTeilBaum) ->
	false;
% Wenn Rechter Teilbaum leer
isBT({Atom, Hoehe, LinkerTeilBaum, {}}) when is_tuple(LinkerTeilBaum) ->
	{LTBAtom, LTBHoehe, _LTBLinks, _LTBRechts} = LinkerTeilBaum,

	% 4. Prüfe, ob das Element und die Höhe Integer Atome, und die Teilbäume Tupel sind
	case is_integer(Atom) andalso is_integer(Hoehe)
		% 5. Prüfe, ob die Kinder der Binärbaum Eigenschaft entsprechen (Linke Kinder kleiner, Rechte größer dem Element)
		andalso LTBAtom < Atom
		% 6. Prüfe, ob die Höhe korrekt gebildet wurde
		andalso Hoehe >= 1 andalso LTBHoehe == 1 andalso Hoehe == 2
		of
		false -> false;
		true ->
			% 7. Prüfe, ob der Knoten AVL ausgeglichen ist: Ist einer der Teilbäume leer, nehme 0 als Höhe
			{_LTBAtom, LinkerTBHoehe, _LTBLinks, _LTBRechts} = LinkerTeilBaum,
			BF = 0 - LinkerTBHoehe,
			(BF  >= -1 andalso BF =< 1)
	end;


% 4. [...] und die Teilbäume Tupel sind
isBT({_Atom, _Hoehe, LinkerTeilBaum, RechterTeilBaum}) when not (is_tuple(LinkerTeilBaum) andalso is_tuple(RechterTeilBaum)) ->
	false;
isBT({Atom, Hoehe, LinkerTeilBaum, RechterTeilBaum}) when is_tuple(LinkerTeilBaum) andalso is_tuple(RechterTeilBaum) ->
	{LTBAtom, LTBHoehe, _LTBLinks, _LTBRechts} = LinkerTeilBaum,
	{RTBAtom, RTBHoehe, _RTBLinks, _RTBRechts} = RechterTeilBaum,
	% 4. Prüfe, ob das Element und die Höhe Integer Atome, und die Teilbäume Tupel sind
	case is_integer(Atom) andalso is_integer(Hoehe)
		% 5. Prüfe, ob die Kinder der Binärbaum Eigenschaft entsprechen (Linke Kinder kleiner, Rechte größer dem Element)
		andalso LTBAtom < Atom andalso RTBAtom > Atom
		% 6. Prüfe, ob die Höhe korrekt gebildet wurde
		% Hoehencheck - die Differez der Höhen muss bei mindestens einem 1 sein und der Knoten muss hoeher sein als die Blaetter
		andalso Hoehe >= 1 andalso Hoehe - LTBHoehe >= 1 andalso Hoehe - RTBHoehe >= 1 andalso (Hoehe - LTBHoehe == 1 orelse Hoehe - RTBHoehe == 1)
		of
		false -> false;
		true ->
			% 7. Prüfe, ob der Knoten AVL ausgeglichen ist: Ist einer der Teilbäume leer, nehme 0 als Höhe
			{_LTBAtom, LinkerTBHoehe, _LTBLinks, _LTBRechts} = LinkerTeilBaum,
			{_RTBAtom, RechterTBHoehe, _RTBLinks, _RTBRechts} = RechterTeilBaum,
			BF = RechterTBHoehe - LinkerTBHoehe,
			case BF >= -1 andalso BF =< 1 of
				true ->
					% 8. Prüfe, ob die Teilbäume links und rechts ebenfalls AVL Bäume sind
					case isBT(LinkerTeilBaum) of
						true -> isBT(RechterTeilBaum);
						false -> false
					end;
				false -> false
			end
	end.


% Diese Funktion fügt einem ABL Baum ein Blatt hinzu und gibt den Veränderten/Erweiterten AVL Baum zurück
% visibilty: public
% return: btree
% 1. Ist der (Teil-)Baum Leer, wird das Atom mit der Höhe 1 in Baumstruktur zurückgeben
insertBT({}, Element) -> {Element, 1, {}, {}};

% 3. Addiere 1 zur Höhe des Knotens, wenn sich die maximale Höhe der Kinder verändert hat
% Biede Teilbaueme leer
insertBT({Atom, Hoehe, {}, {}},Element) when Element < Atom ->
	{Atom, Hoehe+1, insertBT({},Element), {}};

% 3. Addiere 1 zur Höhe des Knotens, wenn sich die maximale Höhe der Kinder verändert hat
insertBT({Atom, Hoehe, {}, {}},Element)	when Element > Atom ->
	{Atom, Hoehe+1, {}, insertBT({},Element)};

% 2. Vergleiche das Element mit dem Atom des Knotens:
%	 ist das Element kleiner dem Atom, füge das Element mit dieser Funktion in den Linken Teilbaum ein
% Teilbaum Links leer
insertBT({Atom, Hoehe, {}, RechterTeilBaum},Element) when Element < Atom ->
	{Atom, Hoehe, insertBT({},Element),RechterTeilBaum};

% 2. Vergleiche das Element mit dem Atom des Knotens:
%	 [...]
%	 ist das Element größer dem Atom, füge das Element mit dieser Funktion  in den rechten Teilbaum ein
% Teilbaum Rechts leer
insertBT({Atom, Hoehe, LinkerTeilBaum, {}},Element)	when Element > Atom ->
	{Atom, Hoehe, LinkerTeilBaum,insertBT({},Element)};

% 2. Vergleiche das Element mit dem Atom des Knotens:
%	 ist das Element kleiner dem Atom, füge das Element mit dieser Funktion in den Linken Teilbaum ein
% beide Teilbaueme gefuellt
insertBT({Atom, Hoehe, LinkerTeilBaum, RechterTeilBaum},Element) when Element < Atom ->
		{NLAtom, NLHoehe, NLLinkerTeilBaum, NLRechterTeilBaum} = insertBT(LinkerTeilBaum,Element),
		% 4. Prüfe, ob die Höhe des Knotens angepasst werden muss (d.h. die Höhe des neuen Kindes – die höhe des Knotens ergibt 0)
		case NLHoehe - Hoehe == 0 of
			true -> % 4b. Muss die Höhe angepasst werden, addiere 1 zur maximalen Höhe der Kinder
				{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, NeuBaumRechterTeil} = {Atom, Hoehe+1, {NLAtom, NLHoehe, NLLinkerTeilBaum, NLRechterTeilBaum}, RechterTeilBaum},
				% 5. Hat sich die Balance geändert, wird geprüft, ob die Balance noch stimmt (Differenz Höhe des rechten Kindes und Höhe des linken Kindes)
				case (getHeight(NeuBaumRechterTeil) - getHeight(NeuBaumLinkerTeil) < -1) orelse (getHeight(NeuBaumRechterTeil) - getHeight(NeuBaumLinkerTeil) > 1) of
					true -> % 5b. Liegt die Differenz außerhalb von -1 und 1, muss balanciert werden
						{NBLAtom, _NBLHoehe, _NBLLinks, _NBLRechts} = NeuBaumLinkerTeil,
						if % 6. Unterscheide 4 Fälle
							% Left Left Case
							( Element < NBLAtom ) ->
								% Left-Left Case -  Balancierung – mit einfacher Rotation
								% Die Überlast kommt vom linken Kind im linken Teilbaum
								% → Führe eine Rechtsrotation an der Wurzel dieses (Teil-)Baumes durch
								% 7. Gebe den (Teil-)Baum zurück
								rotateRight({NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, NeuBaumRechterTeil});
							% Left Right Case
							( Element > NBLAtom ) ->
								% 6b. Left-Right Case - Balancierung – mit doppelter Rotation
								% Die Überlast kommt vom rechten Kind im linken Teilbaum
								% → Führe eine Linksrotation mit dem linken Teilbaum durch und anschließend eine 	Rechtsrotation an der Wurzel dieses (Teil-)Baumes
								% 7. Gebe den (Teil-)Baum zurück
								rotateRight({NeuBaumAtom, NeuBaumHoehe, rotateLeft(NeuBaumLinkerTeil), NeuBaumRechterTeil});
							true -> {NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, NeuBaumRechterTeil}
						end;
					false -> % 5a. Liegt die Differenz zwischen -1 und 1, ist der Baum in Balance und kann zurückgegeben werden
						{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, NeuBaumRechterTeil}
				end;
			false -> % 4a. Hat sich die Höhe nicht geändert, hat sich die Balance des Knotens nicht verändert und der Baum kann zurückgegeben werden
				{Atom, Hoehe, {NLAtom, NLHoehe, NLLinkerTeilBaum, NLRechterTeilBaum}, RechterTeilBaum}
		end;

% 2. Vergleiche das Element mit dem Atom des Knotens:
%	 [...]
%	 ist das Element größer dem Atom, füge das Element mit dieser Funktion  in den rechten Teilbaum ein
% beide Teilbaume gefuellt
insertBT({Atom, Hoehe, LinkerTeilBaum, RechterTeilBaum},Element) when Element > Atom ->
		{NRAtom, NRHoehe, NRLinkerTeilBaum, NRRechterTeilBaum} = insertBT(RechterTeilBaum,Element),
		% 4. Prüfe, ob die Höhe des Knotens angepasst werden muss (d.h. die Höhe des neuen Kindes – die höhe des Knotens ergibt 0)
		case NRHoehe - Hoehe == 0 of
			true ->
				% 4b. Muss die Höhe angepasst werden, addiere 1 zur maximalen Höhe der Kinder
				{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, NeuBaumRechterTeil} = {Atom, Hoehe+1, LinkerTeilBaum, {NRAtom, NRHoehe, NRLinkerTeilBaum, NRRechterTeilBaum}},
				% 5. Hat sich die Balance geändert, wird geprüft, ob die Balance noch stimmt (Differenz Höhe des rechten Kindes und Höhe des linken Kindes)
				case (getHeight(NeuBaumRechterTeil) - getHeight(NeuBaumLinkerTeil) < -1) orelse (getHeight(NeuBaumRechterTeil) - getHeight(NeuBaumLinkerTeil) > 1) of
					true -> % 5b. Liegt die Differenz außerhalb von -1 und 1, muss balanciert werden
						{NBRAtom, _NBRHoehe, _NBRLinks, _NBRRechts} = NeuBaumRechterTeil,
						if % 6. Unterscheide 4 Fälle
							% Right Left Case
							( Element < NBRAtom ) ->
								% 6d. Right-Left Case - Balancierung – mit doppelter Rotation
								% Die Überlast kommt vom linken Kind im rechten Teilbaum
								% → Führe eine Rechtsrotation mit dem rechten Teilbaum durch und anschließend eine 	Linksrotation an der Wurzel dieses (Teil-)Baumes
								% 7. Gebe den (Teil-)Baum zurück
								rotateLeft({NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, rotateRight(NeuBaumRechterTeil)});
							% Right Right Case
							( Element > NBRAtom ) ->
								% 6c. Right-Right Case -  Balancierung – mit einfacher Rotation
								% Die Überlast kommt vom rechten Kind im rechten Teilbaum
								% → Führe eine Linksrotation an der Wurzel dieses (Teil-)Baumes durch
								% 7. Gebe den (Teil-)Baum zurück
								rotateLeft({NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, NeuBaumRechterTeil});
							true -> {NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, NeuBaumRechterTeil}
						end;

					false -> % 5a. Liegt die Differenz zwischen -1 und 1, ist der Baum in Balance und kann zurückgegeben werden
						{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinkerTeil, NeuBaumRechterTeil}
				end;
			false -> 	% 4a. Hat sich die Höhe nicht geändert, hat sich die Balance des Knotens nicht verändert und der Baum kann zurückgegeben werden
				{Atom, Hoehe, LinkerTeilBaum, {NRAtom, NRHoehe, NRLinkerTeilBaum, NRRechterTeilBaum}}
		end.



% Diese Funktion löscht ein Element aus einem Baum. Und zwar ist es wichtig, dass der Baum danach
% immer noch der Datenstrukturdefinition entspricht sowie AVL Eigenschaften aufweist.
% visibilty: public
% return: btree
% 2. Ist das Element ungleich dem Atom und der Knoten hat keine Kinder mehr, ist das Element nicht vorhanden und der leere Baum wird zurückgegeben
deleteBT({}, _Element) ->
		{};

% 1. Vergleiche das Übergebene Element mit dem Atom des Baumknotens
% 5. Ist das Element gleich dem Atom, unterscheide folgende Fälle:
% 5a. Hat der Baumknoten keine Kinder, kann das Element gelöscht werden und ein leerer Baum zurückgegeben werden
deleteBT({Atom, _Hoehe, {}, {}}, Element) when Element == Atom ->
	{};
% 5b. Hat der Baumknoten nur ein Kind, kann der entsprechende Teilbaum den Platz einnehmen und zurückgegeben werden
deleteBT({Atom, _Hoehe, {}, RechterTeilBaum}, Element) when Element == Atom ->
	RechterTeilBaum;
deleteBT({Atom, _Hoehe, LinkerTeilBaum, {}}, Element) when Element == Atom ->
	LinkerTeilBaum;

% 1. Vergleiche das Übergebene Element mit dem Atom des Baumknotens
% 5. Ist das Element gleich dem Atom, unterscheide folgende Fälle:
% 5c. Der Baumknoten hat links und rechts Teilbäume und der Ablauf geht weiter
deleteBT({Atom, Hoehe, LinkerTeilBaum, RechterTeilBaum}, Element) when Element == Atom ->
	% 6. Suche und merke das kleinste Element des rechten Teilbaumes
	NeuesAtom = fineTheSmallest(RechterTeilBaum),
	% 7. Lösche das kleinste Element aus dem rechten Teilbaum
	NeuerRechterTeil = deleteBT(RechterTeilBaum, NeuesAtom),
	% 9. Prüfe, ob sich die Höhe geändert hat
	case Hoehe-getHeight(NeuerRechterTeil) == 1 orelse Hoehe-getHeight(LinkerTeilBaum) == 1 of
	true ->
		% 8. Ersetze das zu löschende Element des Baumknotens mit dem gemerkten kleinsten Element des rechten Teilbaumes
		% 9a. Wenn sich die Höhe nicht geändert hat, gebe den (Teil-)Baum zurück
		{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts} = {NeuesAtom, Hoehe, LinkerTeilBaum, NeuerRechterTeil};
	false ->
		% 8. Ersetze das zu löschende Element des Baumknotens mit dem gemerkten kleinsten Element des rechten Teilbaumes
		% 9b. Hat sich die Höhe geändert, passe die Höhe an (Höhe -1)
		{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts} = {NeuesAtom, Hoehe-1, LinkerTeilBaum, NeuerRechterTeil}
	end,

	% 10. Prüfe, ob sich der Knoten noch AVL ausgeglichen ist (Höhe rechtes Kind – Höhe Linkes Kind zwischen -1 und 1)
	case (getHeight(NeuBaumRechts) - getHeight(NeuBaumLinks) < -1) orelse (getHeight(NeuBaumRechts) - getHeight(NeuBaumLinks) > 1) of
		true -> % 10b. Ist der Knoten im Ungleichgewicht , unterscheide 4 Fälle
			{_NBLAtom, _NBLHoehe, NBLLinks, NBLRechts} = NeuBaumLinks,

			% Left case Varianten pruefen (da wir aus dem rechten Teilbaum loeschen, kann nur der Linke Teil zu einer Ueberlast fuhren)
			case ( getHeight(NBLLinks) >= getHeight(NBLRechts) ) of
				true -> % Left Left case
					% 11a. Left-Left Case -  Balancierung – mit einfacher Rotation
					%   Die Überlast kommt vom linken Kind im linken Teilbaum
					%   → Führe eine Rechtsrotation an der Wurzel dieses (Teil-)Baumes  durch
					% 12. Gebe den veränderten Baum zurück
					rotateRight({NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts});

				false -> % Left Right Case bleibt uebrig
					% 11b. Left-Right Case - Balancierung – mit doppelter Rotation
					%   Die Überlast kommt vom rechten Kind im linken Teilbaum
					%   → Führe eine Linksrotation mit dem linken Teilbaum durch und anschließend eine 	Rechtsrotation an der Wurzel dieses (Teil-)Baumes
					% 12. Gebe den veränderten Baum zurück
					rotateRight({NeuBaumAtom, NeuBaumHoehe, rotateLeft(NeuBaumLinks), NeuBaumRechts})
			end;
			false -> % 10a. Ist der Knoten in Balance, gebe den (Teil-)Baum zurück
				{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts}
		end;

% Ist das Element noch nicht gefunden, wird in den Teilbaeumen gesucht

% 1. Vergleiche das Übergebene Element mit dem Atom des Baumknotens
% 3. Ist das Element kleiner dem Atom [...]
deleteBT({Atom, Grad, LinkerTeilBaum, RechterTeilBaum}, Element) when Element < Atom ->
	% 3. [..] versuche das Atom im linken Teilbaum zu löschen
	NeueLinkerTeil = deleteBT(LinkerTeilBaum, Element),
	% 9. Prüfe, ob sich die Höhe geändert hat
	case Grad-getHeight(NeueLinkerTeil) == 1 orelse Grad-getHeight(RechterTeilBaum) == 1 of
		true ->
			% 9a. Wenn sich die Höhe nicht geändert hat, gebe den (Teil-)Baum zurück
			{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts} = {Atom, Grad, NeueLinkerTeil, RechterTeilBaum};
		false ->
			% 9b. Hat sich die Höhe geändert, passe die Höhe an (Höhe -1)
			{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts} = {Atom, Grad-1, NeueLinkerTeil, RechterTeilBaum}
	end,
	% 10. Prüfe, ob sich der Knoten noch AVL ausgeglichen ist (Höhe rechtes Kind – Höhe Linkes Kind zwischen -1 und 1)
	case (getHeight(NeuBaumRechts) - getHeight(NeuBaumLinks) < -1) orelse (getHeight(NeuBaumRechts) - getHeight(NeuBaumLinks) > 1) of
		true -> % 10b. Ist der Knoten im Ungleichgewicht , unterscheide 4 Fälle
			{_NBRAtom, _NBRHoehe, NBRLinks, NBRRechts} = NeuBaumRechts,

			% Right case Variante pruefen - da wir im Linken teil geloescht haben, kann nur die Überlast im rechten Teil entstehen
			case ( getHeight(NBRRechts) >= getHeight(NBRLinks)) of
				true -> % Right Right case
					% 11c. Right-Right Case -  Balancierung – mit einfacher Rotation
					%  Die Überlast kommt vom rechten Kind im rechten Teilbaum
					%  → Führe eine Linksrotation an der Wurzel dieses (Teil-)Baumes durch
					% 12. Gebe den veränderten Baum zurück
					rotateLeft({NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts});
					false ->  %Right Left Case bleibt uebrig
					% 11d. Right-Left Case - Balancierung – mit doppelter Rotation
					%  Die Überlast kommt vom linken Kind im rechten Teilbaum
					%  → Führe eine Rechtsrotation mit dem rechten Teilbaum durch und anschließend eine 	Linksrotation an der Wurzel dieses (Teil-)Baumes
					% 12. Gebe den veränderten Baum zurück
					rotateLeft({NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, rotateRight(NeuBaumRechts)})
			end;
		false -> % 10a. Ist der Knoten in Balance, gebe den (Teil-)Baum zurück
		{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts}
end;

% 1. Vergleiche das Übergebene Element mit dem Atom des Baumknotens
% 4. Ist das Element größter dem Atom, [...]
deleteBT({Atom, Grad, LinkerTeilBaum, RechterTeilBaum}, Element) when Element > Atom->
	% 4. [...] versuche es im rechten Teilbaum zu löschen
	NeuerRechterTeil = deleteBT(RechterTeilBaum, Element),
	% 9. Prüfe, ob sich die Höhe geändert hat
	case Grad-getHeight(NeuerRechterTeil) == 1 orelse Grad-getHeight(LinkerTeilBaum) == 1 of
	true ->
		% 9a. Wenn sich die Höhe nicht geändert hat, gebe den (Teil-)Baum zurück
		{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts} = {Atom, Grad, LinkerTeilBaum, NeuerRechterTeil};
	false ->
		% 9b. Hat sich die Höhe geändert, passe die Höhe an (Höhe -1)
		{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts} = {Atom, Grad-1, LinkerTeilBaum, NeuerRechterTeil}
	end,
	% 10. Prüfe, ob sich der Knoten noch AVL ausgeglichen ist (Höhe rechtes Kind – Höhe Linkes Kind zwischen -1 und 1)
	case (getHeight(NeuBaumRechts) - getHeight(NeuBaumLinks) < -1) orelse (getHeight(NeuBaumRechts) - getHeight(NeuBaumLinks) > 1) of
		true -> % 10b. Ist der Knoten im Ungleichgewicht , unterscheide 4 Fälle
			{_NBLAtom, _NBLHoehe, NBLLinks, NBLRechts} = NeuBaumLinks,

			% Left case Varianten pruefen (da wir aus dem rechten Teilbaum loeschen, kann nur der Linke Teil zu einer Ueberlast fuhren)
			case 	( getHeight(NBLLinks) >= getHeight(NBLRechts) ) of
				true -> % Left Left case
					% 11a. Left-Left Case -  Balancierung – mit einfacher Rotation
					%   Die Überlast kommt vom linken Kind im linken Teilbaum
					%   → Führe eine Rechtsrotation an der Wurzel dieses (Teil-)Baumes  durch
					% 12. Gebe den veränderten Baum zurück
					rotateRight({NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts});

				false -> % Left Right Case bleibt uebrig
					% 11b. Left-Right Case - Balancierung – mit doppelter Rotation
					%   Die Überlast kommt vom rechten Kind im linken Teilbaum
					%   → Führe eine Linksrotation mit dem linken Teilbaum durch und anschließend eine 	Rechtsrotation an der Wurzel dieses (Teil-)Baumes
					% 12. Gebe den veränderten Baum zurück
					rotateRight({NeuBaumAtom, NeuBaumHoehe, rotateLeft(NeuBaumLinks), NeuBaumRechts})
			end;

		false -> % 10a. Ist der Knoten in Balance, gebe den (Teil-)Baum zurück
			{NeuBaumAtom, NeuBaumHoehe, NeuBaumLinks, NeuBaumRechts}
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

% Private Hilfsmethode um den niedrigsten Wert herauszufinden.
% da der Linke Teilbaum immer kleiner Seiner Zahl ist, muss nur die Linke Seite durchsucht werden
% Diese Methode wird beim Loeschen benoetigt
% visibilty: Private
% params: btree
% return: Integer
fineTheSmallest({}) ->
		'this is empty';
fineTheSmallest(BTree) ->
		{Atom, _Grad, LinkerTeilBaum, _RechterTeilBaum} = BTree,
		% 1. Ist der linke Teilbaum leer, gebe das Element des Knotens zurück
		if LinkerTeilBaum == {} ->
			Atom;
		true ->
			% 2. Ist der linke Teilbaum nicht leer, führe diese Funktion auf den Linken Teilbaum aus
			fineTheSmallest(LinkerTeilBaum)
		end.
