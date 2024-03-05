**Opis rozwiązania**

**Znaki użyte do reprezentacji elementów planszy:**<br/>
Wall 	#<br/>
Player 	@<br/>
Player on goal square 	+<br/>
Box 	$<br/>
Box on goal square 	*<br/>
Goal square 	.<br/>
Floor 	(Space)<br/>
Blank   -	<br/>

**Modyfikacja funkcji drawState:**

Mam zdefiniowane typy
**type DrawFun = State -> Integer -> Integer -> Char**
**type Picture = DrawFun -> DrawFun**

Funkcja **draw** korzysta z funkcji **coloredScreen** i **stateScreen**

Funkcja **coloredScreen** dostaje łańcuch znaków i zwraca łańcuch z dodanymi sekwencjami ANSI dla kolorów.

Funkcja **stateScreen** dostaje stan planszy podczas rozgrywki i generuje odpowiednią reprezentację tekstową.
Korzysta z funkcji **tileIgnoringBoxes**, **tileNoticingBoxes** i **tileNoticingPlayer**.

Funkcja **tileIgnoringBoxes** dostaje stan planszy i dwie współrzędne. Zwraca znak, który powinien być reprezentacją
tekstową tego pola z zastrzeżeniem, że igronowane są wszystkie skrzynie i gracz.

Funkcja **tileNoticingBoxes** jest typu **type Picture = DrawFun -> DrawFun**. Na podstawie funkcji, którą dostanie,
generuje funkcję, która da taki sam wynik jak funkcja będąca argumentem, chyba że na danym polu powinna być skrzynia - 
wtedy zwracany znak jest zależny od tego, czy funkcja będąca argumentem zwracała reprezentację Storage dla danych współrzędnych.

Funkcja **tileNoticingPlayer** jest typu **type Picture = DrawFun -> DrawFun**. Na podstawie funkcji, którą dostanie,
generuje funkcję, która da taki sam wynik jak funkcja będąca argumentem, chyba że na danym polu stoi gracz - 
wtedy zwracany znak jest zależny od tego, czy funkcja będąca argumentem zwracała reprezentację Storage dla danych współrzędnych.

**Pozostałe elementy:**

Do obsługi kierunków używane są klawisze WASD.

Implementacja jest rozszerzona o kolory przy użyciu sekwencji ANSI dla kolorów (ekran startowy i ekran gry).
