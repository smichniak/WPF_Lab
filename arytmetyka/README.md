## Zadanie 1: Arytmetyka

## Arytmetyka przybliżonych wartości

Tam gdzie dokonujemy pomiarów wielkości fizycznych, wyniki są obarczone pewnym błędem, np. 5m ± 10%. Każdą taką przybliżoną wartość traktujemy jak zbiór możliwych wartości. Zaimplementuj pakiet operacji arytmetycznych na takich przybliżonych wartościach zawierający:

* konstruktory:
  * wartosc\_dokladnosc x p = x ± p% (dla p \> 0),
  * wartosc\_od\_do x y = (x+y)/2 ± (y-x)/2 (dla x < y),
  * wartosc\_dokladna x = x ± 0
* selektory:
  * in\_wartosc x y ⇔ wartość x może być równa y,
  * min\_wartosc x = kres dolny możliwych wartości x (lub -∞ jeśli możliwe wartości x nie są ograniczone od dołu),
  * max\_wartosc x = kres górny możliwych wartości x (lub ∞ jeśli możliwe wartości x nie są ograniczone od góry),
  * sr\_wartosc x = średnia (arytmetyczna) wartości min\_wartosc x i max\_wartosc x (lub nan jeśli min\_wartosc x i max\_wartosc x nie są skończone),
* modyfikatory:
  * plus a b = { x + y : in\_wartosc a x ∧ in\_wartosc b y },
  * minus a b = { x - y : in\_wartosc a x ∧ in\_wartosc b y },
  * razy a b = { x \* y : in\_wartosc a x ∧ in\_wartosc b y },
  * podzielic a b = {x / y:  in\_wartosc a x ∧ in\_wartosc b y }.

Zakładamy przy tym implicite, że wszystkie argumenty typu float są liczbami rzeczywistymi (tzn. są różne od infinity, neg\_infinity i nan.   
Natomiast w przypadku, gdy wynik nie jest liczbą rzeczywistą, powinien być odpowiednią z wartości: infinity, neg\_infinity lub nan.

Rozwiązując to zadanie możesz przyjąć następujące zasady ułatwiające rozumowanie:

* Przyjmij, że modyfikatory domykają wynikowe zbiory wartości -- to znaczy, jeżeli wynikiem jest przedział otwarty, to przyjmij, że zostaje on zamieniony na przedział domknięty.
* Operacje na wartościach przybliżonych są monotoniczne ze względu na zawieranie się zbiorów możliwych wartości.   
To znaczy, jeżeli wartości przybliżone x, y i z spełniają, jako zbiory możliwych wartości, x ⊆ y, to:  
plus x z ⊆ plus y z,  
plus z x ⊆ plus z y,  
i podobie dla innych operacji arytmetycznych.
* Kilka przykładów opartych o powyższą zasadę:

    let jeden = wartosc_dokladna 1.0;;  
    let zero = wartosc_dokladna 0.0;;  
    in_wartosc (razy jeden zero) 0.0;;  
    _- : bool = true_  
    in_wartosc (razy zero (wartosc_od_do 1.0 10.0)) 0.0;;  
    _- : bool = true_  
    in_wartosc (razy zero (wartosc_od_do 0.0 1.0)) 0.0;;  
    _- : bool = true_  
    let duzo = podzielic jeden (wartosc_od_do 0.0 1.0);;

    sr_wartosc duzo;; _- : float = infinity_ in_wartosc (razy zero duzo) 0.0;; _- : bool = true_

* Liczby zmiennopozycyjne i operacje na nich potrafią być zaskakujące. Na przykład, standard IEEE przewiduje dwie reprezentacje zera (+0.0 i -0.0), przy czym 1.0 /. 0.0 = infinity, oraz 1.0 /. (-0.0) = neg\_infinity.   
Może być to pomocne, np. jeśli dzielisz przez wartość przybliżoną, która zawiera jednostronne otoczenie zera.   
Ale może też okazać się pułapką, gdy rozważasz dzielenie przez wartość dokładnie równą zero.  
Pamiętaj, że w definicji operacji podzielic, występuje dzielenie "matematyczne", które nie jest określone gdy dzielimy przez zero.
* Może Ci się przydać standardowa procedura [classify\_float](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VALclassify_float), która ułatwia odróżnienie nieskończoności, symbolu nieokreślonego i zwykłych liczb.

Twoje rozwiązanie ma być umieszczone w pliku o nazwie arytmetyka.ml i pasować do specyfikacji interfejsu `arytmetyka.mli`.
