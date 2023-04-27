#kinematic
kinematyka <- function() {
  return("Kinematyka jest dziedziną nauki zajmującą się opisem ruchu ciał bez odnoszenia się do przyczyn ich ruchu. 
          W kontekście robotyki, kinematyka zajmuje się badaniem ruchu robotów, w tym analizą geometrii, położenia, prędkości,
          przyspieszeń i innych parametrów ruchu robotów. Kinematyka jest kluczowa dla projektowania, sterowania i programowania 
          robotów, ponieważ pozwala na określenie, jak robot powinien się poruszać, aby osiągnąć zamierzone cele.")
}

#simple kinematic task
simple_task <- function() {
  return("Proste zadanie kinematyki w robotyce polega na określeniu położenia (lub orientacji) końcówki robota (narzędzia)
          w przestrzeni na podstawie znanych położeń i wymiarów poszczególnych członów robota oraz parametrów kinematycznych
          (tzw. tabela Denavita-Hartenberga). Zadanie to jest często stosowane w robotyce przemysłowej w celu zaplanowania ruchu
          robota lub określenia, czy robot jest w stanie dotrzeć do określonego punktu w przestrzeni.\n\n
          Proste zadanie kinematyki można rozwiązać na wiele sposobów, przy użyciu różnych metod numerycznych i algorytmów.
          Jednym z najpopularniejszych sposobów jest metoda Jacobiego, która pozwala na szybkie obliczenie położenia końcówki
          robota na podstawie zadanych kątów przegubów")
}

# inverse kinematic task
inverse_task <- function() {
  return("Odwrotne zadanie kinematyki w robotyce to proces określenia kątów przegubów robota, potrzebnych do osiągnięcia 
          określonego położenia i orientacji końcówki narzędzia w przestrzeni. Innymi słowy, odwrotne zadanie kinematyki polega 
          na znalezieniu wartości kątów przegubów robota, aby końcówka narzędzia znalazła się w pożądanej pozycji i orientacji.
          
          Odwrotne zadanie kinematyki jest ważnym zagadnieniem w robotyce, ponieważ umożliwia planowanie ruchów robota na podstawie
          pożądanych pozycji końcówki narzędzia w przestrzeni, co ma zastosowanie w wielu dziedzinach, takich jak produkcja,
          medycyna, rolnictwo czy lotnictwo. Jednak rozwiązanie odwrotnego zadania kinematyki jest zwykle bardziej skomplikowane 
          niż prostego zadania kinematyki, ponieważ wymaga ono wyznaczenia wielu różnych rozwiązań i wyboru optymalnego rozwiązania, 
          które spełni zadane wymagania.")
}

#about me
about_me0 <- function() {
  return("Witaj w mojej aplikacji! Nazywam się Marek Lubszczyk i jestem twórcą aplikacji służącej  do 
          obliczania współrzędnych TCP robota na podstawie danych z tabeli DH.")
} 
about_me1 <- function() {
  return("Moim celem przy tworzeniu tej aplikacji było stworzenie narzędzia, które umożliwiłoby inżynierom i specjalistom od robotyki 
          szybkie i precyzyjne obliczanie współrzędnej TCP robota z wykorzystaniem tabeli DH.
          Jest ona prostym, ale skutecznym 
          narzędziem dzięki któremu można szybciej i skuteczniej projektować i programować roboty.  W aplikacji umieściłem także 
          funkcję wykresu 3D, który pozwala na wizualizację i sprawdzenie ustawienia robota. Aplikacja wykorzystuje matematyczne 
          równania i algorytmy, które pozwalają na precyzyjne obliczenia, nawet przy złożonych konfiguracjach robotów. Aplikacja 
          została zaprojektowana w oparciu o najnowsze technologie i standardy. Dzięki temu jest intuicyjna w obsłudze i działa 
          sprawnie nawet na słabszych urządzeniach.")
} 
about_me2 <- function() {
  return("Z przyjemnością wyrażam moje serdeczne podziękowania dla Roberta Pieprzycy za 
           cenny wkład i pomoc w tworzeniu tej aplikacji. Bez jego wsparcia i pomysłów nie udałoby się stworzyć tego narzędzia w takiej 
           formie, jaką znamy dzisiaj.")
} 
about_me3 <- function() {
  return("Jestem bardzo zadowolony z tej aplikacji i mam nadzieję, że będzie ona przydatna dla inżynierów i specjalistów od robotyki.
           Dziękuję Ci za odwiedzenie mojej strony i zapraszam do korzystania z mojej aplikacji.")
}

# dh parameters list 
dh_list <- function() {
  tags$ul(
    style = "margin-left: 40px; font-size: 25px; font-family: Times New Roman; white-space: nowrap;",
    tags$li("θi: kąt theta między osią z obrotu a osią z poprzedniego członu wzdłuż osi x."),
    "(oznaczenie w tabeli : t)",br(),br(),
    tags$li("di: odległość między osią z obrotu a osią z poprzedniego członu wzdłuż osi z."),
    "(oznaczenie w tabeli : d)",br(),br(),
    tags$li("ai: odległość między osią z obrotu i osią z poprzedniego członu wzdłuż osi x."),
    "(oznaczenie w tabeli : a)",br(),br(),
    tags$li("αi: kąt między osią z poprzedniego członu a osią z obecnego członu wzdłuż osi x."),
    "(oznaczenie w tabeli : al)",br(),br(),
    tags$ul(
      style = "margin-left: 50px; font-size: 20px; font-family: Times New Roman; white-space: nowrap;",
      "gdzie: ",br(),
      tags$li("i to numer kolejnego członu,"),
      tags$li("a, α, d, θ to zmienne opisujące geometrię połączenia między członami.")
    )
  )
}