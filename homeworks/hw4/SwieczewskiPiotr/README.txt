ORYGINALNY WYKRES: https://x.com/irgendeine_lea/status/1838510725502738677
DANE: https://www.kaggle.com/datasets/datasnaek/chess/data (TidyTuesday Week 40 , 2024) 

PROBLEMY ORYGINALNEJ WIZUALIZACJI:
- Estetyka: za dużo etykiet przez co trudno czytać, niektóre kolory słabo widać
- Scandinavian Defense... (cyan) znika z bins 1600-<2000 a na wykresie linia dalej 
jest co jest z lekka mylące tym bardziej że potem pojawiają się ułamkowe wartości
- W kategorii 2000+ mamy ranks: 1, 2.5, 3.5. Być może chodziło o to
że te dwa otwarcia pojawiają się tak samo często aczkolwiek to nie 
uzasadnia wyboru wartości ułamkowych (+szanse na to że 2 otwarcia są tak samo 
często są bardzo małe)
- Nie jest to problem z samym wykresem aczkolwiek w danych różnice 
ratingu są czasem znaczące np. 1500 vs 1191 więc pytanie czy były
one pomijane na oryginalnym wykresie czy wyciągana była średnia arytmetyczna

MOJE ZMIANY
- Jeżeli bardzo nam zależy żeby zostać przy bumpchart to żeby było to jakkolwiek czytelne // estetyczne trzeba sztucznie ograniczyć się do 5 kategorii (próbowałem kilka razy bez tego). Ogólnie doszedłem do wniosku że najlepiej zmienić typ wykresu.
