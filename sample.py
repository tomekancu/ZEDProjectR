"""
Pobierz zbiór danych breast-cancer.arff i zapoznaj się z jego opisem.

Wykorzystaj dowolne narzędzie (RapidMiner, Orange Data Mining, python, R, Weka)
 do zbudowania klasyfikatora typu drzewo decyzyjne.

Twoim zadaniem jest maksymalizacja ogólnej dokładności klasyfikatora (ang. accuracy) pod warunkiem uzyskania
co najmniej 90% czułości (ang. recall) dla klasy “recurrence-events”.

Trzy najlepsze rozwiązania otrzymują 20 XP, trzy kolejne otrzymują 15 XP, pozostałe otrzymują 10 XP.
Odpowiedź w postaci pliku *.pdf (1 strona, zrzut ekranu przedstawiający macierz pomyłek, kod pythona,
 plik z wyeksportowanym procesem przepływu) prześlij do niedzieli, 28 kwietnia,
 godz. 21:00 na adres Mikolaj.Morzy@put.poznan.pl
"""

import pandas as pd
from scipy.io import arff
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.tree import DecisionTreeClassifier


def main():
    data, meta = arff.loadarff("breast-cancer.arff")
    df = pd.DataFrame(data)

    y = df.pop('Class').replace([b"'no-recurrence-events'", b"'recurrence-events'"],
                                ['no-recurrence-events', 'recurrence-events']).astype('str')
    X = df
    X = X.apply(lambda x: LabelEncoder().fit_transform(x))

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

    model = DecisionTreeClassifier(
        criterion='entropy',
        max_depth=7,
        min_samples_leaf=7,
        random_state=41,
        class_weight={"no-recurrence-events": 1.0, "recurrence-events": 5.0}
    )

    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    tn, fp, fn, tp = confusion_matrix(y_test, y_pred).ravel()
    print(f'confusion matrix\n{tp, fp}\n{fn, tn}')
    print(f'accuracy: {accuracy_score(y_test, y_pred)}')
    recal_recorrence_events = classification_report(y_test, y_pred, output_dict=True)["recurrence-events"]["recall"]
    print(f'recal[recurrence-events]: {recal_recorrence_events}')


if __name__ == '__main__':
    main()
