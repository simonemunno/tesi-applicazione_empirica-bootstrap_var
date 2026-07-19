# Metodi bootstrap per la quantificazione dell'incertezza del Value at Risk

Repository contenente il codice R utilizzato per l'applicazione empirica della tesi triennale:

**Metodi Bootstrap per la quantificazione dell'incertezza del Value at Risk**

L'applicazione considera i prezzi giornalieri dell'indice S&P 500, calcola i rendimenti logaritmici e le perdite relative, e confronta tre metodi di stima del Value at Risk:

1. VaR storico con bootstrap non parametrico;
2. VaR parametrico basato sulla distribuzione normale con bootstrap parametrico;
3. VaR parametrico basato sulla distribuzione t di Student con bootstrap parametrico.

## Struttura del repository

- `data/`: contiene i file CSV con i dati dell'S&P 500 scaricati da Stooq;
- `R/`: contiene gli script R dell'analisi empirica;
- `output/figures/`: contiene le figure generate dagli script;
- `output/tables/`: contiene le tabelle riassuntive dei risultati.

Le repliche bootstrap complete non sono incluse nel repository per evitare file di grandi dimensioni. Possono essere rigenerate eseguendo gli script R.

## Dati

I dati sono stati scaricati in formato CSV dalla piattaforma Stooq.

Sono considerate due finestre temporali:

- 2018--2022;
- 2021--2025.

Per ciascun giorno di negoziazione sono disponibili le variabili `Date`, `Open`, `High`, `Low`, `Close` e `Volume`. Nell'analisi vengono utilizzate soltanto `Date` e `Close`.

## Procedura

Dai prezzi di chiusura vengono calcolati i rendimenti logaritmici e le perdite relative. Il VaR viene stimato ai livelli di confidenza 95% e 99%.

## Replicazione

Per replicare l'analisi, aprire il progetto RStudio e lanciare gli script nella cartella `R/` nell'ordine numerico.

Le simulazioni bootstrap sono state eseguite con `B = 100000` (possono essere modificate secondo il proprio interesse e obiettivo).

## Software

Analisi svolta in R.
