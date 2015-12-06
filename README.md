# Лабораторная № 1 Haskell 
Аникеенко Владислав 151003

# Параметры
```bash
Available options:
  -h,--help                Show this help text
  FILE                     input file.
  -o,--output FILE         File with output results
  -c,--class-count COUNT   Class count
  -a,--accuracy VALUE      Accuracy [0.0000001]
  -m,--metrix NAME         Dest [Euclid] | Hamming.
  -i,--initialization NAME Initialization [Center] | Accessory.
  -d,--delimiter VALUE     Delimiter for csv file. Default '.'
  -g,--ignore_header       Skip header.
  -f,--ignore_first_element
                           Skip first column.
  -l,--ignore_last_element Skip last column.
```

# Билд
```bash
cabal install mersenne-random
cabal install optparse-applicative
cd ./lab...
ghc make --main
```

# Запуск
```bash
main butterfly.txt -c 2 -l -d ","
```
