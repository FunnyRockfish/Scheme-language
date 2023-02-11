% Лабораторная работа № 7. Оболочка и скрипты
% 23 декабря 2022 г.
% Артём Черников, Егор Домаскин, ИУ9-12Б

#Цель работы
Научиться работать в консоли PowerShell и осознать, как она устроена. Установить 
bash, понять его и написать лабораторную работу на скриптовом языке так, чтобы 
она запускалась из консоли. Но всё-таки главная из этих целей - это именно 
разобраться в консоли, ведь в будущем она мне сильно пригодится!

#Реализация 
Файл `longprog.sh`
```bash
#!/bin/bash

echo stupid program started
sleep 10
echo stupid program stopped
echo  

Файл `myscript.sh`
```bash
#!/bin/bash

tfile="$(mktemp ./outputXXXXX.txt)" || exit 1
tfile2="$(mktemp ./errorXXXXX.txt)" || exit 1


program="$1"
interval="$2"

echo script started

sudo chmod +x ${program}

while true; do
    ./$program >> "$tfile" 2>> "$tfile2" &
    echo prog must be started
    sleep $(( "$interval" * 1 ))
    while pidof -x "$program" > /dev/null; do
     sleep $(( "$interval" * 1 ))
    done
done
```
Файл `cfile.sh`
```bash
countLines(){
  TOTAL=0
  for file in `find "$1" -name "*.c" -o -name "*.h"`
  do
    numOfLines=$(grep -cve '^\s*$' $file)
    TOTAL=$(($TOTAL + $numOfLines))
    echo $file $numOfLines
  done
  echo "Количество непустых строк:" $TOTAL
}

countLines "$1"
```
Файл `driver.py`
```python
#!/usr/bin/env python3


import sys
from mystrings import randstr

if __name__ == '__main__':
    length = int(sys.argv[1])
    strings = int(sys.argv[2])

    print(*randstr(length, strings), sep="\n")

Файл `mystrings.py`
```python

from secrets import choice
from random import sample
from string import digits, ascii_letters, punctuation

def randstr(length, count):
        if (type(length) != int or type(count) != int):
                return print("Arguments should be integer")

        if (count <= 0 or length <= 0):
                return print("Arguments should be positive")

        alphabet = digits + ascii_letters + punctuation

        strings = [""]*count
        for i in range(count):
                strings[i] = ''.join(choice(alphabet) for j in range(length))

        return strings
```
Файл `memoization.py`
```python
def memoize(func):
    memo = {}  # 

    def memorized(*args):  
        if args in memo:   
            return memo[args]
        else:
            memo[args] = func(*args)  
            return memo[args]

    return memorized

def fib(n):
    if n < 2:
        return n
    return fib(n - 2) + fib(n - 1)

fib = memoize(fib)
print(fib(10))
print(fib(15)) 
```
#Тестирование
Файл `memoization.py`
55
610

Файл `cfile.sh`
Количество непустых строк: 11

#Вывод
Я научился в полной мере оперировать со скриптовыми языками и оболочкой, 
разобрался в консоли PowerShell. Также я научился запускать и работать с файлами 
bash и python внутри консоли. Это было очень увлекательно, но совсем для меня не 
просто, ведь я всё таки почти впервые столкнулся с консолью. Но вместе с Егором 
Домаскиным я смог в полной мере разобраться в этой теме, и, полученные на 
лекциях знания нашли своё практическое применение при написании этой 
лабораторной работы.  
