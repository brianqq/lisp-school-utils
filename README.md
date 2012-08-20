Lisp-school-utils
==================
This is a collection of utilities I am writing as I do my AP Chem and AP Statistics summer assignments/homework. As of now the only purpose of this project is to help me learn and maybe save some time on assignments. Right now it contains very little.

#Statistics
`stats.lisp` contains a function for computing averages and a dsl for expressing formula in terms of E, the expected value operator. 

#Chemistry
`chem.lisp` contains utilities for scraping information about elements from their wikipedia pages.

#Todo
`todo.lisp` reads in .org files and sets timers that correspond to when events are scheduled. The timers bring up some sort of reminder, which is currently implemented as console output, but this will change.

#Dependencies/thanks
Quicklisp 
CL-PPCRE
Drakma
defmemo
trivial-timers