entop
=====
A top-like Erlang node monitoring tool

Introduction
------------
Entop is a tool which shows information about a remote Erlang node in a way which is similar to unix 'top'. It needs cecho verion 0.3.0 or higher (http://www.github.com/mazenharake/cecho) to run.

Usage
-----
To run entop make sure you have Erlang installed and that the cecho library (http://www.github.com/mazenharake/cecho) is available in your Erlang code path. The start script assumes that you run it inside the entop application root directory, if you don't want that then change the paths in the scripts accordingly or just make sure you have the ebin/ directory for entop in your Erlang code path. Look at the start script for more details.

    Usage: ./entop <TARGETNODE> [-name <NAME>|-sname <SNAME>] [-setcookie <COOKIE>]

An example of how you run entop:

    > ./entop foo@11.0.1.2 -name entop@11.0.1.3 -setcookie secret

Commands when running entop:
[1-N]: 
  Sort on column number 1 through N. Starts with first column (1) and up to N
  where N is the last column.
    
r:
  Toggles the sorting order from ascending to descending and vice versa.

q:
  Quits entop and return to the shell.

Ctrl-C:
  Same as 'q'.

'<' and '>':
  Moves the sorting column to the left or right respectively

Contribute
----------
Should you find yourself using entop and have issues, comments or feedback please [create an issue!] [1]

[1]: http://github.com/mazenharake/entop/issues "entop issues"
