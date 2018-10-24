# entop
A top-like Erlang node monitoring tool


## Introduction
Entop is a tool which shows information about a remote Erlang node in a way which is similar to unix 'top'.

For entop to run it needs `cecho 0.5.1` or higher
(http://www.github.com/mazenharake/cecho).

## Compile
To clean/compile run

    ./rebar3 clean
    ./rebar3 compile

You can now ensure that cecho and entop are installed in your erlang libs dir,
then you can copy the entop script to a bin directory

## Usage
To run entop make sure you have Erlang installed, cecho and entop libraries installed, and that the script is in your path.

    Usage: ./entop <TARGETNODE> <COOKIE>

### An example of how you run entop:

    > ./entop foo@11.0.1.2 secret

### User Interface:
entop's interface can be customized so this section only applies for the "built-in" interface.

#### Headers
##### First row
Shows information about the node which is more or less static such as the node name the operating system, erl flags and erlang version it is running.
##### Second row
Shows information on what the local time is (according to the node), how long it has been up for (Days:Hours:Minutes:Seconds) and how much latency there is to the node I.e. how long a net_adm:ping() takes.
##### Third row
Shows information about the processes of the system; the total number of processes, the run queue (number of processes scheduled to run by the scheduler(s)), the reductions per interval (RpI) which shows how many reductions the system has made since it last called the node.
##### Fourth row
Shows how much process memory is being used and the total amount.
##### Fifth row
Shows how much system memory, atom memory (currently used/total allocated), binary memory, code memory and ets memory.
##### Sixth row
Is left blank and is reserved for now.
##### Seventh row
Shows information about the rows in the list such as the interval in which the information is fetched, what the list is sorted on and how long it took to retrieve the information.

### Commands when running entop:

[0-9]:
  Sort on column number 0 through 9. Starts with first column (0)
  and up to the last column (9).

r:
  Toggles the sorting order from ascending to descending and vice versa.

q:
  Quits entop and return to the shell.

Ctrl-C:
  Same as 'q'.

'<' and '>':
  Moves the sorting column to the left or right respectively
  (these are the lower/greater-than-tags; not arrow keys).

### GProc Support

When entop starts, it checks if the ````gproc```` module is loaded on the target. If so, for processes with no ````registered_name````, entop checks for a gproc name, typically set using ````gen_server:start({via, gproc, ...} ...````.

Retrieving the gproc name for all processes at once is expensive, so names are dynamically fetched as rows are rendered.
The resulting pid/name results are cached in a local-to-entop ets table. The number of name lookups made is indicated in the table header.

GProc names are prefixed with ````l```` or ````g```` to indicate local or global registration.

Contribute
----------
Should you find yourself using entop and have issues, comments or feedback please [create an issue!] [1]

[1]: http://github.com/mazenharake/entop/issues "entop issues"
