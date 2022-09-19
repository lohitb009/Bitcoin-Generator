
# Distributed Bitcoin Mining Using Erlang

The goal of this project is to use Erlang and
the Actor Model to build a distributed solution to
'mine bitcoins'. The definition of 'mining a coin' is different in the context of this project. First, the program takes a required number of zeroes as the input. The problem statement is to is to find a set of strings which when hashed with SHA-256 have the required number of zeroes in the beginning of the hash. The first part of the string is pre-defined as the username of the user.

## Architecture Diagram

![Alt text](imgs_readme/arch.png?raw=true "Architecture")

## How to Run Project

1. Open two terminals in the src folder of this project.
2. On one of the terminals (say A terminal), enter the below commands:
```
erl -name Head2@127.0.0.1
erlang:set_cookie('lohit').
```
3. On the other terminal (say B terminal), enter the below commands:
```
erl -name ForLoop2@127.0.0.1
erlang:set_cookie('lohit').
net_kernel:connect_node('Head2@127.0.0.1').
nodes().
register(forLoop2,self()).
```
4. On Terminal A:
```
nodes().
{forLoop2, 'ForLoop2@127.0.0.1'} ! self().
c(headProcess).
c(minerProcess).
```
5. On Terminal B:
```
flush().
HeadProcessId = <above output>
c(forLoop).
c(cryptoWorker).
forLoop:startMining(HeadProcessId,<no_of_coins_req>,<no_of_zeroes_req>).
```
6. On Terminal A: (Immediately after Step 5 to avoid the timeout that has been set)
```
headProcess:receiveLoopRequest().
```


## Execution Details

1. Size of the work unit that resulted in the best performance:

The size of work unit that achieved maximum performance was 1. That is, if 100 coins were to be mined, 100 cryptoworkers and 100 coin collectors would be created.
This was determined as the best performance characteristic mainly due to the queuing delays 
that occurred when multiple tasks were associated with the same process. A scenario can be considered where two miners 
were given two coins (6 leading zeroes) each to mine. Due to luck in randomness, miner 1 finished mining both coins in 1 second.
Whereas, the miner 2 took 30 seconds to mine its first coin. This causes a delay in mining the second coin, while the miner 1 has been idle for a long time. Hence, due to
the factor of randomness in computation (unpredictability of expected time), it is most efficient to
have fine granularity in task distribution.

2. Result of running the program for input 4

The Client Side:
Note: Input Arguments (8,4) starts 8 mining processes mining each for 4 leading zeroes in hashes).
The current cpu and real time is printed each time a coin is mined on server side.

![Input](imgs_readme/2.1.png?raw=true "Input")

The Server Side: The mining occurs and results are printed

![Input](imgs_readme/2.1.png?raw=true "Output")

3. Ratio of CPU time to Real Time for above execution

CPU Time When Last Coin was mined : 21593 (from above pics)
Real Time When Last Coin was mined : 701145

Initial CPU Time: 15531
Initial Real Time: 699356

By calculating by taking differences, the ratio is 3.38

4. Coin with the most 0s found

The maximum zeroes found were 6, the output is given below.

![Input](imgs_readme/4.1.png?raw=true "Output")

5. Largest number of working machines achieved

In total, 2 machines (laptops) were used for executing the code. On one side there was a supervisor for the crypto workers and on the other, a coin collector head for the coin collectors. The crypto worker supervisor managed a total of  10000000 (ten million)crypto workers that mined coins. And the coin collector head managed  10000000 (ten million) coin-collectors that collected the mined coins from each of the 10000000 (ten million) crypto workers.
However it is possible for hosting the mining processes on many laptops/PCs if resources are available.  

