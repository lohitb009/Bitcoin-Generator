
## <u>Distributed Bitcoin Mining Using Erlang</u>
##### <i>Lohit Bhambri(lohit.bhambri@ufl.edu)</i> 
##### <i>Imthiaz Hussain (imthiazh.hussain@ufl.edu)</i>


The goal of this project is to use Erlang and
the Actor Model to build a distributed solution to
'mine bitcoins'. The definition of 'mining a coin' is different in the context of this project. The problem statement is to find a set of strings, when hashed with SHA-256, have the required number of zero's as prefix. The first part of the string is pre-defined as the username of the user.

## Architecture Diagram

![Alt text](imgs_readme/Arch.png?raw=true "Architecture")

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

The size of work unit that achieved maximum performance is 1 i.e., if 100 coins are to be mined, 100 crypto-workers and coin-collectors need to be spawned simultaneously.
This is determined as the best performance characteristic mainly due to the queuing delays that occurred when multiple collector requests were assigned to the same worker process. A scenario was considered where two miners 
were given two coins (6 leading zeroes) each to mine. Due to luck in randomness, miner 1 finished mining both coins in 1 second.
Whereas, miner 2 took 30 seconds to mine its first coin. This causes a delay in mining the second coin, while the miner 1 has been idle all this time. Hence, due to
the factor of randomness in computation (unpredictability of expected time), it is most efficient to
have fine granularity in task distribution. After finishing mining of a coin, the cryptoworker can be set to continue mining infinitely if needed.

2. Result of running the program for input 4

The Client Side:
Note: Input Arguments (100,4) starts 100 mining processes each mining coins for 4 leading zeroes).
The current cpu and real time is printed each time a coin is mined on server side.

```aidl
(ForLoop2@127.0.0.1)11> forLoop:startMining(HeadProcessId,100,4).
Initial Real Time is 19876196 milliseconds 
Initial CPU time is 936203 milliseconds
Worker Pid is <0.29544.4> 
Worker Pid is <0.29545.4> 
Worker Pid is <0.29546.4>
Worker Pid is <0.29547.4>
Worker Pid is <0.29548.4>
Worker Pid is <0.29549.4>
Worker Pid is <0.29550.4>
Worker Pid is <0.29551.4>
Worker Pid is <0.29552.4> 
Worker Pid is <0.29553.4>
Worker Pid is <0.29554.4>
Worker Pid is <0.29555.4> 
Worker Pid is <0.29556.4> 
Worker Pid is <0.29557.4> 
Worker Pid is <0.29558.4>
Worker Pid is <0.29559.4>
Worker Pid is <0.29560.4> 
Worker Pid is <0.29561.4>
Worker Pid is <0.29562.4> 
Worker Pid is <0.29563.4> 
Worker Pid is <0.29564.4> 
Worker Pid is <0.29565.4> 
Coin# 86 Current Total CPU Time: 937953 milliseconds 
Coin# 86 Current Total Real Time: 19876943 milliseconds 
Worker Pid is <0.29566.4>
Worker Pid is <0.29567.4> 
Coin# 78 Current Total CPU Time: 938078 milliseconds 
Coin# 78 Current Total Real Time: 19876960 milliseconds
Coin# 100 Current Total CPU Time: 938203 milliseconds 
Coin# 100 Current Total Real Time: 19876973 milliseconds 
Worker Pid is <0.29568.4> 
Worker Pid is <0.29569.4> 
Worker Pid is <0.29570.4> 
Worker Pid is <0.29571.4> 
Worker Pid is <0.29572.4> 
Worker Pid is <0.29573.4> 
Worker Pid is <0.29574.4>
Worker Pid is <0.29575.4> 
Worker Pid is <0.29576.4> 
Worker Pid is <0.29577.4> 
Worker Pid is <0.29578.4> 
Worker Pid is <0.29579.4> 
Worker Pid is <0.29580.4>
Worker Pid is <0.29581.4> 
Worker Pid is <0.29582.4> 
Worker Pid is <0.29583.4> 
Worker Pid is <0.29584.4>
Worker Pid is <0.29585.4> 
Worker Pid is <0.29586.4>
Worker Pid is <0.29587.4> 
Worker Pid is <0.29588.4>
Worker Pid is <0.29589.4> 
Worker Pid is <0.29590.4> 
Worker Pid is <0.29591.4> 
Worker Pid is <0.29592.4> 
Worker Pid is <0.29593.4> 
Worker Pid is <0.29594.4> 
Worker Pid is <0.29595.4> 
Worker Pid is <0.29596.4> 
Worker Pid is <0.29597.4> 
Worker Pid is <0.29598.4> 
Worker Pid is <0.29599.4> 
Worker Pid is <0.29600.4> 
Coin# 84 Current Total CPU Time: 942281 milliseconds
Coin# 84 Current Total Real Time: 19877529 milliseconds 
Worker Pid is <0.29601.4>
Worker Pid is <0.29602.4> 
Worker Pid is <0.29603.4>
Worker Pid is <0.29604.4> 
Coin# 71 Current Total CPU Time: 942890 milliseconds
Coin# 71 Current Total Real Time: 19877604 milliseconds
Worker Pid is <0.29605.4> 
Worker Pid is <0.29606.4>
Coin# 43 Current Total CPU Time: 942890 milliseconds
Coin# 43 Current Total Real Time: 19877611 milliseconds 
Coin# 68 Current Total CPU Time: 943109 milliseconds
Worker Pid is <0.29607.4>
Coin# 68 Current Total Real Time: 19877639 milliseconds 
Worker Pid is <0.29608.4> 
Worker Pid is <0.29609.4> 
Worker Pid is <0.29610.4> 
Worker Pid is <0.29611.4> 
Coin# 90 Current Total CPU Time: 943718 milliseconds 
Coin# 90 Current Total Real Time: 19877718 milliseconds
Worker Pid is <0.29612.4> 
Worker Pid is <0.29613.4> 
Worker Pid is <0.29614.4> 
Worker Pid is <0.29615.4>
Coin# 99 Current Total CPU Time: 944343 milliseconds 
Coin# 99 Current Total Real Time: 19877799 milliseconds
Worker Pid is <0.29616.4> 
Worker Pid is <0.29617.4>
Worker Pid is <0.29618.4>
Worker Pid is <0.29619.4> 
Worker Pid is <0.29620.4>
Worker Pid is <0.29621.4>
Worker Pid is <0.29622.4> 
Worker Pid is <0.29623.4>
Worker Pid is <0.29624.4> 
Worker Pid is <0.29625.4> 
Worker Pid is <0.29626.4>
Worker Pid is <0.29627.4> 
Worker Pid is <0.29628.4> 
Worker Pid is <0.29629.4> 
Worker Pid is <0.29630.4> 
Worker Pid is <0.29631.4> 
Worker Pid is <0.29632.4> 
Worker Pid is <0.29633.4> 
Worker Pid is <0.29634.4> 
Worker Pid is <0.29635.4>
Coin# 79 Current Total CPU Time: 946937 milliseconds 
Coin# 79 Current Total Real Time: 19878136 milliseconds 
Worker Pid is <0.29636.4>
Worker Pid is <0.29637.4> 
Worker Pid is <0.29638.4> 
Worker Pid is <0.29639.4>
Worker Pid is <0.29640.4> 
Coin# 66 Current Total CPU Time: 947671 milliseconds
Coin# 66 Current Total Real Time: 19878233 milliseconds 
Worker Pid is <0.29641.4>
Worker Pid is <0.29642.4> 
Worker Pid is <0.29643.4>
Finished spawning processes 
ok
(ForLoop2@127.0.0.1)12> Coin# 29 Current Total CPU Time: 948437 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 29 Current Total Real Time: 19878340 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 89 Current Total CPU Time: 949406 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 89 Current Total Real Time: 19878461 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 73 Current Total CPU Time: 949890 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 73 Current Total Real Time: 19878523 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 15 Current Total CPU Time: 950671 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 15 Current Total Real Time: 19878632 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 10 Current Total CPU Time: 950671 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 10 Current Total Real Time: 19878638 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 21 Current Total CPU Time: 951875 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 21 Current Total Real Time: 19878795 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 87 Current Total CPU Time: 952984 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 87 Current Total Real Time: 19878935 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 80 Current Total CPU Time: 953718 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 80 Current Total Real Time: 19879021 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 64 Current Total CPU Time: 953718 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 64 Current Total Real Time: 19879030 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 51 Current Total CPU Time: 953843 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 51 Current Total Real Time: 19879045 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 65 Current Total CPU Time: 953968 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 65 Current Total Real Time: 19879059 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 72 Current Total CPU Time: 954328 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 72 Current Total Real Time: 19879097 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 17 Current Total CPU Time: 954453 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 17 Current Total Real Time: 19879114 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 33 Current Total CPU Time: 955203 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 33 Current Total Real Time: 19879208 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 18 Current Total CPU Time: 955453 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 18 Current Total Real Time: 19879246 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 2 Current Total CPU Time: 955953 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 97 Current Total CPU Time: 955953 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 2 Current Total Real Time: 19879303 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 97 Current Total Real Time: 19879304 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 98 Current Total CPU Time: 956578 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 98 Current Total Real Time: 19879391 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 5 Current Total CPU Time: 957578 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 5 Current Total Real Time: 19879504 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 45 Current Total CPU Time: 959703 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 45 Current Total Real Time: 19879777 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 42 Current Total CPU Time: 960078 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 42 Current Total Real Time: 19879820 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 57 Current Total CPU Time: 961125 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 57 Current Total Real Time: 19879965 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 9 Current Total CPU Time: 962187 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 48 Current Total CPU Time: 962187 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 9 Current Total Real Time: 19880104 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 48 Current Total Real Time: 19880104 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 85 Current Total CPU Time: 962312 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 85 Current Total Real Time: 19880119 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 7 Current Total CPU Time: 962531 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 7 Current Total Real Time: 19880155 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 35 Current Total CPU Time: 962781 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 35 Current Total Real Time: 19880175 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 53 Current Total CPU Time: 962906 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 53 Current Total Real Time: 19880194 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 27 Current Total CPU Time: 962906 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 27 Current Total Real Time: 19880205 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 67 Current Total CPU Time: 963156 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 67 Current Total Real Time: 19880235 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 63 Current Total CPU Time: 964625 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 63 Current Total Real Time: 19880412 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 55 Current Total CPU Time: 965703 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 55 Current Total Real Time: 19880556 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 20 Current Total CPU Time: 966078 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 20 Current Total Real Time: 19880607 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 69 Current Total CPU Time: 966687 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 69 Current Total Real Time: 19880684 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 22 Current Total CPU Time: 967796 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 22 Current Total Real Time: 19880819 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 61 Current Total CPU Time: 970750 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 61 Current Total Real Time: 19881203 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 39 Current Total CPU Time: 971000 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 39 Current Total Real Time: 19881224 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 54 Current Total CPU Time: 971234 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 54 Current Total Real Time: 19881255 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 1 Current Total CPU Time: 971343 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 1 Current Total Real Time: 19881274 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 96 Current Total CPU Time: 972484 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 96 Current Total Real Time: 19881438 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 8 Current Total CPU Time: 974640 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 8 Current Total Real Time: 19881715 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 56 Current Total CPU Time: 976781 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 56 Current Total Real Time: 19881998 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 94 Current Total CPU Time: 977656 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 94 Current Total Real Time: 19882109 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 4 Current Total CPU Time: 980234 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 4 Current Total Real Time: 19882426 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 14 Current Total CPU Time: 980578 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 14 Current Total Real Time: 19882474 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 37 Current Total CPU Time: 983406 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 37 Current Total Real Time: 19882836 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 81 Current Total CPU Time: 985250 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 81 Current Total Real Time: 19883096 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 58 Current Total CPU Time: 986171 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 58 Current Total Real Time: 19883211 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 38 Current Total CPU Time: 986281 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 38 Current Total Real Time: 19883234 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 77 Current Total CPU Time: 986750 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 77 Current Total Real Time: 19883288 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 62 Current Total CPU Time: 987109 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 62 Current Total Real Time: 19883343 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 82 Current Total CPU Time: 987453 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 82 Current Total Real Time: 19883393 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 13 Current Total CPU Time: 987921 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 13 Current Total Real Time: 19883452 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 92 Current Total CPU Time: 991640 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 92 Current Total Real Time: 19883938 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 12 Current Total CPU Time: 992109 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 12 Current Total Real Time: 19883992 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 52 Current Total CPU Time: 992968 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 52 Current Total Real Time: 19884106 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 16 Current Total CPU Time: 993843 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 16 Current Total Real Time: 19884209 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 30 Current Total CPU Time: 993843 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 30 Current Total Real Time: 19884220 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 47 Current Total CPU Time: 994781 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 47 Current Total Real Time: 19884334 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 46 Current Total CPU Time: 994906 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 46 Current Total Real Time: 19884353 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 25 Current Total CPU Time: 995140 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 25 Current Total Real Time: 19884387 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 93 Current Total CPU Time: 995468 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 93 Current Total Real Time: 19884431 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 60 Current Total CPU Time: 995812 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 60 Current Total Real Time: 19884474 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 32 Current Total CPU Time: 997828 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 32 Current Total Real Time: 19884752 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 40 Current Total CPU Time: 999500 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 40 Current Total Real Time: 19884961 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 11 Current Total CPU Time: 999734 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 11 Current Total Real Time: 19884993 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 95 Current Total CPU Time: 1002812 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 95 Current Total Real Time: 19885396 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 75 Current Total CPU Time: 1003156 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 75 Current Total Real Time: 19885453 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 31 Current Total CPU Time: 1003640 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 31 Current Total Real Time: 19885503 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 49 Current Total CPU Time: 1005703 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 49 Current Total Real Time: 19885784 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 36 Current Total CPU Time: 1007718 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 36 Current Total Real Time: 19886048 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 6 Current Total CPU Time: 1009187 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 6 Current Total Real Time: 19886229 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 26 Current Total CPU Time: 1009421 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 26 Current Total Real Time: 19886265 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 34 Current Total CPU Time: 1013781 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 34 Current Total Real Time: 19886826 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 76 Current Total CPU Time: 1015640 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 76 Current Total Real Time: 19887067 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 23 Current Total CPU Time: 1017078 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 23 Current Total Real Time: 19887254 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 28 Current Total CPU Time: 1017312 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 28 Current Total Real Time: 19887299 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 70 Current Total CPU Time: 1018265 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 70 Current Total Real Time: 19887421 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 24 Current Total CPU Time: 1019125 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 24 Current Total Real Time: 19887527 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 59 Current Total CPU Time: 1019859 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 59 Current Total Real Time: 19887615 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 50 Current Total CPU Time: 1019968 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 50 Current Total Real Time: 19887629 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 88 Current Total CPU Time: 1024531 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 88 Current Total Real Time: 19888226 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 83 Current Total CPU Time: 1024734 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 83 Current Total Real Time: 19888265 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 74 Current Total CPU Time: 1026484 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 74 Current Total Real Time: 19888556 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 3 Current Total CPU Time: 1026562 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 3 Current Total Real Time: 19888567 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 41 Current Total CPU Time: 1026562 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 41 Current Total Real Time: 19888572 milliseconds
(ForLoop2@127.0.0.1)12> Coin# 44 Current Total CPU Time: 1026843 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 44 Current Total Real Time: 19888668 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 19 Current Total CPU Time: 1027000 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 19 Current Total Real Time: 19888745 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 91 Current Total CPU Time: 1027484 milliseconds 
(ForLoop2@127.0.0.1)12> Coin# 91 Current Total Real Time: 19889228 milliseconds 
(ForLoop2@127.0.0.1)12>

```

[//]: # (![Input]&#40;imgs_readme/2.1.png?raw=true "Input"&#41;)

The Server Side: The mining occurs and results are printed

```aidl
(Head2@127.0.0.1)10> headProcess:receiveLoopRequest().
In Miner Area 
MinerPid is <0.29552.4> 
Minned "lohit.bhambri;IjdIe0ygvfs=" hashed result is "0000786c86199107911dd3f7eb225ba75ffb253a344c044fc97c4efecdd657b0"

In Miner Area
In Miner Area 
MinerPid is <0.29560.4> 
MinerPid is <0.29538.4>
Minned "lohit.bhambri;qvVbQzXmPw4=" hashed result is "00003c77afed218115782605a44a7cee2a0a89a382961d5d3db168d9d17df91d" 
Minned "lohit.bhambri;Do8uZQAUhjc=" hashed result is "00003fe202c38c8f4adf1f3889d36859843c27f3e6dfebd87eb963d174dd705a" 


In Miner Area 
MinerPid is <0.29554.4> 
Minned "lohit.bhambri;pZNwcgQ4X4Q=" hashed result is "0000eb7c640b7b1bb14c0f9638f4d8b0093aa3c92834f2732c6aa838633c6853"

In Miner Area 
MinerPid is <0.29567.4> 
Minned "lohit.bhambri;+a7nFus/IGY=" hashed result is "0000d9bd85c17d8514d95cf2c29f798e77748f106c41bd7d3ed81ed1c46bd79a"

In Miner Area 
MinerPid is <0.29595.4>
Minned "lohit.bhambri;ifW1Tk36HMs=" hashed result is "0000ab003afdaaf6c0ed52f1a0519d33f8449324842fe563a43b3f3fffd812fe"

In Miner Area 
MinerPid is <0.29570.4>
Minned "lohit.bhambri;k+jji3ok1B4=" hashed result is "00001d78d2379cc59663544b04cdcfe8be109f5403a203cb17e903be87335ee1" 

In Miner Area 
MinerPid is <0.29548.4> 
Minned "lohit.bhambri;PxbT1zfEoo4=" hashed result is "000035af298191941c5441ef9cff574597239bd777f714cad03fd2b37e8e142b" 

In Miner Area
MinerPid is <0.29539.4> 
Minned "lohit.bhambri;WYpPPCwQC2c=" hashed result is "0000ce137b7a47769f56729155914748cbb80b455b714459eed09ec983e75599" 

In Miner Area 
MinerPid is <0.29559.4> 
Minned "lohit.bhambri;92mEAc2lz/k=" hashed result is "0000af7bbb2cf5ac0767faceeaffbd77041c45812f5e5484cf0e71f4547e15b5" 

In Miner Area 
MinerPid is <0.29572.4> 
Minned "lohit.bhambri;SN+lL5dKDSQ=" hashed result is "0000bfb6e89e70d9262999112cea665bb89dca981dc6ce6b4102838d8c484e57" 

In Miner Area 
MinerPid is <0.29609.4> 
Minned "lohit.bhambri;os6Dp9VDFCk=" hashed result is "0000e1cea13814ac00e1a625a4ec6856f59b774e84023a0038b60f5b875ab249" 

In Miner Area 
MinerPid is <0.29549.4> 
Minned "lohit.bhambri;P59fXPzSwZw=" hashed result is "00000d6e74fddcb74d40f98f0210a692a332906b93d7c2e357d1a4bf1e501876" 

In Miner Area
MinerPid is <0.29565.4> 
Minned "lohit.bhambri;Qpu6qP3iP18=" hashed result is "0000c698a1c3e496efc4a7014bef0a8caa0ad74868110a70e5385a5292cea002" 

In Miner Area 
MinerPid is <0.29623.4> 
Minned "lohit.bhambri;WI0PbaB2pWw=" hashed result is "0000a676cc9f484a456a07ca93da4e4240346d1ab7cff897a93e6812de161101"

In Miner Area
MinerPid is <0.29628.4> 
Minned "lohit.bhambri;TCGHS7kOpwI=" hashed result is "0000b3823137b38abdd17f0352fd5863dafccc7798213dc4f637af67559773d5"

In Miner Area 
MinerPid is <0.29617.4> 
Minned "lohit.bhambri;Tb1PtJB3PRA=" hashed result is "00000f1c5d8286dc57442295115e5da4e460719ed3c8a52a3b3e3f178ea68f00" 

In Miner Area 
MinerPid is <0.29551.4> 
Minned "lohit.bhambri;F/owB3Ijiw0=" hashed result is "000036e0c2704589168d782ab4dac47eb0addf932264f6215e83083744310348"

In Miner Area 
MinerPid is <0.29558.4> 
Minned "lohit.bhambri;xtMQWYj/E7I=" hashed result is "0000e9397fa0f5e3ac9a3440c51fe93f05644667d53c56edc9e0016e048c6494" 

In Miner Area 
MinerPid is <0.29574.4> 
Minned "lohit.bhambri;Ztvuyyz+A/I=" hashed result is "0000fa32ff7a1a02ebc629149fcb873ad686598c7c1ca6638181d4e7b9a2e688"

In Miner Area 
MinerPid is <0.29587.4>
Minned "lohit.bhambri;Nvy6IpHeRgY=" hashed result is "0000d188894a7b71a54b6c15042317c74e3ca4febece21e8b7fbd48641ebfec3" 

In Miner Area
MinerPid is <0.29573.4> 
Minned "lohit.bhambri;SZ6b+UchtZE=" hashed result is "0000b6681a3227a302dfce291ed77e7ef097ef171d66376ff1ee1c24d091201c" 

In Miner Area 
MinerPid is <0.29566.4>
Minned "lohit.bhambri;fxEXlaA1cA0=" hashed result is "00009f4e939d5a73ec3490a1b08645f9f2188855be3de8213d25338b0ef0dd91" 

In Miner Area
MinerPid is <0.29621.4> 
Minned "lohit.bhambri;eikXGfRhFHY=" hashed result is "0000b0a5728759bcf09c336d62ff3d43ba8ad0d98541075444f816d339fa5380" 

In Miner Area
MinerPid is <0.29605.4> 
Minned "lohit.bhambri;ltbs5KOd3Vc=" hashed result is "00006c7154131e312fd389070654ab42e48f2071f17b32319072ef968a91e560" 

In Miner Area
MinerPid is <0.29620.4> 
Minned "lohit.bhambri;WO0aLeb8W7g=" hashed result is "00006873b09279b0cc780ab7cfbaeee6429b8089f73738ed5ee311912e3584d5" 

In Miner Area 
In Miner Area 
MinerPid is <0.29541.4> 
MinerPid is <0.29636.4>
Minned "lohit.bhambri;ik0/d0EV0jU=" hashed result is "00007a067ba463d73681030c031e30838c7b10c7ba8cc803352aeb234d1b880e" 
Minned "lohit.bhambri;X+7TgjDmq4M=" hashed result is "000051ae171f0ca5fd1f4702b8bde9a6231f21352147dd57fc65136ca2bf8f84" 


In Miner Area 
MinerPid is <0.29540.4> 
Minned "lohit.bhambri;hsW+W3n3raM=" hashed result is "000092d752be40432fd462a6aafebec7eeb3d7430d2de893aa248de00b1da200" 

In Miner Area
MinerPid is <0.29633.4> 
Minned "lohit.bhambri;b/R4kRqfUOE=" hashed result is "0000027c8c89281ecaa58454f52098afa2a7f69850d6102e7c502b99f1bb0f0e" 

In Miner Area 
MinerPid is <0.29593.4> 
Minned "lohit.bhambri;NnT178B5l1U=" hashed result is "00008be90b519d3b3c70a216cb53e7a12fba3334ca1fd3f651bba2f21080b7f1" 

In Miner Area 
MinerPid is <0.29596.4> 
Minned "lohit.bhambri;+nUjkcSmaXc=" hashed result is "0000d5249cec16453101194569e3971f5ce7c5d11f9105324233f26734e0a6c3" 

In Miner Area 
MinerPid is <0.29581.4> 
Minned "lohit.bhambri;NCkL0xWxnLE=" hashed result is "00001234cdc4544cf3019a1f338351d0753581a7f0607be3eb82c65aa9ebaffa" 

In Miner Area 
In Miner Area 
In Miner Area
MinerPid is <0.29629.4> 
MinerPid is <0.29590.4> 
MinerPid is <0.29553.4>
Minned "lohit.bhambri;YZMgToE2vDw=" hashed result is "0000ee25eee2e0253f5cd58b52279144be131a3a5ed40d2d474521b010392f98"
Minned "lohit.bhambri;5ibyXVK8F4c=" hashed result is "000032b7b80b2bec4c386a11d4202485f32c84cf9cc3c4b9dd693291b5adaaa5" 
Minned "lohit.bhambri;KcT6f5YzOiM=" hashed result is "00007a6948a9a067fcc7c8bdfdd7ef80504e4d2bacbc54c1aba0145e96c7cf94"



In Miner Area
MinerPid is <0.29631.4>
Minned "lohit.bhambri;2CR/zr9h2pA=" hashed result is "0000b1f8755d88063de754b0b3e54d0e31f7cb7767b76b76a989f67ec81fb31b" 

In Miner Area 
MinerPid is <0.29603.4>
Minned "lohit.bhambri;1ohyMoCfr+c=" hashed result is "00005b1b1d408cf82ebe2580c70c254d5e68dd0875ec41e3cdb6c27a4748ce3c" 

In Miner Area 
MinerPid is <0.29585.4> 
Minned "lohit.bhambri;A+uw2CP/J8g=" hashed result is "00000cef95c3fef114cccfb5095b430818c96fa0019b01f5e747acd926777a60"

In Miner Area 
MinerPid is <0.29611.4> 
Minned "lohit.bhambri;TKvsHXBsxdE=" hashed result is "000080ec4bb4e950ea4d9af54cc7db0e8c421f715f0f0fe3445ebbf56d2aa82e"

In Miner Area 
MinerPid is <0.29571.4>
Minned "lohit.bhambri;qXLs1E8H3gk=" hashed result is "000013da3e652dda31984fe4de78cd067585811686353ed00d6a7f927f17d4e4" 

In Miner Area
MinerPid is <0.29575.4> 
Minned "lohit.bhambri;owo5A9iqj6E=" hashed result is "0000bbd5b0ec03ca0f330f4fc30235cca00b8163b9f5e83ebe421b1e5a35952e"

In Miner Area 
MinerPid is <0.29583.4>
Minned "lohit.bhambri;+fJDYABacMQ=" hashed result is "000090ff4ff5039d9ddb9eaa8998fba62f09a5c352909fba2009ef7bbf0f2dcc" 

In Miner Area 
MinerPid is <0.29618.4>
Minned "lohit.bhambri;QGGDhy6Uov0=" hashed result is "000059869c26d53c312b036fda4e5ee5abea4d0d3ab5e4898583f02521563b81"

In Miner Area 
MinerPid is <0.29569.4>
Minned "lohit.bhambri;+drYYe7QcNU=" hashed result is "0000f41718f2921f75ebc665941e4295c9b3a65e30c99491467a1e1253564a36" 

In Miner Area 
MinerPid is <0.29616.4> 
Minned "lohit.bhambri;9J9SZMUVQuY=" hashed result is "000006f4d22b3f4e22e38ff58369693bfefe9dca6c87e25c85ec46f7b061ed78" 

In Miner Area 
MinerPid is <0.29577.4> 
Minned "lohit.bhambri;qFk4zcnJWSM=" hashed result is "0000375140d66d76dabd2a4ee2c3e6fc1ee4c13827f82e1992ad1e2a99bd63f6" 

In Miner Area 
MinerPid is <0.29599.4> 
Minned "lohit.bhambri;FSMxbjEw/jg=" hashed result is "0000c76fd2ff8611ce509d4f3cc2f17dc0608acf6fdfe380c98b3e4646d8dd92" 

In Miner Area 
In Miner Area 
MinerPid is <0.29584.4>
MinerPid is <0.29637.4> 
Minned "lohit.bhambri;Yzn4kHDjSZQ=" hashed result is "0000ed37f4cdad5ee9ff254dcbb6f81dba0244ce769222f5a7c0c4c65941495a" 
Minned "lohit.bhambri;Bgsd8Y3MWtU=" hashed result is "0000dcb2d5e22571746d82bd21cb8c7d158f7d0b634214981ae2a0b7cba62223" 


In Miner Area 
MinerPid is <0.29542.4> 
Minned "lohit.bhambri;kB04qOkXMXg=" hashed result is "00005c8baa9af474f6688c0248379849a70d70b30ad1f0dfe1a2f11d486d330d"

In Miner Area 
MinerPid is <0.29630.4> 
Minned "lohit.bhambri;PcVS2dwnH5M=" hashed result is "00000fc84d1411d86bc1400b4405481ce01f0b817aac9eac1c47c748783e728b"

In Miner Area 
MinerPid is <0.29582.4> 
Minned "lohit.bhambri;HouKdQmw3FM=" hashed result is "00004456fbdfe0cc8854fe21aef705cdaef970acfaf5c7352c212a2fbf002bdd" 

In Miner Area 
MinerPid is <0.29544.4> 
Minned "lohit.bhambri;hyWB1oRUleY=" hashed result is "00006755673fcfcec38776a71ccf5b782311fd48a10597b8b404e9cbdf6d5c7b" 

In Miner Area 
MinerPid is <0.29634.4> 
Minned "lohit.bhambri;sqDS8krQ/2U=" hashed result is "000043d75f7867f04c14829701e6bfc0a2842e294119b3b8da6bdaccbe808125"

In Miner Area 
MinerPid is <0.29624.4> 
Minned "lohit.bhambri;YcosT4dlvRQ=" hashed result is "0000fac41e9806fcb4f375d3fd1153320825edb9f03bfe420dfcf684794eee52" 

In Miner Area 
MinerPid is <0.29601.4> 
Minned "lohit.bhambri;Y9aJtnMWR1U=" hashed result is "0000db2bbfda449042154a75d200dae906f38f62eecb40da6263dccd297640e6" 

In Miner Area 
MinerPid is <0.29557.4> 
Minned "lohit.bhambri;L8iqblxI+Ek=" hashed result is "0000a7abc99c48f570037a022ec0ecdfdf0312476536ce770ab0ad7583f9abff" 

In Miner Area 
MinerPid is <0.29580.4> 
Minned "lohit.bhambri;uCx142JkoWo=" hashed result is "00006b6643396ecacce24e74c7f43386eee8bbc88433a12d29e20dd27dd3daf6" 

In Miner Area
MinerPid is <0.29600.4>
Minned "lohit.bhambri;oGIjwdkuxLE=" hashed result is "0000bb55a829bbdb2b92b9ca24416673ef0d1723c564d2c39764314f0957c3ed" 

In Miner Area 
MinerPid is <0.29561.4> 
Minned "lohit.bhambri;RtaX0xSiIe0=" hashed result is "00005ce42873bb12b64a494102de0ba4adbb03186a0818e449f96276290b38b3" 

In Miner Area 
MinerPid is <0.29576.4> 
Minned "lohit.bhambri;eJxFbL80fwY=" hashed result is "0000309694ab7cb3749d866d979343d7740b51a7c0406f4b70def562eb1a5df6" 

In Miner Area 
MinerPid is <0.29556.4> 
Minned "lohit.bhambri;6d7RcEtbKbs=" hashed result is "00002146233696279bcc03d7e5e3efad321ff0683fc0ac3fcac820b65760126c"

In Miner Area 
MinerPid is <0.29625.4>
Minned "lohit.bhambri;zIOfN9BgSRo=" hashed result is "00008fc8b443e5bd4412d5eb4ebb09ec6e895766126803815459de65a4fba920" 

In Miner Area 
MinerPid is <0.29546.4> 
Minned "lohit.bhambri;TYqjp9koM7o=" hashed result is "0000b839a5359974ea1970100ca35d576526ffb6e9007b856ee2d9f58b536f08"

In Miner Area
MinerPid is <0.29626.4> 
Minned "lohit.bhambri;LU3AZBaMD1I=" hashed result is "00000a584ef5e785e24ad5b3c3eb1fd516cb88a4ee3515dc69c58ee113543057" 

In Miner Area 
MinerPid is <0.29586.4> 
Minned "lohit.bhambri;4P4h3gttkao=" hashed result is "0000ba164d2922bca77e5d3d453e930f8f39f356979abeb466da4337696cfc8b"

In Miner Area 
MinerPid is <0.29622.4> 
Minned "lohit.bhambri;GxoTlXGpbBw=" hashed result is "0000aa465899c29036e27ce989661c0f681d7497e7534f72516024b0340060bb"

In Miner Area 
MinerPid is <0.29608.4> 
Minned "lohit.bhambri;jxvZn0P2Rgs=" hashed result is "0000227296d724a9a7063da020879ce28f92ceed3a85b339cd94b50cf249b100" 

In Miner Area 
MinerPid is <0.29591.4> 
Minned "lohit.bhambri;KHXNzRBMHIM=" hashed result is "00008cc5d3d091aecf482101f0a8a52826ac8ab4ca9611cd5eac2f2c92ff94cf"

In Miner Area 
MinerPid is <0.29592.4>
Minned "lohit.bhambri;02ewjap3SAY=" hashed result is "00008b4b4319098f5e0b2da0300996cd5c98901bb4a8cd0aae44ccd8939826fe"

In Miner Area 
MinerPid is <0.29613.4>
Minned "lohit.bhambri;Z4lI1avTKlA=" hashed result is "000039fde3bb8ed38717d88cbbb9a6c7d848f385599cba92715bc8b14e3beade" 

In Miner Area 
MinerPid is <0.29545.4> 
Minned "lohit.bhambri;DYaH1N+ePIo=" hashed result is "0000da9b2511db7ec8039005fe3797c71606e4737d04d8a3217fec22261fbbee" 

In Miner Area 
MinerPid is <0.29578.4> 
Minned "lohit.bhambri;fplK4cyrBoc=" hashed result is "000065721a3a9943baba6285fd8d3647fda6acd38d7df61849d37d4e2d90990f" 

In Miner Area 
MinerPid is <0.29606.4> 
Minned "lohit.bhambri;Vir9nI20rAM=" hashed result is "000037365a106d2b1922a8682e167e1e81e4d3de54c9b803391075d8b8984577" 

In Miner Area 
MinerPid is <0.29598.4> 
Minned "lohit.bhambri;nn7ytUBYKUc=" hashed result is "00008024a6dbcc5861a9b63678d847251c2c7bc30bc7a2ffc3c5799ad9452b86" 

In Miner Area 
MinerPid is <0.29627.4> 
Minned "lohit.bhambri;pDBKQa7NkrQ=" hashed result is "00001a5033fa81d59ef784e0cbc2b52c63a54f4a1dfefe86befb6bb3b5b995d4"

In Miner Area 
MinerPid is <0.29543.4> 

```

[//]: # (![Input]&#40;imgs_readme/2.1.png?raw=true "Output"&#41;)

3. Ratio of CPU time to Real Time for above execution

CPU Time When Last Coin was mined : 1027484 milliseconds (from above pics)

Real Time When Last Coin was mined : 19889228 milliseconds

Initial CPU Time: 936203 milliseconds

Initial Real Time: 19876196 milliseconds

Ratio = result=(final_cpu-init_cpu)/(final_real-init_real)

By calculating by taking differences, the ratio is 7.004373848987108

4. Coin with the most 0s found

The maximum zeroes found were 6, the input and output is given below. There were 100 cryptoworkers spawned but they gave only 16 coins after allowing a runtime of 10 minutes.

Input:

```aidl
(ForLoop2@127.0.0.1)13> forLoop:startMining(HeadProcessId,100,6). 
Initial Real Time is 20396813 milliseconds 
Initial CPU time is 1027500 milliseconds
Worker Pid is <0.29646.4> 
Worker Pid is <0.29647.4> 
Worker Pid is <0.29648.4>
Worker Pid is <0.29649.4>
Worker Pid is <0.29650.4>
Worker Pid is <0.29651.4>
Worker Pid is <0.29652.4>
Worker Pid is <0.29653.4>
Worker Pid is <0.29654.4> 
Worker Pid is <0.29655.4>
Worker Pid is <0.29656.4>
Worker Pid is <0.29657.4> 
Worker Pid is <0.29658.4>
Worker Pid is <0.29659.4> 
Worker Pid is <0.29660.4>
Worker Pid is <0.29661.4> 
Worker Pid is <0.29662.4> 
Worker Pid is <0.29663.4>
Worker Pid is <0.29664.4> 
Worker Pid is <0.29665.4> 
Worker Pid is <0.29666.4>
Worker Pid is <0.29667.4> 
Worker Pid is <0.29668.4> 
Worker Pid is <0.29669.4>
Worker Pid is <0.29670.4> 
Worker Pid is <0.29671.4>
Worker Pid is <0.29672.4>
Worker Pid is <0.29673.4>
Worker Pid is <0.29674.4>
Worker Pid is <0.29675.4>
Worker Pid is <0.29676.4>
Worker Pid is <0.29677.4> 
Worker Pid is <0.29678.4>
Worker Pid is <0.29679.4>
Worker Pid is <0.29680.4> 
Worker Pid is <0.29681.4> 
Worker Pid is <0.29682.4> 
Worker Pid is <0.29683.4> 
Worker Pid is <0.29684.4> 
Worker Pid is <0.29685.4> 
Worker Pid is <0.29686.4>
Worker Pid is <0.29687.4> 
Worker Pid is <0.29688.4> 
Worker Pid is <0.29689.4> 
Worker Pid is <0.29690.4> 
Worker Pid is <0.29691.4> 
Worker Pid is <0.29692.4> 
Worker Pid is <0.29693.4> 
Worker Pid is <0.29694.4> 
Worker Pid is <0.29695.4>
Worker Pid is <0.29696.4> 
Worker Pid is <0.29697.4> 
Worker Pid is <0.29698.4>
Worker Pid is <0.29699.4> 
Worker Pid is <0.29700.4> 
Worker Pid is <0.29701.4>
Worker Pid is <0.29702.4> 
Worker Pid is <0.29703.4> 
Worker Pid is <0.29704.4> 
Worker Pid is <0.29705.4> 
Worker Pid is <0.29706.4>
Worker Pid is <0.29707.4> 
Worker Pid is <0.29708.4> 
Worker Pid is <0.29709.4> 
Worker Pid is <0.29710.4>
Worker Pid is <0.29711.4> 
Worker Pid is <0.29712.4> 
Worker Pid is <0.29713.4>
Worker Pid is <0.29714.4> 
Worker Pid is <0.29715.4>
Worker Pid is <0.29716.4> 
Worker Pid is <0.29717.4> 
Worker Pid is <0.29718.4>
Worker Pid is <0.29719.4> 
Worker Pid is <0.29720.4> 
Worker Pid is <0.29721.4> 
Worker Pid is <0.29722.4> 
Worker Pid is <0.29723.4> 
Worker Pid is <0.29724.4>
Worker Pid is <0.29725.4> 
Worker Pid is <0.29726.4> 
Worker Pid is <0.29727.4> 
Worker Pid is <0.29728.4> 
Worker Pid is <0.29729.4> 
Worker Pid is <0.29730.4>
Worker Pid is <0.29731.4> 
Worker Pid is <0.29732.4> 
Worker Pid is <0.29733.4> 
Worker Pid is <0.29734.4> 
Worker Pid is <0.29735.4> 
Worker Pid is <0.29736.4> 
Worker Pid is <0.29737.4>
Worker Pid is <0.29738.4> 
Worker Pid is <0.29739.4> 
Worker Pid is <0.29740.4>
Worker Pid is <0.29741.4> 
Worker Pid is <0.29742.4> 
Worker Pid is <0.29743.4>
Worker Pid is <0.29744.4>
Worker Pid is <0.29745.4> 
Finished spawning processes
ok
(ForLoop2@127.0.0.1)14> Coin# 60 Current Total CPU Time: 1047750 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 60 Current Total Real Time: 20399877 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 76 Current Total CPU Time: 2095031 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 76 Current Total Real Time: 20533737 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 42 Current Total CPU Time: 2237218 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 42 Current Total Real Time: 20552211 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 39 Current Total CPU Time: 2457718 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 39 Current Total Real Time: 20580633 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 35 Current Total CPU Time: 2826140 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 35 Current Total Real Time: 20628577 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 80 Current Total CPU Time: 4030109 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 80 Current Total Real Time: 20788333 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 20 Current Total CPU Time: 4213796 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 20 Current Total Real Time: 20814237 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 24 Current Total CPU Time: 4400375 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 24 Current Total Real Time: 20840642 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 45 Current Total CPU Time: 4405625 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 45 Current Total Real Time: 20841346 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 2 Current Total CPU Time: 4552343 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 2 Current Total Real Time: 20861987 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 53 Current Total CPU Time: 4742484 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 53 Current Total Real Time: 20888033 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 91 Current Total CPU Time: 4752390 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 91 Current Total Real Time: 20889310 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 57 Current Total CPU Time: 5257765 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 57 Current Total Real Time: 20954887 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 83 Current Total CPU Time: 5475156 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 83 Current Total Real Time: 20983974 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 23 Current Total CPU Time: 5608500 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 23 Current Total Real Time: 21002060 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 85 Current Total CPU Time: 5623968 milliseconds 
(ForLoop2@127.0.0.1)14> Coin# 85 Current Total Real Time: 21004222 milliseconds 
(ForLoop2@127.0.0.1)14> ^G.
Eshell V13.0.4  (abort with ^G)
(ForLoop2@127.0.0.1)1>
```
Output
```aidl
(Head2@127.0.0.1)12> headProcess:receiveLoopRequest().
In Miner Area 
MinerPid is <0.29680.4> 
Minned "lohit.bhambri;Qcq5K/ue1OQ=" hashed result is "0000000603aea0b4a1d29fdd92aaa3d1185821953643ec29ba46abc34f579759" 

Timeoutok
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29664.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;1kf9fALedCw=" hashed result is "000000b072ac7919cbf0d0f0b72f1a80c5c2252646da3bc7ec5100741223d512" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29698.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;9Y7HUgcx/Go=" hashed result is "0000007b1cebd3679511a03abe6dca08ccce5bae34441c117e91b12841abfe8e" 
(Head2@127.0.0.1)13>
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29701.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;FHFpHB3WelQ=" hashed result is "000000041a5438c0323181760cf009b41d3d6ee78856a5c657bad4593e8efbf7" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29705.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;1fdyBAoyVgw=" hashed result is "0000003b2acccaed5cb9bb1e7c195e24aed502c3262b8af07ac9e0e6c846261f" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29660.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;C7gQVJk/v50=" hashed result is "0000004ab6a72dc791ff00126e4955c0e26120d0e600d87afe6c647d64130249" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29720.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;CpPjz7AA2B8=" hashed result is "0000000755b08488dfaed62473a8fa7eda81afc3d3ff1a2709f92cebffc8a12c" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29716.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;LnRNszCeWME=" hashed result is "00000091840303c47968c2fd83eedbce4db96f31aed161d87cbc01c0edb35e5d" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29695.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;/dys2dXFJUc=" hashed result is "0000007ade9c9df84b9a60a19a258a74ffb422eae9e1bae9ac9043350a3a8072" 
(Head2@127.0.0.1)13>
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29738.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;M/KgyDgsa9g=" hashed result is "0000008dadf2699d7652671793740cc83c1b64280df6ab3a4fbb21095b87f52c" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29687.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;ZnVmhM47PrI=" hashed result is "00000018e03c38a56ec2b9d18ab6e472cbaf5ee44de12ea0a7f36d6d445cc432" 
(Head2@127.0.0.1)13>
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29649.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;rIamAjTBdZM=" hashed result is "00000076c8800c190f9a6e1885296390049d78faa9721d8213f8193812525dee" 
(Head2@127.0.0.1)13>
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29683.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;EYtJdHApxGI=" hashed result is "000000d6e74d686fbf6179b2515adba0dc6c7a3aab14115281c9ebfd9f4b5972" 
(Head2@127.0.0.1)13>
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29657.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;xpVyFxtwMxE=" hashed result is "00000068d5dda85bca9e7988083c22b89a469e46100413cfb8e16b7a9435718e" 
(Head2@127.0.0.1)13>
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29717.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;Z79zE3TLY88=" hashed result is "000000d4be6c4a7f737e4f0587251970bbf26eb189b4d111ff4966736fa36244" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> In Miner Area 
(Head2@127.0.0.1)13> MinerPid is <0.29655.4> 
(Head2@127.0.0.1)13> Minned "lohit.bhambri;9A8Gyr3TvoM=" hashed result is "000000b3b9cdeeeef242fb920ced27df71b04ae67dced15d2ac3ae86e9718bce" 
(Head2@127.0.0.1)13> 
(Head2@127.0.0.1)13> ^G.
Eshell V13.0.4  (abort with ^G)
(Head2@127.0.0.1)1> 

```

[//]: # (![Input]&#40;imgs_readme/4.1.png?raw=true "Output"&#41;)

5. Largest number of working machines achieved

In total, 2 machines (laptops) were used for executing the code. On one side there was a supervisor for the crypto workers and on the other, a coin collector head for the coin collectors. The crypto worker supervisor managed a total of  10000000 (ten million)crypto workers that mined coins. And the coin collector head managed  10000000 (ten million) coin-collectors that collected the mined coins from each of the 10000000 (ten million) crypto workers.
However it is possible to host the mining processes on many laptops/PCs if resources are available.  

