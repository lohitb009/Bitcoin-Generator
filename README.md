
# Distributed Bitcoin Mining Using Erlang

The goal of this project is to use Erlang and
the Actor Model to build a distributed solution to
'mine bitcoins'. The definition of 'mining a coin' is different in the context of this project. The problem statement is to find a set of strings, when hashed with SHA-256, have the required number of zero's as prefix. The first part of the string is pre-defined as the username of the user.

[//]: # (## Architecture Diagram)

[//]: # ()
[//]: # (![Alt text]&#40;imgs_readme/Arch.png?raw=true "Architecture"&#41;)



## Execution Details

1. Size of the work unit that resulted in the best performance:

The size of work unit that achieved maximum performance was any value above 5. The performance characteristic used was the ratio of CPU time to real time.
For 100 coins to mine, various work unit values were given and tested. Finally, a work unit of 8 was selected as the machines used for testing are provided with 8 processors. When a worker was tasked with 8 coins to mine, it would spawn 8 miner processes. Hence, matching the processors and the work unit  would enable a complete and efficient use of CPU resources (without any queueing delays at the processors). Once a worker in the other machine/terminal finishes the task, it is assigned another work unit of tasks.

![Alt text](imgs_readme/1.1.png?raw=true "Architecture")




2. Result of running the program for input 4

The Master Side (Laptop/Terminal 1):
Note: Input Arguments (100,4) user input suggests a task of 100 coins having 4 leading zeroes.
Note: 'Supervisor' in below output refers to master node.

```aidl
(server@127.0.0.1)1> server:start_process(100,4).
The local process ID for workers to connect : <0.5695.0>. 
Please note the above. Waiting for 15 seconds before starting mining. Workers may connect mid-way while mining.
nothing
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #1 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5693.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;B6JOKM8+rXo=" hashed result is "00002c417566bfd88460be4982ea7f719a2e3eb8f5827755332994cf32974274" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #2 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5693.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;NgcIvP3xybk=" hashed result is "00004ff4eebbfd8bfa4bf30d84953d42ce35ec067b001383b53553fbe4aeb6a8" 
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #3 Mined!
(server@127.0.0.1)2> Mined by Process PID : <0.5693.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;ahAG4lL6aG0=" hashed result is "000025d4ca7c22d364e9079ada81c82af930ef3bb15170f0adc3774b963e484d"
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #4 Mined!
(server@127.0.0.1)2> Mined by Process PID : <0.5693.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;eewOVR/K5Xo=" hashed result is "000055ceb42711e38a37c30d4b70e295bff682d392d0db37308a7e516e9271db"
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #5 Mined!
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;slT/B4Q6WrI=" hashed result is "0000d490a13ed475099c0df6861858b7440d6ffd6f51b55cdf1e2c4b9188a680"
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #6 Mined!
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;gXg98f7hYsY=" hashed result is "00005fceceeedafc5d644a927e02a714a391353815bde0db167da10f52897c62"
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #7 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;ALIC82W1N10=" hashed result is "00001411aec21d22a08dd5ee4f2b2d564deebfcf15d8a2ef6772bdd67a75c3c7"
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #8 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;dgQAHrC6osE=" hashed result is "000088867f3583556102a34141f0abb842bf1c775b6b480ab9704dca3e09e31b" 
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #9 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;6Tz7k5Qgo4M=" hashed result is "000078181c3f080f9f86aac29ae52603735b23cf9bb128c5ca2bc1481b09c904"
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #10 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;J5MW16765Qw=" hashed result is "000061721b1b97e1063afa7f44113ac11ed08a60a7e06b864fb7751a2bd69aa7" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #11 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;jofS9PhNZcA=" hashed result is "00008fcf330124609d14a2a259bef60e1924bf5a71319e9eaf51642e700ed9dd" 
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #12 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;OOUFOz+AfMQ=" hashed result is "0000d1c08e1841c22ddce85b4c415db53f687a394bd55e25edf1d5dbb69d4374" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #13 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;Ei2cO2IKcZ4=" hashed result is "00007a5c1833e4c5226e0c2e33b928db063dc393e308690a5bb8d35689e0c4a1" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #14 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;vUKxXtIsXOQ=" hashed result is "000013252da48c8f96bb131287cad96cec9a3669fe51486535f71eb57b65a5ad" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #15 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;TSdKlPKC3Bk=" hashed result is "00005a87ebc7b6091f84fd741005dd506dec1b32171f7fb985461d5c2ba10ea9" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #16 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;J27xrqfwlgE=" hashed result is "000061cf19d178cb9bfd3264ffb242f3adcf63c08312e05a77e9b5e1c500b3f1" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #17 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;aulNRL/3sNM=" hashed result is "00002bca860c0b40336cead868ec3482922c67f7e57f7a14037013f1edad2a7d" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #18 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;ovh1f43GuEw=" hashed result is "0000d8414053b37153c44368e6b001480d1f08e8db1bb84faa92a76b122e86f2" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #19 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;E2ZIUx14YRQ=" hashed result is "00004d6d9a92f98501ac861f58b28ecdba6d29099077abe55e11bea2226b4f86" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #20 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;QABmpsfKrIo=" hashed result is "0000ba00aa37e02435baa9dcdaea3a07812e4a81c7b272cd7c83d78be97b3fbc" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #21 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;ZULYd/fJ2K4=" hashed result is "0000f3743a22ca044c4eec141fbb0949caf7994ad6744c99f827d6665d3a1eb0" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #22 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;kus5POILiTE=" hashed result is "0000a969c9c15a76284a41986de21bfeeae4cc5634eb9f47c7f569e6574ad8e2" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #23 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;DAmurUyFZFo=" hashed result is "0000fd4c87695ce3e321f37117708673fff80870724cecf9b7afbf33dd9449d0" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #24 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;HP4ijtW23Uc=" hashed result is "00002b2ede4deb399bb04db85d0c4e38103c5db72516bde28a6b67b5d08bc0e6" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #25 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5693.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;Z3LXXp2jvx0=" hashed result is "0000a5ec1aeb941749f1c6bfb4847f04902a7e21243fa416bc13e8c7df6c6263" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #26 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;99tHS+R1xWA=" hashed result is "000021fbee2dd763f80b57ed46d3e7a379d340b113279c93d03f30e406e322c8" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #27 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;a4WB71Oitok=" hashed result is "0000785ebf1bfd15354e0f038156e04d57769309380cfd9d2238005a29d277d5" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #28 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5693.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;DEzqnBVUrWo=" hashed result is "0000c59234e8fc734c70da549f7992ab6d40d1767590da0bf9b327bca3000b56" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #29 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;RnmzREUfxII=" hashed result is "00005fae8633752565e8919241e8fa30e80031977af6075b51f867caa61a9b0d" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #30 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;yjMLKFa3e+o=" hashed result is "0000a4017ed1a72f88bead0845b041e95b579084512eac23b9c54e3946e4517b" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #31 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;DcVjykqXGPY=" hashed result is "00008060428adcd5c480dc5169ea949f2e098dda12e095efc3a50603e1b1a161"
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #32 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;gNYiu5Kb2wA=" hashed result is "00009e68b291d27efaa06c52f2012992cbb3ff8ad8805cb362a8a1333b030070" 
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #33 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;cRX4mtM+tlc=" hashed result is "00001b6deb506ce05bb4e898562ca62fee3411f169df985108e0015844a25b45" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #34 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;OqncYt3QrCg=" hashed result is "00002d6bb7a804a33aa024cd484a5c5683247be85e0e6a60da92754eafb43c2f" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #35 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;0PCE49iA9/I=" hashed result is "00000b8570dbecdc51d10be24d0d24cf832fb95ca23506dbbe1e06d06e0d5913" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #36 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;5Q0cPwaC6MY=" hashed result is "00009d784253bbdeaca6a93656465370d33357c7d3dee6ddefc55897e5eb17d5" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #37 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;Eqd1ojH+qGY=" hashed result is "000037fc0dcb19bc33a745f5627e5898e94ed63a9c4fdbae6dbe120e5667c56b" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #38 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;TYJ+o6iaMCM=" hashed result is "00007a1b7b47af9832e47226cc89f46b1d370f141bc66c2b052a22dc57769254" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #39 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;HRkeLqCkOro=" hashed result is "000062f66462e6f08d1deb763584b99e1b3d192d6a774e6a3b32460d00e07497" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #40 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;yZE3EhQsSuQ=" hashed result is "0000a972be397df3daefa189b82a3af174d903b82a2dfea4c8182eec29e4896c" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #41 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;WFv2YgzC9S4=" hashed result is "0000fece6b32263b24ddfac46e3863139690dbee658bcb972227618c7fbc1259" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #42 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;Mf45eGDqWlg=" hashed result is "0000aaa55395ff00a526df4508ec2569a7993f2590770e079c5f7b8e7cea4045" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #43 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;XEODpJIWswU=" hashed result is "0000c7be90bc3b7492c395836d0a8ce270c05c6adf661979917839b090887b73" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #44 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;pXuN6dguaLo=" hashed result is "0000404942ca60a0d2ecc094b4f2e8d200000af9f2f635ef188142faa102bb9f" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #45 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;8vBbQYO1F9Q=" hashed result is "00001ba155bd4d0be568b3659a739bb1acbb89816dd238a3c01722d44c297477" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #46 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;QE8oSWICl6k=" hashed result is "000085c3e609aa6f6efe4144a7cc1ec78887c78012f0c41acf4bc28bcde802eb" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #47 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;fFQC5mB4eCQ=" hashed result is "0000e563d8818c2cb24f1813d52c8d736605e0e00082eaee47e6193b6a93e194" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #48 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;hVbr/OPEw3g=" hashed result is "000030b78f289f5673dcc024409dc3cf3bd7535edad0324b440883e636526637" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #49 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;5rCASjt8vt0=" hashed result is "000004b2eba47262069f0c7653aea43a040ec4520eb7493ae4d7d4b4cff2e03a" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #50 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;S4DxiIrvp9g=" hashed result is "00008e9e0e2a329b4d36a9f2fbe5353468cb1de09a2705ff95ddadecdfea286c" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #51 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;F1ftIU4yWMg=" hashed result is "0000a845405f7728e6ee82775fe0f681bdc7acf82307ec307c11be638d22af06" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #52 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;43kZpOaM6dw=" hashed result is "0000f8e8ebdcdfd3358499f454b3e68dca7b3aa4d09562308364bf9e21e47f7a" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #53 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;EVqqJbeKqMs=" hashed result is "000059efa144c1b071aa4fad82782cec988ff3228b74b7b4ea831d1c6b16b6d6" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #54 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;FP4hi8xPqcc=" hashed result is "0000043b6677af468569175e132d93c757b75efb487b4c3b5ee9a4f5f1c3f877" 
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #55 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;SHaD/maIIsM=" hashed result is "000048107d9b823848356e1d4e541947605be01c128a96ed61d610020bc14c6f" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #56 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;DrgdlCTrVNk=" hashed result is "0000a3fcc1df1e460e863a7b74c5cd8dbf3fd1d2d13ea79055a1d596b6bf139a" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #57 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;nlFVJdMBoCU=" hashed result is "0000364fda7ab96b714597e9df6508d17389fa720455e2b69d0d63af09b1547c" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #58 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;f8N/akHOJxg=" hashed result is "0000498793ea46fda1febd9ca59afd00a2bb39ab0c3ac1ea83efeac042ea58de" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #59 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;s2DqfJ6F9oY=" hashed result is "0000287534d408f589c643a611262a0ec09eb93d3d098daa95c446faf349d7b6" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #60 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;0///slqXZCU=" hashed result is "000036c0376980a47d434870d2ead3a860f0dd2d65c8b6424eeb2e1aba90a011" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #61 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;kPJeHLIohLc=" hashed result is "0000916147cacb5b95b15a39a33c3d4295dc4711eb1b671dd9821b1df09f9997" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #62 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;X76F5kU75c8=" hashed result is "000054d64feafc5a7088b93809d7ec45afc5723283d1c80f280d98edfe223e02" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #63 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;mO9qs0OXW7s=" hashed result is "0000a00ba4735791c15c3dc3dca76c35c7ecc8367107580d734f5dddb6bf1b7b" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #64 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;lpvNK8lT/5k=" hashed result is "000089311d2afb46e9613507a11e6fe979cb338628a37018724481202d987479" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #65 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;jfmEgOv5JqU=" hashed result is "00001c9902368f1635fa8f77ba446427f8cfb16e2317b72113d66ee3005cebc3" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #66 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;5Kxm27gzBKw=" hashed result is "00006271ce75cb858cbb9e134ee6d8532824a472b5fec5f78c12e7bf0ab552ce" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #67 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;08vKaQ7dsC8=" hashed result is "00008fb5893d52e9676903f15a1eb901f917eb1ad17a353973def0bcbdd03ffa" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #68 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;clnKyltLZ68=" hashed result is "00009ea79fc35c13d046ea0054d4e69e7da1f2eeb81a26a7a6a74103ce8fa939" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #69 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;Kph0W2hA04c=" hashed result is "00002fdf54f256f578cef49e7c4847b1e5daf77fb315881a78b8666083ff624f" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #70 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;IE7ONUhEl1A=" hashed result is "00005d9386f8bf826412ec42acdbfa4a1eb0c25a5936854e0b60ed6b67c7fbea" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #71 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;hiplJKiGeHE=" hashed result is "0000cea54f74c856c1845979c2b6ebb3f527c9e2c17554ae55e0263cd829d97a" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #72 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;z2kNPb3cgOA=" hashed result is "00001988932e8044dbf87b3f8b0606689b21f6c6bbfdf7d72446dfc4b00d5687" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #73 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;sfx69qHH6CQ=" hashed result is "00009ed89b1c3af651b16954b00bd5891b5b92baf0140032a21150cd10509e3d" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #74 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;YKw/dxHEqA4=" hashed result is "00008e245075f00a84857f484922ef720270decd4fb13c7657d7d0b77043dd93" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #75 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;Rs1K2YKlkpA=" hashed result is "000037db603f2d39a5951628d7d0aa1ed85e1582a6e4e6093019246159895c84" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #76 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;3nxrcEQqvSw=" hashed result is "00009416858c91c7a73ef0b870f9bb1a839f886f48a5ff6e05ecf0426adf84e2" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #77 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;qyOUHiHT9gs=" hashed result is "0000eda68c253e194a4d27647d71d783d0f9fe37fe52ad271f24f7a185e3d3be" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #78 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;XdtaASqfjv0=" hashed result is "000080d61ded01e055605b32de7fb98c23c8d8e666743216680a2a653b76d38e" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #79 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;p/8PrSnruTY=" hashed result is "0000b4f8122ff4f6182cf0d7a745720894cc64c14f9ecc914fd2c4f38eda7a0d" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #80 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;PEUD5wYfq4I=" hashed result is "000025060fc2efb2b2075f580a8e5128bba713121ae592c5e4c209018ff4c324" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #81 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;2vNT4Sabfvk=" hashed result is "00004aeb66cdb1ea0e8875e6c3e7bd1d0cc86d4e6e3bcb6c1d324c083023561a" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #82 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;JBCo1AgQN3o=" hashed result is "000080234bc021cbe615b124c7878d1df419ae1bce4211c3d055e2727c19e7f3" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #83 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;6tjg2Yls9m8=" hashed result is "00002e008b71e36ebefe648613d18e327ac4e3ccb3c0f3dd5a8f18ed96c230a0" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #84 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;rbca8ysyd3M=" hashed result is "00009715f4a05762693f38fc39bba53c8181be49084ea17ba352f55aff062184" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #85 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;pk0biRu+WBE=" hashed result is "000023679a4b8f85fea32f4733ca4548b937bbc524076ced38575d73922fd9c3" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #86 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;XlVkzjmvHtc=" hashed result is "00005dc4e971a9900e1467b0dd639672a7da8429ce3256df6ef25cf959d9eabe" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #87 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;zcJmU0rs5r4=" hashed result is "0000cabd04d570a041f8a4d561d5d667f3e7e98ea9e1001cb9ebe255ba0fed90" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #88 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;KQhAGWHoSyw=" hashed result is "00005b8320a3f4c32cb78d2db987402b308b44800cfe3d2868b88d25d89b24bb" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #89 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;XY9+IW6JSsM=" hashed result is "00004e1d8d579008a9f3608f92e1524f676c6604134d67d5f8dc0ffb0c371dcd" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #90 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;eLTRRFmkx3g=" hashed result is "0000d737b2ebfb6160db76dc2c3a29f5e58be474012a6b52c7951a57338fa794" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #91 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;A6GDNMieWVU=" hashed result is "0000fbe87e65da5eead0b826221feec500cf3e863cf95ce522bc5341de75f6dd" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #92 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;C+2TDWV7elE=" hashed result is "0000ed92f4e62b24e800ad2f6eaf04523e827d1662470079453455af34ca9e8e" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #93 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;tAgQs7yxHAE=" hashed result is "0000be7f82ed1b2a294c2af65a6ea4abacb570b68107494fea0a3e5338b4b3f5" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #94 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;N1c4WPjUlwA=" hashed result is "00006fe0ffbd3db4c5c6d5327ef79c87d81d480929feea9760553489dea3e52c"
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #95 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;YvEXteVR614=" hashed result is "0000be29c7be3fd98ce27a2f0019d64a29b3d26b224a9ea03ebafb84e341a3f5" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #96 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;HeFJLPrzDgM=" hashed result is "000084bf2475d8655281afd3575922e80f49f55bd82029d92166a0fb7dc7d07a" 
(server@127.0.0.1)2> 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #97 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;uxMFzw7TDBk=" hashed result is "00006e985072e6dddb56ce7d242442d986fdce5d21bb90176beb89f925d1b8eb" 
(server@127.0.0.1)2>
(server@127.0.0.1)2> 
(server@127.0.0.1)2> 
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #98 Mined! 
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;/IZXiwmc1U4=" hashed result is "00005618d27b9fb881cd1f4cbceea4c822ca9c4d6c9f8b69f568718a11bebf73" 
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #99 Mined!
(server@127.0.0.1)2> Mined by Process PID : <8943.1188.0> (Worker from Other Node)
(server@127.0.0.1)2> Mined "lohit.bhambri;F6uhLDYPUgc=" hashed result is "00003240ab639df7f7dc35bc633d2144f1f28cae9aadf57aa57510916683733a"
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> ---------(server@127.0.0.1)2> Coin #100 Mined!
(server@127.0.0.1)2> Mined by Process PID : <0.5695.0> (Supervisor)
(server@127.0.0.1)2> Mined "lohit.bhambri;MY51Fyb98KE=" hashed result is "00001f6870517b7157ee838efdffcfad1b2ece68a3556aae04b4007f26d07cf4"
(server@127.0.0.1)2>
(server@127.0.0.1)2>
(server@127.0.0.1)2> Final Real Time is 70157 milliseconds
(server@127.0.0.1)2> Final CPU time is 322765 milliseconds
(server@127.0.0.1)2> The final ratio is 4.600610060293342
(server@127.0.0.1)2>
```

[//]: # (![Input]&#40;imgs_readme/2.1.png?raw=true "Input"&#41;)

The Worker Side (Laptop/Terminal 2): 


```aidl
(worker@127.0.0.1)1> f(Server_PID).                         
ok
(worker@127.0.0.1)2> Server_PID = <8850.5695.0>.            
<8850.5695.0>
(worker@127.0.0.1)3> worker:main_loop(join, 4, Server_PID).
Finished spawning processes 
Finished spawning processes 
Finished spawning processes 
...
...
Finished spawning processes ...

```



[//]: # (![Input]&#40;imgs_readme/2.1.png?raw=true "Output"&#41;)

3. Ratio of CPU time to Real Time for above execution

Final Real Time is 70157 milliseconds


Final CPU time is 322765 milliseconds


The final ratio is 4.600610060293342


4. Coin with the most 0s found

The maximum zeroes found were 6, the input and output is given below. 4 coinswere mined after allowing a runtime of ~5 minutes.

The Master Side (Laptop/Terminal 1):

```aidl
(server@127.0.0.1)2> server:start_process(8,6).
The local process ID for workers to connect : <0.7528.0>. 
Please note the above. Waiting for 15 seconds before starting mining. Workers may connect mid-way while mining. 
nothing
(server@127.0.0.1)3> 
(server@127.0.0.1)3> ---------(server@127.0.0.1)3> Coin #1 Mined! 
(server@127.0.0.1)3> Mined by Process PID : <8943.1576.0> (Worker from Other Node)
(server@127.0.0.1)3> Mined "lohit.bhambri;NrkgZp/pjfU=" hashed result is "00000070066b7d49f2510b1631acacca4c301bf865d368cf53ecf2a9d76f68c9"
(server@127.0.0.1)3>
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> ---------(server@127.0.0.1)3> Coin #2 Mined! 
(server@127.0.0.1)3> Mined by Process PID : <8943.1576.0> (Worker from Other Node)
(server@127.0.0.1)3> Mined "lohit.bhambri;bOWR7VsafAA=" hashed result is "000000d4dfc987701f2991c350bad8592dae3aab6fc99bf0332734bced3110f9" 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> ---------(server@127.0.0.1)3> Coin #3 Mined! 
(server@127.0.0.1)3> Mined by Process PID : <8943.1576.0> (Worker from Other Node)
(server@127.0.0.1)3> Mined "lohit.bhambri;l1v35rDPE8Y=" hashed result is "00000063cfe9535e14dc751a6661ba5850a9bc22f2a334d5dfc6a00602855284" 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> ---------(server@127.0.0.1)3> Coin #4 Mined! 
(server@127.0.0.1)3> Mined by Process PID : <8943.1576.0> (Worker from Other Node)
(server@127.0.0.1)3> Mined "lohit.bhambri;ywjdsW2fFYw=" hashed result is "00000047ed07ec43ee51f31a9cab507cc173a622a65c0acd5fe3ac9c3c97210b" 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> ^G.
Eshell V13.0.4  (abort with ^G)
```
The Worker Side (Laptop/Terminal 2):

```aidl
Server_PID = <8850.7528.0>.            
<8850.7528.0>
(worker@127.0.0.1)2> worker:main_loop(join, 6, Server_PID). 
Finished spawning processes 
Finished spawning processes 
Finished spawning processes 
Finished spawning processes 
Finished spawning processes 
^G.
Eshell V13.0.4  (abort with ^G)

```

[//]: # (![Input]&#40;imgs_readme/4.1.png?raw=true "Output"&#41;)

5. Largest number of working machines achieved

During the trials, 2 machines (laptops) could be used for executing the code.
The code was tested to work for 1 master and 4 workers. However, the number of machines and workers can be extended based on need.
The master was tested to process requests of more than 10 million coins. This task would be shared among the participating workers.

## How to Run Project

View video demo here : https://drive.google.com/drive/folders/1qy1VlTM0ePqpxJfcZJI5_U1YoQUe3vA_?usp=sharing
or follow instructions below:

1. Open two terminals in the src folder of this project.
2. On one of the terminals (say Master terminal), enter the below commands:
```
erl -name server@127.0.0.1
```
3. On the other terminal (say worker terminal), enter the below commands:
```
erl -name worker@127.0.0.1
net_kernel:connect_node('server@127.0.0.1').
register(worker,self()).

```
4. On Master Terminal:
```
{worker, 'worker@127.0.0.1'} ! self().
```
5. On Worker Terminal:
```
flush().
```
Note down the first number among the three in PID. This is the node number.

eg. if flush() returns <9030.85.0>, Node# is 9030.

6. On Master Terminal:
```
server:start_process(20,4).
```
This means generate 20 coins with 4 leading zeroes. After this command generates output, note down the second and third parts of local process ID for workers to connect.

eg. if the pid displayed is <0.85.3>, note down "85" as PID2 and "3" as PID3.
7. On Worker Terminal:

Set Server PID as : <Node#, PID2, PID3>, which have been got from previous steps  
In our case, it is <9030.85.3>
```aidl
f(Server_PID).
Server_PID = <ENTER_HERE>.
worker:main_loop(join, 4, Server_PID).
```