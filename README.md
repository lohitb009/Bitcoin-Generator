
## <u>Distributed Bitcoin Mining Using Erlang</u>
##### <i>Lohit Bhambri(lohit.bhambri@ufl.edu)</i> 
##### <i>Imthiaz Hussain (imthiazh.hussain@ufl.edu)</i>


The goal of this project is to use Erlang and
the Actor Model to build a distributed solution to
'mine bitcoins'. The definition of 'mining a coin' is different in the context of this project. The problem statement is to find a set of strings, when hashed with SHA-256, have the required number of zero's as prefix. The first part of the string is pre-defined as the username of the user.

[//]: # (## Architecture Diagram)

[//]: # ()
[//]: # (![Alt text]&#40;imgs_readme/Arch.png?raw=true "Architecture"&#41;)



## Execution Details

1. Size of the work unit that resulted in the best performance:

For 100 coins to mine, various work unit values were given and tested.The size of work unit that achieved maximum performance was any value above 5. The performance characteristic used was the ratio of CPU time to real time.
 Finally, a work unit of 8 was selected as the machines used for testing are provided with 8 processors. When a worker was tasked with 8 coins to mine, it would spawn 8 miner processes. Hence, matching the processors and the work unit  would enable a complete and efficient use of CPU resources (without any queueing delays at the processors). This process parallellism adds on to the efficiency of distributed mining. Once a worker in the other machine/terminal finishes the task, it is assigned another work unit of tasks.

![Alt text](imgs_readme/1.1.png?raw=true "Architecture")




2. Result of running the program for input 4

The Master Side (Laptop/Terminal 1):
Note: Input Arguments (30,4) user input suggests a task of 30 coins having 4 leading zeroes.
The "Mined by Process PID" can be used to see if coin came from master or worker. The worker's and master's PIDs are printed in their respective terminal.

```aidl
(server@127.0.0.1)8> server:start_process(30,4).
The local process ID for workers to connect : <0.2324.0>. 
The PID of Master is : <0.1309.0>.
Please note the above. Waiting for 25 seconds before starting mining. Workers may connect mid-way while mining.
nothing
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #1 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.1309.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;4VNgnU4E5hE=" hashed result is "000005bcd96aa3c30985d6c5d5a9c114a6a9089e82b8aa7133fabbb7d953038a" 
(server@127.0.0.1)9>
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #2 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;jkd9gXlc4Oo=" hashed result is "0000ed70b73fb6499186e450e8ea7076bff124fc84a72ab89e35efb11d91bb2a"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #3 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;5yN/xVl8uM8=" hashed result is "00001548297fc4233ca28393dac2e0eb904ea286dcb05de0f0fc002576fe2e46"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #4 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;GTJhdjsB7tI=" hashed result is "00005cee7c23d08428967ea6dae761d9de1d77bcf26403ee9d24831cc8c2f6ed" 
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #5 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;o/AW7Q2z9Gk=" hashed result is "00004d0ccbeac545c89a70e6dd942164a377eb0ae0c7d603381912578c620c82"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #6 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;yjPPlFnrF2o=" hashed result is "0000cdc44a2b0f11e6ff404638d5755646c73d544fbbeab1a88be3a258fdecce" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #7 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.1309.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;rBuX9AjPO4s=" hashed result is "000022c4129c107d89aeb1ac848dd6984d68ac6e609fcb6dec9773ecfe0bcc02" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #8 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;TSxdp7w0QmY=" hashed result is "000018b9f12621a36fb02e7ee8fb4de19087633cd1713824397e3201029e9059" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #9 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;7W6L1S+IOQE=" hashed result is "00007a07b0e8709101a40699f6744d78ce74ad16f2a7bb81cbeaf16a55b557e1" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #10 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;ZcZxWGkF0SQ=" hashed result is "0000fa030210c3268453eae97de0b4c5dc24ea9a402e4ed2faef697345d18bbe" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #11 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;oeSGubJoOK0=" hashed result is "000094831a8201a478d78f98b7cfbe4cce8b94a0fe64fbac01d49b9abb75a854" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #12 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;7csdQOuu9UU=" hashed result is "0000676754cfde9a7a1ace6b0b1cb4ff07ef254b32f9521faba3b5adc0214661" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #13 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;y8ab60+EFd0=" hashed result is "0000d7bb63b9c418f7707e497ab2fbda35e3baff9e1041a648292d2461748017" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #14 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8943.362.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;5jjMn9mu9eU=" hashed result is "0000bab7bde271ab028710ac549ea3e740b00532f3ee366cee8c337df8b54224" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #15 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8943.362.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;YmtjzezroOg=" hashed result is "0000cb6b87f9b22ed178baa392cb7b5188c74c4d18c32a36aa74525925af4b77" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #16 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;KAaOhLIZOc8=" hashed result is "0000e8810fdbd7e5f21ce12546264c01e7e50c8dd1dfa326e553ea45054b687e" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #17 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;2lYY85byyYg=" hashed result is "000019cf3d2bf7fce09c36f6a9a944306427e3814502db4ad684c886ab7900e8" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #18 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8943.362.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;R0D3WYNYA8s=" hashed result is "0000bf7da95eda0c44c51eb018c98bc608f6984b37e547bb57b095db9fa44163" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #19 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;C9ugctK7Bik=" hashed result is "000018e579164bf23fc9603fff6e6c56f82441424efd79c0ab23992764d8fefd" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #20 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.1309.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;y9a4KTFthkI=" hashed result is "0000cc2a285fab0c175c40dd32db5d92e29e6765b85d7e5e31991055f6eef4c9" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #21 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8943.362.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;PJCv6EnjfpM=" hashed result is "0000af5fba3c790269a4c86ca58d3fce54eaba1ce09eb3cc8f8ab027e2e96a6a" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #22 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;PUg5r5SwrRg=" hashed result is "0000593e430dd31ea8a9bb3642646aecf2d539ace2446fa29ee1b369dfa834dd" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #23 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;booj8QHRCTo=" hashed result is "0000641c1eaec5ae38c2aec998e423b1c04e5417d62a40879c9eb4ebab72f0d9" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> 
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #24 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8943.362.0> 
(server@127.0.0.1)9> Mined "lohit.bhambri;sty/Hd/EsY0=" hashed result is "0000088f437dcbe7c0a0b51e658ea9e37c1253cc5ad13ccecfa293cc31fa8740" 
(server@127.0.0.1)9> 
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #25 Mined!
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;4UosamLsVkY=" hashed result is "00009222ded9d1eafba67cd843c11d3d82bd7d8fa4dfdf8cf5c57f0f5a133d92"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #26 Mined!
(server@127.0.0.1)9> Mined by Process PID : <0.2324.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;ajpK82e6n/I=" hashed result is "00006b18d5b4a6735940a10fd2f78a1e95cf7f3cef66235dce8d604fbc32b816"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #27 Mined!
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;+9URLXA825g=" hashed result is "00006d7e7a7ec0041cb0e2c936e89daf75e6e127d4e05172d228487df2e31844"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #28 Mined! 
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;G5lt4C5pD9s=" hashed result is "00001fd5e2ca72f1d6ea1ad524dc7d22e1d952c9b7521bab44b6d1d1d9fa3798"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #29 Mined!
(server@127.0.0.1)9> Mined by Process PID : <8944.606.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;gbskXSYq1Y8=" hashed result is "000066cf9c80785bab8da527a362f178be6bc7fa43e16c308af7d05321fbf8e7"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> ---------(server@127.0.0.1)9> Coin #30 Mined!
(server@127.0.0.1)9> Mined by Process PID : <8943.362.0>
(server@127.0.0.1)9> Mined "lohit.bhambri;XLVr/3aNEEY=" hashed result is "0000aac1c871a5da0b9a91139e8e926b770f013554a29cb34af8cbcb79551309"
(server@127.0.0.1)9>
(server@127.0.0.1)9>
(server@127.0.0.1)9> Final Real Time is 8717 milliseconds
(server@127.0.0.1)9> Final CPU time is 29750 milliseconds
(server@127.0.0.1)9> The final ratio is 3.412871400711254

```

[//]: # (![Input]&#40;imgs_readme/2.1.png?raw=true "Input"&#41;)

The Worker_1 Side (Laptop/Terminal 2): 


```aidl
(worker2@127.0.0.1)2> worker:main_loop(join, 4, Server_PID).  
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
Self PID is : <0.606.0> 
^G.
Eshell V13.0.4  (abort with ^G)


```

The Worker_2 Side (Laptop/Terminal 3):

```aidl
(worker@127.0.0.1)2> worker:main_loop(join, 4, Server_PID).
Self PID is : <0.362.0> 
Self PID is : <0.362.0> 
Self PID is : <0.362.0> 
Self PID is : <0.362.0> 
Self PID is : <0.362.0> 
Self PID is : <0.362.0> 
Self PID is : <0.362.0> 
^G.
Eshell V13.0.4  (abort with ^G)

```



[//]: # (![Input]&#40;imgs_readme/2.1.png?raw=true "Output"&#41;)

3. Ratio of CPU time to Real Time for above execution

Final Real Time is 8717 milliseconds

Final CPU time is 29750 milliseconds

The final ratio is 3.412871400711254

However, in some executions with varying inputs (eg. 1 or 2 leading zeroes), ratios around 7 were achieved.


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
(server@127.0.0.1)3> Mined by Process PID : <8943.1576.0>
(server@127.0.0.1)3> Mined "lohit.bhambri;NrkgZp/pjfU=" hashed result is "00000070066b7d49f2510b1631acacca4c301bf865d368cf53ecf2a9d76f68c9"
(server@127.0.0.1)3>
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> ---------(server@127.0.0.1)3> Coin #2 Mined! 
(server@127.0.0.1)3> Mined by Process PID : <8943.1576.0>
(server@127.0.0.1)3> Mined "lohit.bhambri;bOWR7VsafAA=" hashed result is "000000d4dfc987701f2991c350bad8592dae3aab6fc99bf0332734bced3110f9" 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> ---------(server@127.0.0.1)3> Coin #3 Mined! 
(server@127.0.0.1)3> Mined by Process PID : <8943.1576.0>
(server@127.0.0.1)3> Mined "lohit.bhambri;l1v35rDPE8Y=" hashed result is "00000063cfe9535e14dc751a6661ba5850a9bc22f2a334d5dfc6a00602855284" 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> 
(server@127.0.0.1)3> ---------(server@127.0.0.1)3> Coin #4 Mined! 
(server@127.0.0.1)3> Mined by Process PID : <8943.1576.0>
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
Self PID is : <0.1576.0>
^G.
Eshell V13.0.4  (abort with ^G)

```

[//]: # (![Input]&#40;imgs_readme/4.1.png?raw=true "Output"&#41;)

5. Largest number of working machines achieved

During the trials, 2 machines (laptops on same network) could be used for executing the code.
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