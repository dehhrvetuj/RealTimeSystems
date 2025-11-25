Part 1:

1. The different between relative delay and absolute delay:
Relative delay means that the program needs to wait an amount of seconds from the moment the line is executed. If the program is late or delays each cycle, the delay will be added on top of that lateness. Each cycle now becomes slightly longer that it is intended to be, which causes drift.
Absolute delay means that the program has to wait until the clock reaches the specific time. In this case, even if the program runs slowly for one cycle, the next cycle will always go back to the correct time which prevents drift. 
2. If we know the exact execution times of F1, F2, and F3, relative delay cannot avoid drift. This is because relative delays still accumulate small errors, such as OS scheduling, floating point rounding, timer resolution or interrupts and CPU delays. Each small error gets added to the next cycle, and the start time will slightly shifts from the ideal scheduler.

Part 2:
1. To synchronize the watchdog task with the start and the end of F3’s execution we used two entries Start and Done inside the watchdog task. So that before F3 begins, the main program calls Watchdog.Start, which tells the watch dog to begin monitoring the time. And when F3 finishes, the program calls Watchdog.Done, which tells the watchdog that F3 has already ended. 
2. To monitor the execution time of F3, inside the watchdog task, we used a select statement with 2 alternatives: 
If F3 finished before the deadline, accept Done is called
If the deadline passes before F3 finishes, delay F3_Deadline is called.
If the delay completes first, the watchdog prints a warning message and sets the flag to True, so that F3 misses its deadline, but the watchdog still wait for the task to be Done, so F3 is always allowed to finish. 

3. When F3 misses its deadline, the cyclic scheduler resets its timing base:
It sets Start_Time := Clock; and 
It resets Step_count := 0;
      This forces the next F1 to start exactly at a clean new second. By resetting the time base, the system will get back to the correct timing after F3 is late. 



Part 3：Process Communication
1. Show by a concrete scenario that the producer-consumer-buffer program using blocking rendezvous communication can have deadlocks and explain the mistake that can cause it.





