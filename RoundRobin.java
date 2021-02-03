import java.util.Arrays;
import java.util.Scanner;

public class RoundRobin {

	static Scanner sc = new Scanner(System.in);
	

	
	
	
	static void  RoundRobinSch(Process proc[], int n, int quantum)
	{
		int wt[] = new int[n];
		int tat[] = new int[n];
		int CT[] = new int[n];
		int rem_bt[] = new int[n];
		
		int totwt = 0, tot_tat = 0;
		
		
		
		Arrays.sort(proc, new sortByArrival());
		
		
		for(int i =0 ; i < n; i++)
		{
			rem_bt[i] = proc[i].bt;
		}
		
		
		int t = 0;
		
		System.out.println("Processes " + " Arrival Time " +  " Burst time " + " Waiting time " + " Turn around time" + "Completion Time");
			
		int complete = 0;
		while(complete != n)
		{
			
			for(int i = 0; i < n; i++)
			{
				if(rem_bt[i] > 0 && proc[i].at <= t)
				{
					
					
					if(rem_bt[i] > quantum)
					{
						t += quantum;
						rem_bt[i] -= quantum;
						
						CT[i] = t;
						
						tat[i] = CT[i] - proc[i].at;
						wt[i] = tat[i] - proc[i].bt;
						
						System.out.println(" " + proc[i].pid + "\t\t" + proc[i].at + "\t\t"
				                  + proc[i].bt + "\t\t " + wt[i] 
				                  + "\t\t" + tat[i] + "\t\t" + CT[i]);
						
					}
					else // now process will finish 
					{
						t += rem_bt[i];
						
						rem_bt[i] = 0;
						CT[i] = t;
						
						tat[i] = CT[i] - proc[i].at;
						wt[i] = tat[i] - proc[i].bt;
						
						/*
						 *   OR 
						 *   tat[i] = CT[i];
						 *   wat[i] = tat[i] - proc[i].bt - proc[i].at
						 * 
						 * */
						
						complete += 1;
						
						System.out.println(" " + proc[i].pid + "\t\t" + proc[i].at + "\t\t"
				                  + proc[i].bt + "\t\t " + wt[i] 
				                  + "\t\t" + tat[i] + "\t\t" + CT[i]); 
						
					}
				}
			}
			
		}
	
		
		
		
		
		
		
		
		for (int i = 0; i < n; i++) 
		{ 
			 totwt = totwt + wt[i]; 
			 tot_tat = tot_tat + tat[i]; 
			 /*System.out.println(" " + proc[i].pid + "\t\t" + proc[i].at + "\t\t"
			                  + proc[i].bt + "\t\t " + wt[i] 
			                  + "\t\t" + tat[i] + "\t\t" + CT[i]); */
		}

		
		System.out.println("Average waiting time = " + 
		               (float)totwt / (float)n); 
		System.out.println("Average turn around time = " + 
		                (float)tot_tat / (float)n);		
	}
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub

		
		int quantum = 2;
		int n;
		System.out.println("Enter number of processes:");
		 n = sc.nextInt();
		
		
		Process proc[] = new Process[n];
		 
		for(int i = 0; i < n; i++)
		{
			System.out.println("Enter Arrival time for process " + i + 1);
			int at = sc.nextInt();
			
			
			System.out.println("Enter Burst time for process " + i + 1);
			int bt = sc.nextInt();
			
			proc[i] = new Process(i+1,at, bt);
			
		}
		
		RoundRobinSch(proc, n, quantum);
		
	}

}
