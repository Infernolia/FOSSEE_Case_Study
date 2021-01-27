import java.util.*;
class Demo
		{
			public static void main(String args[]) 
				{
				Scanner sc = new Scanner(System.in);
				
					
								try
								{
								fun();
								}
								catch(NullPointerException e)
								{
								System.out.println("\n\n Array except found, enter +ve no");

								}
								

				
					
					
				}
			static void fun()
			{

				try
				{
					fun2();

				}
				catch(NullPointerException e)
								{
								System.out.println("\n\n Null ptr except found, enter +ve no");

								}
								
	
			}

			static void fun2()
			{
				try
				{
					int arr[] = new int[5];
					System.out.println(arr[6]);
				}
				catch(ArithmeticException e)
				{
					System.out.println("\n\n Arithmetic except found, enter +ve no");
				}
				

			}




			
						
		}
