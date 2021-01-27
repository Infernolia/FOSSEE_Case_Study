import java.util.*;
class Demo
		{
			public static void main(String args[]) 
				{
				Scanner sc = new Scanner(System.in);
				
					while(true)
					{
								int x = sc.nextInt();
								if(x>0)
								{
									break;
								}
								else
								{
								try
								{
								ArithmeticException a = new ArithmeticException();
								throw a;
								}
								catch(ArithmeticException e)
								{
								System.out.println("\n\n Artih except found, enter +ve no");

								}
								}

					}
					
				
					
					
				}




			
						
		}


