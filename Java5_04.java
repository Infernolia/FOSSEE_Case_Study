import java.util.*;

class A
{
	
	public int id;
	A()
	{

		id = 4;
		System.out.println(" in  con A");
	}
	
	
		{
			
			System.out.println(" in  anonymous A");
			
		}

		static
		{
			
			System.out.println(" in static A");
			
		}
	
	
}

class B extends A
{
	public  int bd;
	B()
	{
		bd = 8;       
		System.out.println(" in  con  B");                                                 
	}

	{
			
			System.out.println(" in  anonymous B");
			
		}

		static
		{
			
			System.out.println(" in static B");
			
		}


}


class C extends B
{
	public  int cd;
	C()
	{
		cd = 7;    
		System.out.println(" in  con C");                                                     
	}

	{
			
			System.out.println(" in  anonymous C");
			
		}

		static
		{
			
			System.out.println(" in static  C");
			
		}


}



class Demo
		{
			public static void main(String args[])
				{
					
					 C x = new C();
					

					
				}
						
		}