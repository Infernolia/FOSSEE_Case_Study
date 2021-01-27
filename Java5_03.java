import java.util.*;

class A
{
	
	public static int id;
	A(int x)
	{

	id =x;
	
	}
	
	public static void show()
		{
			
			System.out.println(" "+  id);
			
		}
	
	
}

class B extends A
{
	public static int bd =7;
	B()
	{
		super(bd);                                                         
	}


}



class Demo
		{
			public static void main(String args[])
				{
					
					B x = new B();
					x.show();

					
				}
						
		}