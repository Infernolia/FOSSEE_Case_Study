import java.util.*;

class A
{
	
	public static int id;
	A()
	{

	id =7;
	
	}
	
	
}

class B extends A
{
	public int bd;

	public static void show()
		{
			
			System.out.println(" "+  id);
			
		}

}



class Demo
		{
			public static void main(String args[])
				{
					B obj = new B();
					obj.show();
					
				}
						
		}