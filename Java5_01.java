import java.util.*;

class A
{
	
	public int id;
	public void show()
	{
		id = 3;
		System.out.println(" "+  id);

	}
	
	
}

class B extends A
{
	public int bd;

	public void show(int x)
		{
			bd = x;
			System.out.println(" "+  bd);
			
		}

}



class Demo
		{
			public static void main(String args[])
				{
					B obj = new B();
					obj.show();
					obj.show(6);
				}
						
		}