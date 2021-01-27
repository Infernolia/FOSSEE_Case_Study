import java.util.*;

abstract class A
{
abstract public void fun();	
public void c()
{

System.out.println("aBO");
}
	
}

class B extends A
{

public void fun()
{
System.out.println("Hi");

}

public void show()
{
super.c();

}
}





class Demo
		{
			public static void main(String args[])
				{
					
					B x = new B();
					x.fun();
				x.show();	
					A y;
					y = new B();
					y.fun();	
					//y.show();				

					
				}
						
		}