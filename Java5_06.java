import java.util.*;

interface intr
{
	 int x =0;
	
	public void mousePressed();
	interface intr1 
		{
			int y =7;
	
			public void mousePressed();


		}

	
	

}




class Myclass implements intr,intr1
{
	
	
	public void mousePressed()
	{
		System.out.println(" hey" +y);

	}
	
	
	


}

class Demo
		{
			public static void main(String args[])
				{
					
					Myclass a = new Myclass();
					a.mousePressed();
					a.mouseReleased();		

					
				}
						
		}


