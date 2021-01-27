import java.util.*;




class MyException extends Throwable
{

int errorcode;

public MyException(int x)
{
	errorcode = x;


}

public String toString()
{
	return "Hi"+ errorcode;


}

}
class Demo
		{
			public static void main(String args[]) throws MyException
				{
				Scanner sc = new Scanner(System.in);
				
					
								try
								{
								MyException obj = new MyException(6);
								throw obj;
								}
								catch(MyException e)
								{
								System.out.println("\n\n My except found " +e);

								}
								

				
					
					
				}
		



			
						
		}
