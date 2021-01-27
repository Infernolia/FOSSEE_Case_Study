import java.util.*;

public class Exception_2
{
public static void main(String args[])throws Exception,Throwable
{
fun();


}

static void fun() throws Exception,Throwable,RuntimeException
{
	Scanner sc = new Scanner(System.in);
	int x;
	x = sc.nextInt();
	if(x>0)
	{
		throw new ArithmeticException();
	}
	fun();
}
}