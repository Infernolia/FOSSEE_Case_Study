import java.util.Comparator;

public class sortByArrival implements Comparator<Process> {
		
	public int compare(Process p1, Process p2)
	{
		return p1.at - p2.at;
	}

}
