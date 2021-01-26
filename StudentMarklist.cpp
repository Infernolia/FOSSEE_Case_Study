//============================================================================
// Name        : StudentMarklist.cpp
// Author      : Roll no.21301.Aboli Marathe
// Version     :
// Copyright   : Student Marklist
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
#include <string>

using namespace std;

class stu
	{
		int n;
		string name[20];
		int mks[100];
		int presentee[100];


		public:
		void input();
		void max();
		void min();
		void average();
		void mostfrequent();
		void attendance();
	};


int main()
{
			stu x;
			x.input();
			int choice = 0;
			do {
						cout << "\n Choose the operation to be performed: 1 for Maximum Score, 2 for Minimum Score, 3 for Average Score, 4 for most frequent score , 5 for class attendance and 6 to exit: ";
						cin >> choice;

						switch (choice)
						{
								case 1:x.max(); break;
								case 2:	x.min(); break;
								case 3:	x.average(); break;
								case 4:	x.mostfrequent(); break;
								case 5:	x.attendance(); break;
								default:"\n Incorrect choice! Please try again.";break;
						}
			} while (1);

	return 0;
}







void stu::input()
{
			int i = 0;
			cout << "\n Enter the number of students in the class :";
			cin >> n;

			for (i = 0; i < n; i++)
			{
						cout << "\n Enter the name, marks of the student and attendance(0 or 1) : ";

						cin>>name[i];
							cin >> mks[i];
						cin >> presentee[i];

						if((presentee[i]==0)&&(mks[i]>0))
							{
									cout<<"\n\n Invalid data! Absent student cannot have marks";
									mks[i]=0;
							}

			}
}


void stu::max()
{
	int i = 0; int b = 0; int c = 0;
	for (i = 0; i < n; i++)
	{
		if (mks[i] > b)
			b = mks[i];
	}
	for (i = 0; i < n; i++)
	{
		if (mks[i] == b)
			c=i;
	}
	cout << "\n The highest score is " << b <<" of "<<name[c];
}

void stu::min()
{
	int i = 0; int b = 10000; int c = 0;
	for (i = 0; i < n; i++)
	{
		if (b > mks[i])
			b = mks[i];
	}
	for (i = 0; i < n; i++)
	{
		if (mks[i] == b)
			c = i;
	}
	cout << "\n The lowest score is " << b << " of " << name[c];
}

void stu::average()
{
	int i = 0; int sum = 0;int counter=0;
	for (i = 0; i < n; i++)
	{
		if(presentee[i]==0)
		{
			counter++;
		}
		sum = sum + mks[i];
	}
	int avg = 0;
	avg = sum / (n-counter);
	cout << "\n The average score is " << avg;
}

void stu::attendance()
{
	int i = 0; int a = 0;
	for (i = 0; i < n; i++)
	{
		if (presentee[i] == 0)
			a++;

		if(presentee[i]!=0 && presentee[i]!=1)
			cout<<"\n\n Attendance must be 0 or 1";
	}


	cout << "\n The students absent for test are :" << a;
}





void  stu::mostfrequent()
{

			int b, freq[100], i, j = 0; int k = 0;
			int mod = 0;

			for (i = 0; i < n; i++)
			{
				freq[i] = 0;

			}


			for (i = 0; i < n; i++)
			{
				b = mks[i];
				for (j = 0; j < n; j++)
				{
					if (mks[j] == b)
					{
						freq[i]++;
					}

				}

			}

			for (i = 0; i < n; i++)
			{
				if (freq[i] > mod)
					mod = freq[i];



			}
			int s = 0;

			for (i = 0; i < n; i++)
			{
				if (mod == freq[i])
					s = i;

			}

			cout << "\n The most frequent score is : " << mks[s];
}
