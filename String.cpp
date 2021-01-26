//============================================================================
// Name        : String.cpp
// Author      : Aboli
// Version     :
// Copyright   : 
// Description : Hello World in C++, Ansi-style
//============================================================================


#include <iostream>
#include<string>
using namespace std;

class alpha
{
char ori[50];
int n;


public:
void input();
void copy();
void concate();
void subst();
void eq();
void reve();
void leng();
void freq();
void remnew();
void chardel();
void replace();
void pali();

};


int main() {

	alpha x;
	x.input();
	int cur=0;
	int choice=0;
	int minchoice=0;


	do
	{
	cout<<"\n ENter the operation you wish to perform : 1 for basic string ops,2 for frequency of a character,3 for removal of a string from original,4 for removing all occurences of a particular character,5 for replacing a string by new string of any length,6 for finding if original string is plaindrome or not :";
	cin>>choice;
	switch(choice)
	{
	case 1:cout<<"\n ENter the basic operation you wish to perform : 1 for copy,2 for concatenate,3 for substring,4 for equality check,5 for reversing a string,6 for finding the length of the string  :";
	       cin>>minchoice;
	       switch(minchoice)
	       {
	       case 1:x.copy(); break;

	       		case 2:x.concate(); break;

	       		case 3:x.subst(); break;

	       		case 4:x.eq(); break;

	       		case 5:x.reve(); break;

	       		case 6:x.leng(); break;

	           	default:cout << "\n Invalid operator ";break;


	       }break;
	       case 2:x.freq(); break;
	       	case 3:x.remnew(); break;
	       	case 4:x.chardel(); break;
	       	case 5:x.replace(); break;
	       	case 6:x.pali(); break;
	       	default:cout << "\n Invalid operator ";break;


	}cout<<"\n\n Do you wish to continue :";cin>>cur;

	}while(cur==1);
	return 0;
}


void alpha::input()
{
	cout << "\n Enter the original string :";
	cin >> ori;
	int i = 0;int count=0;
	for (i = 0; ori[i] !=NULL; i++)
	{
		count++;
	}
	n=count;
}


void alpha::copy()
{
	int i;
	char b[100];
	for (i = 0; i <=n; i++)
	{
		b[i] = ori[i];
	}
		cout <<"\n The copied string is :" <<b;
}

void alpha::concate()
{
	int i;

	 char sec[50];
	cout << "\n Enter the second string :";
	cin >> sec;

	for (i = 0; sec[i] != NULL; i++)
	{
		ori[n + i] = sec[i];
	}

	   	 	cout << "THe concatenated string is :" <<ori;
}

void alpha::subst()
{
	int i; int a;
	int flag1 = 0;
	int flag2 = 0;int count=0;
	char s2[50];


	cout << "\n Enter the  string to be searched:";
	cin >> s2;
	int select;

	for(i=0;s2[i]!=NULL;i++)
		{
			count++;

		}

	for (a = 0; a < n; a++)
	{
		if (ori[a] == s2[0])
		{

			for (i = 0; i<count; i++)
			{
				if(ori[a+i]!=s2[i])
							 break;
			}
			if(flag1==count)
				select=a;
		}

/*
 for(i=0;s2[i]!='\0';i++)
	 n2=i;
 for(i=0;i<n;i++)
 {
	 if(s1[i]==s2[0])
	  for(j=1;j<n2;j++)
		 if(s1[i+j]!=s2[j])
			 break;
	 if(j==n2)
	 {
		cout<<"Substring";
        break;
	 }
 }
 if(i==n)*/

	}
	cout<<"\n\n Check "<< select;

	if (flag1 == count)

		cout << "\n The given substring is present at position : " << select +1;
	else
	cout << "\n Given substring is absent";

}

void alpha::eq()
{
	int i;
	int flag = 0;
	char sec[50];
	cout << "\n Enter the second string :";
	cin >> sec;

	for (i = 0; sec[i] != NULL; i++)
	{
		if (ori[i] != sec[i])
			flag = 1;
	}
	if (flag == 1)
		cout << "\nTHe strings are not equal ";
	else
		cout << "\nThe strings are equal";
}

void alpha::reve()
{
	int i; int j;

	char sec[50];


	for (i=n-1,j=0;i>=0;i--,j++)
	{
		sec[j] = ori[i];
	}
	sec[n]='\0';
	cout << "\n THe reversed string is  : " << sec;
}
void alpha::leng()
{
	cout << n;
}


void alpha::freq()
{
	char fre = ' '; int count = 0;
	cout << "\n Enter the character you wish to test";
	cin >> fre;
	int i;

	for (i = 0; i < n; i++)
	{
		if (ori[i] == fre)
			count++;

	}

	cout << "\n The frequency is "<<count;


}



void alpha::remnew()
{
	int start = 0; int len = 0; char fin[90];
	cout << "\n Enter the index character you wish to start at";
	cin >> start;
	cout << "\n Enter the length";
	cin >> len;
	int i; int a; int b;

	for (a = 0,b=0; a < n; a++)
	{
		if (a == start)
		{
			a += len;
		}
		fin[b] = ori[a];
		b++;

	}
	cout << "\n The final string  is " << fin;


}


void alpha::chardel()
{
	char remove = ' ';
   char fin[50];
	cout << "\n Enter the  character you wish to delete";
	cin >> remove;
	int b;int x=0;
	int i; int a;

	for (a = 0,i=0; a < n; a++)
	{


			if (ori[a] != remove)
		 {
				fin[i]=ori[a];
			i++;}

	}
	fin[a+1]='\0';

	cout << "\n The final string  is " << fin;


}

void alpha::replace()
{
	char remove[50];
	char replac[50];

	int c=0;

   char fin[50];char final[50];
	cout << "\n Enter the string you wish to delete";
	cin >> remove;
	cout << "\n Enter the string you wish to replace it with";
	cin >> replac;

	int b=0;int x=0;int n1=0;int count2=0;
	int i=0; int a;int flag1=0;
	int flag2=0;int flag3=0;
	int count=0;int n2=0;int count3=0;
	int extra=0;int start=0;int n3=0;


	for (i = 0; remove[i]!=NULL; i++)
		{
			count++;
		}

	n1=count;



	for (i = 0; replac[i] !=NULL; i++)
			{
				count2++;
			}
	n2=count2;



	for (a = 0; a < n; a++)
	{
		flag1=0;
		if(ori[a]==remove[0])
			{
			for(i=0;i<n1;i++)
				{
				if(ori[a+i]!=remove[i])
					flag1++;
				else
					flag1+=0;
				}


			if(flag1==0)
			{		cout<<"\n THe string to be removed is present ";
			flag2=a;
					a+= n1;
			}

			else
			{
				a+=0;

			}
			}

		fin[b]=ori[a];
		b++;

	}
	fin[b+1]='\0';

 cout<<"The deleted string is"<<fin;

	for (i = 0; fin[i] != NULL; i++)
			{
				count3++;
			}
	n3=count3;


 for(a=0,b=0;a<n;a++)
 {
	 if(a==flag2)
	 {
		 for(i=0;i<n2;i++)
		 {
			 final[a+i]=replac[i];
		 }
		 a+=n2;
		 b+=n2;
	 }


	 else
	 {
		 final[b]=fin[a];
		 b++;
	 }



 }


}






void alpha::pali()
{

   char fin[90];

	int b;
	int i; int a;int flag=0;

	for (a = n-1,b=0; a >= 0; a--,b++)
	{

		fin[b] = ori[a];

	}

	for(a=0;a<n;a++)
	{
		if(fin[a]!=ori[a])
		flag=1;
	}
	if(flag==0)
	cout << "\n The  string  is a palindrome. ";
	else
		cout << "\n The  string  is  not a palindrome. ";


}
