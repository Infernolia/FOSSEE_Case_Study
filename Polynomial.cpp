//============================================================================
// Name        : Polynomial.cpp
// Author      : Aboli
// Version     :
// Copyright   : 
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
#include <iomanip>

using namespace std;

class polynomial
{

	int n;
	int coef[10];
	int exp[10];
	int x;
	int ex[10];
		int coe[10];
		int finex[10];
		int fincoe[10];
		int k ;
int micounter;
		public:
			void evaluate();
			void add();
			void multiply();
			friend istream &operator>>(istream &op,polynomial &c);
			friend ostream &operator<<(ostream &op,polynomial &c);
};

int main()
{
	polynomial x;
	cin>>x;
	int choice = 0;

	cout << "\n\n Enter choice : 1 for evaluate, 2 for addition, 3 for multiplication :";
	cin >> choice;
	switch (choice)
	{
	case 1: x.evaluate(); break;
	case 2:	x.add(); cout<<x;break;
	case 3:x.multiply(); cout<<x;break;
	default:cout << "\n\n Invalid op";
	}


}







void polynomial::add()
{
	micounter=0;
	int counter = 0;
	int o;
	int i = 0;
	int j = 0;
    k=0;
	int l = 0;
	for (i = 0; i < 10; i++)
	{
		finex[i] = 0;
		fincoe[i] = 0;
	}

	cout << "\n\n number of terms in second polynomial:";
	cin >> o;

	for (i = 0; i < o; i++)
	{
		cout << "\n\n Enter the exponent of the polynomial term :  ";
		cin >> ex[i];
		cout << "\n\n Enter the coefficient of the term x^  " << ex[i] << "  : ";
		cin >> coe[i];

	}
	i = 0;
	j = 0;

	while((i<n)&&(j<o))
	{
			if (exp[i] == ex[j])
			{
				fincoe[k] = coef[i] + coe[j];
				finex[k] = exp[i];
				exp[i] = -1;
				ex[j] = -1;
				i++;
				j++;
				k++;

				counter++;
			}
			else if (exp[i] < ex[j])
			{
				fincoe[k] = coe[j];
				finex[k] = ex[j];
				j++;
				k++;

				counter++;
			}
			else
			{
				fincoe[k] = coef[i];
				finex[k] = exp[i];
				i++;
				k++;

				counter++;
			}

	}
	if (i != (n - 1))
	{
		for (; i < n; i++)
		{
			fincoe[k] = coef[i];
			finex[k] = exp[i];
			k++;


		}
	}

	if (j!=( o - 1))
	{
		for (; j < n; j++)
		{
			fincoe[k] = coe[j];
			finex[k] = ex[j];
			k++;


		}
	}



}



void polynomial::multiply()
{
micounter=1;
	int counter = 0;
	int o;


	int final[10];
	int finalb[10];

	int i = 0;
	int j = 0;
k = 0;
	int l = 0;
	for (i = 0; i < 10; i++)
	{
		finex[i] = 0;
		fincoe[i] = 0;
	}

	cout << "\n\n number of terms in second polynomial:";
	cin >> o;

	for (i = 0; i < o; i++)
	{
		cout << "\n\n Enter the exponent of the polynomial term :  ";
		cin >> ex[i];
		cout << "\n\n Enter the coefficient of the term x^  " << ex[i] << "  : ";
		cin >> coe[i];

	}




	for (i = 0; i < n; i++)
	{
		for (j = 0; j < o; j++)
		{
			fincoe[k] = coef[i] * coe[j];
			finex[k] = exp[i] + ex[j];
			k++;

		}
	}
	int r, s = 0;

	int freqexp[10];
	int freqcoef[10];


	for (i = 0; i < k; i++)
	{
		for (j = 0; j < k; j++)
		{
			if ((i != j) && (finex[j] == finex[i]))
			{
				fincoe[i] += fincoe[j];
				finex[j]-=100;
			}


		}
	}


}


void polynomial::evaluate()
{
	int sum = 0;
	int inter = 1;
	int i,j;
	cout << "\n\n Enter value of x  : ";
	cin >> x;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < exp[i]; j++)
		{
			inter *= x;
		}
		sum +=( coef[i] * inter);
		inter = 1;
	}
	cout << "\n\n The evaluation is : "<<sum;
}


//----------------------------------------------------------------------------



istream &operator>>(istream &op,polynomial &c)
{



	int i = 0;
	c.n = 0;
	cout << "\n\n number of terms :";
	cin >>c.n;

	for (i=0;i<c.n;i++)
	{
		cout << "\n\n Enter the exponent of the polynomial term :  ";
		cin >> c.exp[i];
		cout << "\n\n Enter the coefficient of the term x^  " << c.exp[i]<<"  : ";
		cin >> c.coef[i];

	}

}



 ostream &operator<<(ostream &op,polynomial &c)
{
 int i=0;
		cout << "\n\n The final expression is :";
		if(c.micounter==0)
		{

			for (i = 0; i <= c.k; i++)
						{


								cout <<"\n\n"<< c.fincoe[i] << "  " << "x ^ " << c.finex[i] << "+ ";
						}
		}
		else if(c.micounter==1)
		{
			for (i = 0; i < c.k; i++)
			{
				if (c.finex[i]>=0)

					cout << c.fincoe[i] << "  " << "x ^ " << c.finex[i] << "+ ";

			}

        }
}
