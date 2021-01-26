//============================================================================
// Name        : Sparse.cpp
// Author      : Aboli
// Version     :
// Copyright   : 
// Description : Hello World in C++, Ansi-style
//============================================================================



#include<iostream>
using namespace std;

class sparse
{
	int r;
	int c2;
	int c1;
	int a1[10][10];
	int b[10][10];
	int finb[10][10];
	int b1[10][10];
	int c[10][10];
	int sum[10][10];
public:
	void accept();
	void display();
	void convert();
	void fast();
	void add();

};


void sparse::accept()
{
	int i, j;
	cout<<"Enter the Rows and Columns of matrix\n";
	cin >> r >> c1;
	cout<<"Enter the element\n";
	for (i = 0; i < r; i++)
	{
		for (j = 0; j < c1; j++)
		{
			cin >> a1[i][j];
		}
	}
}

void sparse::display()
{
	int i, j;

	cout << "\n\nThe conventional matrix is :\n\n";
	for (i = 0; i < r; i++)
	{
		for (j = 0; j < c1; j++)
		{
			cout<< a1[i][j]<<"  ";
		}
		cout<<"\n";
	}

	cout << "\n\nThe sparse matrix is :\n\n";
	for (i = 0; i <= b[0][2]; i++)
	{
		for (j = 0; j < 3; j++)
		{
			cout << b[i][j]<<"  ";
		}
		cout << "\n";
	}


	cout << "\n\nThe fast transpose matrix is :\n\n";
	for (i = 0; i <= c[0][2]; i++)
	{
		for (j = 0; j < 3; j++)
		{
			cout << c[i][j] << "  ";
		}
		cout << "\n";
	}

	cout << "\n\nThe sum of sparse matrix is :\n\n";
	for (i = 0; i <= sum[0][2]; i++)
	{
		for (j = 0; j < 3; j++)
		{
			cout << sum[i][j] << "  ";
		}
		cout << "\n";
	}



}


//--------------------------------------------------------------------------------------------------------------

void sparse::convert()
{
	int i, j, k = 1;
	for (i = 0; i < r; i++)
	{
		for (j = 0; j < c1; j++)
		{
			if (a1[i][j] != 0)
			{
				b[k][0] = i;
				b[k][1] = j;
				b[k][2] = a1[i][j];
				k++;
			}
		}
	}
	b[0][0] = r;
	b[0][1] = c1;
	b[0][2] = k - 1;
}
//----------------------------------------------------------

void sparse::fast()
{
	int i, j, m, n, pos[3] = { 0,0,0 }, count[3] = { 0,0,0 };
	for (i = 1; i <= b[0][2]; i++)
	{
		m = b[i][1];
		count[m]++;
	}
	pos[0] = 1;
	for (i = 1; i < b[0][1]; i++)
	{
		pos[i] = pos[i - 1] + count[i - 1];
	}
	for (i = 1; i <= b[0][2]; i++)
	{
		m = b[i][1];
		n = pos[m];
		pos[m]++;
		c[n][0] = b[i][1];
		c[n][1] = b[i][0];
		c[n][2] = b[i][2];
	}
	c[0][0] = b[0][0];
	c[0][1] = b[0][1];
	c[0][2] = b[0][2];
}

//------------------------------------------------
void sparse::add()
{
	int i,j,s,d;
	int p, q;

	cout << "Enter the Rows and Columns of  second matrix\n";
	cin >> p >> q;
	cout << "Enter the element\n";


	for (i = 0; i < p; i++)
	{
		for (j = 0; j < q; j++)
		{
			cin >> b1[i][j];
		}
	}


	int k = 1;
	for (i = 0; i < p; i++)
	{
		for (j = 0; j < q; j++)
		{
			if (b1[i][j] != 0)
			{
				finb[k][0] = i;
				finb[k][1] = j;
				finb[k][2] = b1[i][j];
				k++;
			}
		}
	}
	finb[0][0] = r;
	finb[0][1] = c1;
	finb[0][2] = k - 1;



	cout << "\n\nThe second sparse matrix is :\n\n";
	for (i = 0; i <= finb[0][2]; i++)
	{
		for (j = 0; j < 3; j++)
		{
			cout << finb[i][j] << "  ";
		}
		cout << "\n";
	}


//----------------------------------------------------------------------------------------------------------------------------------------------------------

	i = j = s = d = 0;

	while ((i <=b[0][2] ) && (j <= finb[0][2]))
	{
		if (b[i][0] == finb[j][0])    // if rows are equal
		{
			if (b[i][1] == finb[j][1]) // if columns are equal
			{
				sum[s][0]= b[i][0];
				sum[s][1]  = b[i][1];
				sum[s][2] = b[i][2] + finb[j][2];

				i++;
				j++;
				if (sum[s][2] != 0)
					s++;

			}
			else   // if columns are not equal
			{
				if (b[i][1] < finb[j][1])
				{
					sum[s][0] = b[i][0];
					sum[s][1] = b[i][1];
					sum[s][2] =  b[i][2];

					i++;
					s++;

				}
				else
				{
					sum[s][0] = finb[j][0];
					sum[s][1] = finb[j][1];
					sum[s][2]  = finb[j][2];


					j++;

					s++;
				}
			}
		}
		else   //   if rows are not equal
		{
			if (b[i][0] < finb[j][0])
			{
				sum[s][0] = b[i][0];
				sum[s][1] =  b[i][1];
				sum[s][2] = b[i][2];

				i++;

				s++;
			}
			else
			{
				sum[s][0] = finb[j][0];
				sum[s][1] =  finb[j][1];
				sum[s][2] = finb[j][2];


				j++;
				s++;

			}
		}
	}  // end of while
	if (i <= b[0][2])
	{
		for (p = i; p <= b[0][2]; p++)
		{
			sum[s][0] = b[p][0];
			sum[s][1] = b[p][1];
			sum[s][2] = b[p][2];

			s++;

		}
	}
	else if (j <= b[0][2])
	{
		for (p = j; p <= b[0][2]; p++)
		{
			sum[s][0] = finb[p][0];
			sum[s][1] =  finb[p][1];
			sum[s][2] = finb[p][2];


			s++;

		}
	}
	// end of addition and subtraction
	sum[0][0]  = r;
	sum[0][1]  = c1;
	sum[0][2] = s - 1;




}












//----------------------------------------------------------

int main()
{

	sparse alpha;
	alpha.accept();
	alpha.convert();
	alpha.fast();
	alpha.add();
	alpha.display();
}
