//============================================================================
// Name        : MatrixOperations.cpp
// Author      : Roll no.21301.Aboli Marathe
// Version     :
// Copyright   : Matrix Operations
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;


#include <iostream>
using namespace std;


class matrix
{
	int mat1[10][10];
	int mat2[10][10];
    int m;	int n;
public:
	void input();
	void tri();
	void trans();
	void di();
	void add();
   void saddle();
};

int main()
{
	int c = 0;
	matrix alpha;
	alpha.input();
	do
	{
	cout << "\n Enter the number as per operation: 1 for check of upper triangular, 2 for summation of diagonal elements,3 to commute transpose, 4 to add, subtract and multiply,5 to check saddle point :";
	cin >> c;


	switch (c)
	{
	case 1:alpha.tri(); break;
	case 2:alpha.di(); break;
	case 3:alpha.trans(); break;
	case 4:alpha.add(); break;
	case 5:alpha.saddle(); break;
	default:cout << "Invalid operation"; break;

	}


	}while(1);


	return 0;
}


void matrix::input()
{
	int i, j = 0;


	cout << "\nENter number of rows :";
	cin >> m;


	cout << "\nENter number of columns:";
	cin >> n;

	cout << "\n Enter the elements of the matrix : ";
	int a[10][10]; int sum = 0; int revsum = 0;
	for (i = 0; i < m; i++)
	{
		for (j = 0; j < n; j++)
		{
			cin >> mat1[i][j];

		}
	}


}




void matrix::tri()
{
	int i, j = 0;
	int check = 0;


	int a[10][10]; int sum = 0; int revsum = 0;


	if(m==n)
	{
	for (i = 1; i < m; i++)
	{
		for (j = 0; j < i; j++)
		{
			if (mat1[i][j] != 0)
				check++;

		}

	}
	if (check > 0)
	{
		cout << "\n The matrix is not upper triangular";
	}
	else
	{
		cout << "\n The matrix is upper triangular";
	}

	}
	else
		cout<<"\n Upper triangular is only applicable to square matrices !";

}



void matrix::di()
{
	int i, j = 0;
	int a[10][10]; int sum = 0; int revsum = 0;
	if(m==n)
	{
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < n; j++)
		{
			if (i == j)
				sum = sum + mat1[i][j];

		}
	}


	for (i = 0; i < n; i++)
	{
		for (j = n-1; j >=0 ; j--)
		{
			if((i+j)==(n-1))
			revsum = revsum + mat1[i][j];
		}

	}

	cout << "\n The sum of forward diagonal elements is :"<<sum<<"\n The sum of reverse diagonal elements is :"<<revsum;
}
	else
		cout<<"\n THe sum of diagonal elements is only applicable to square matrices!";

}


void matrix::trans()
{
	int i, j = 0;
	int a[10][10];
	for (i = 0; i < m; i++)
	{
		for (j = 0; j < n; j++)
		{
			a[j][i]=mat1[i][j];

		}
	}

	cout << "\n The resultant transpose  is :\n";
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < m; j++)
		{

			cout << " " << a[i][j] << " ";
		}
		cout << "\n";
	}
}

void matrix::add()
{
	int i, j, k = 0;
	int a[10][10]; int b[10][10]; int c[10][10];
	int m1;int n1;
	cout << "\nENter number of rows in second matrix:";
	cin >> m1;


	cout << "\nENter number of columns in second matrix:";
	cin >> n1;


	for (i=0;i<m;i++)
		{
			for (j = 0; j < n1; j++)
			{
				c[i][j] = 0;

			}
		}

	cout << "\n Enter the elements of the second matrix : ";
	for (i = 0; i < m1; i++)
	{
		for (j = 0; j < n1; j++)
		{
			cin >> mat2[i][j];

		}
	}

	if((m1==m)&&(n1==n))
	{
	for (i = 0; i < m; i++)
		{
			for (j = 0; j < n; j++)
			{

				a[i][j] = mat1[i][j] + mat2[i][j];
			}
		}

	cout << "\n The result of addition is :\n";
		for (i = 0; i < m; i++)
		{
			for (j = 0; j < n; j++)
			{

				cout << " " << a[i][j] << " ";
			}
			cout << "\n";
		}

	}
	else
		cout<<"\n Addition is not applicable as the order is not the same!";

	if((m1==m)&&(n1==n))
	{
	for (i = 0; i < m; i++)
	{
		for (j = 0; j < n; j++)
		{

			b[i][j] = mat1[i][j] - mat2[i][j];
		}
	}


	cout << "\n The result of subtraction is :\n";
	for (i = 0; i < m; i++)
	{
		for (j = 0; j < n; j++)
		{

			cout << " " << b[i][j] << " ";
		}
		cout << "\n";
	}


	}
	else cout<<"\nSubtraction is not applicable as the order is not the same!";

 if(n==m1){
	for (i = 0; i < m; i++)
	{
		for (j = 0; j < n1; j++)
		{
			for (k = 0; k < n; k++)
			{
				c[i][j] = c[i][j] + (mat1[i][k] * mat2[k][j]);
			}
		}
	}




	cout << "\n The result of multiplication is :\n";
	for (i = 0; i < m; i++)
	{
		for (j = 0; j < n1; j++)
		{

			cout << " " << c[i][j] << " ";
		}
		cout << "\n";
	}


 }else
	 cout<<"\n MUltiplication of matrices is not possible as number of columns in first does not match number of rows in second";

}




void matrix::saddle()
{
	int flag=0;
	int i, j = 0;
	int amax ;
	int amin;
	int min[10];
	int max[10];

	for (i = 0; i < m; i++)
	{
		amin = 1000;
		for (j = 0; j < n; j++)
		{
			if (amin > mat1[i][j])
			{
				amin = mat1[i][j];
			}
		}
		min[i] = amin;

	}

	for (j = 0; j < n; j++)
	{
		amax = 0;
		for (i= 0; i < m; i++)
		{
			if ( mat1[i][j]>amax)
			{
				amax = mat1[i][j];
			}
		}
		max[j] = amax;


	}


	for (i = 0; i < m; i++)
	{

		for (j = 0; j < n; j++)
		{
			if ((mat1[i][j] == min[i]) && (mat1[i][j] == max[j]))
			{
				cout << "\n THe saddle point is  : "<< mat1[i][j];
				flag=1;
			}

		}
	}
if(flag==0)
	cout<<"\n There is no saddle point";}
