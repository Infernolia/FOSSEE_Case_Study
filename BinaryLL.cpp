//============================================================================
// Name        : BinaryLL.cpp
// Author      : Aboi
// Version     :
// Copyright   : Roll no.21301
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

class node
{
	int data;
	node *next;
	node *prev;
public:
	friend class binaryll;

};

class binaryll
{
	node *head;

public:
    binaryll()
	{
    	head=NULL;
	}


	void insert();
   void display();
   void oness();
   void twos();


};

void binaryll::insert()
{
	int cont=0;int dt=0;
	node *ptr;
	do

	{

		cout<<"\n\n Enter the bit : ";
		cin>>dt;
		ptr->data=dt;

		cout<<"\n\n 1";


		if(!head)
		{
			cout<<"\n\n 2";
			head=ptr;
		}
		else
		{
			node *t;
			t=head;
			while(t->next)
			{
				t=t->next;
				t->next=ptr;
				ptr->prev=t;
			}

		}
		cout<<"\n\n Do you want to continue :";
		cin >> cont;


	}while(cont>0);
}

void binaryll::oness()
{
 node *ptr;
 ptr=head;
 if(!head)
 {
	 cout<<"\n\n Empty list";

 }

 else
 {
	 head=ptr;
	 while(ptr->next)
	 {
		 ptr=ptr->next;
	 }
	 if(ptr->data==1)
	 {}
	 else
	 {

	 }
 }




}


void binaryll::display()
{

	node* temp;
	temp=head;

	if(!head)
	{

		cout<<"\n\n The list is empty";

	}
	else
	{
		while(temp->next)
		{
			cout<<"["<<temp->data<<"]"<<"->";
			temp=temp->next;

		}
		cout<<"["<<temp->data<<"]"<<"->";

	}

}




int main()

{
	binaryll x;
	cout<<"ENter members of the binary number from left to right";
	x.insert();
	cout<<"\n\n Final list : ";
	x.display();

	cout<<"\n\n ONes complement";
	x.oness();



	return 0;
}
