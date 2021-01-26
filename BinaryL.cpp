//============================================================================
// Name        : BinaryL.cpp
// Author      : Aboli
// Version     :
// Copyright   : Roll no.21301
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>



using namespace std;

struct node
{
	int data;
	node * prev;
	node * next;
	friend class cll;
};


class cll
{
	node *head;
public:
	void create(node *);
	void onescomp(node *);
	void twoscomp(node *);
	void add(cll);
	void display(node *);
};

void cll::create(node * head)
{


}









int main() {

int ch,n,m;

node* pnode,*qnode;

cll p;cll q;
pnode=qnode=NULL;

cout<<"Enter size of first list : "<<endl;

cin>>n;

do{

cout<<"\tWelcome\n";

cout<<" 1.Create List 1\n";

cout<<" 2.Create List 2\n";

cout<<" 3.1's compliment\n";

cout<<" 4.2's compliment\n";

cout<<" 5.Binary Addition\n";

cout<<" 6.Exit\n";

cout<<"Enter your choice : \n";

cin>>ch;

switch (ch)

{

case 1:

cout<<"Enter elements in first node."<<endl;
p.create(pnode);


cout<<"printing first list : ";

p.display(pnode);

break;

case 2:

cout<<"Enter elements in second node."<<endl;


p.create(pnode);
case 3:

p.onescomp(pnode);
break;

case 4:

	p.twoscomp(pnode);

break;

case 5:

p.add(q);

break;

}

}while (ch>=1 && ch<=6);

return 0;

}
