#include <iostream>
#include<stdlib.h>
#include<string.h>
using namespace std;
//data structure for student information i:e node
struct node
{
 int prn;
 string name;
 node *next;
};

class panclub
{
    int num,cnt;
    string nm;  //Data members
    node *head;
 public:
    panclub()//Constructor to initialize object
    {
     num=cnt=0;
     head=NULL;
    }
    node *create();
    void display(node *);
    node *concat(node *,node *);
    void reverse(node *);
    node* insert_president(node *);
    void insert_sec(node *);
        void insert_member(node *);
    node* del_president(node *);
        node* del_secretary(node *);
        node* del_member(node *);
        int numb(node *);
        node * sort(node *);
        node *merge(node *,node *);
};

//To Create the list of Divisions.
node* panclub::create()
{
    node *temp,*n1;
    temp=n1=NULL;
    int cont=0;

    do
    {
        n1=new node; //first of allocate the memory for all feilds of struct.
        cout<<"\nEnter the prn number of student:";
        cin>>num;
        n1->prn=num;
        //storing the prn in node feild prn;
        cout<<"\nEnter the name of student:";
        cin>>nm;
        n1->name=nm;
        //Storing the name in node feild name.;
        n1->next=NULL; //making the next feild null.
        if(head==NULL) //check for head is empty.
        {
            head=n1;   //make new node as head
            temp=head;
        }
        else
        {
            temp=head;
            while(temp->next!=NULL) //attach at the end of list
            {
                temp=temp->next;
            }
            temp->next=n1;
        }
        cnt++;
        cout<<"\nDo you want to continue, 1 or 0:  ";
           cin>>cont;
    }while(cont>0);

    return head;
}

void panclub::display(node *head)
{
 node *temp;
 temp=head;
 while(temp!=NULL)
 {
     if(temp->next==NULL)
     {
      cout<<"["<<temp->prn<<"|"<<temp->name<<"]->NULL";
     }
     else
     {
      cout<<"["<<temp->prn<<"|"<<temp->name<<"]->";
     }
     temp=temp->next;
 }
}

node* panclub::concat(node *head1,node *head2)
{
    node *head3,*temp,*temp1;
    head3=temp=temp1=NULL;
    temp=head1;
    head3=head1;
    while(temp->next!=NULL)
    {
        temp=temp->next;
    }
    temp1=head2;
    temp->next=temp1;
    return head3;
}
void panclub::reverse(node *head)
{
    node *temp;
    temp=head;
    if(temp==NULL)
        return;
    reverse(temp->next);
    cout<<"["<<temp->prn<<"|"<<temp->name<<"]->";
}

node* panclub::insert_president(node *head)
{
 node *temp,*n2;
 temp=n2=NULL;
 temp=head;
 n2=new node;
 cout<<"\nEnter the PRN number of President: ";
 cin>>n2->prn;
 cout<<"\nEnter the name of President: ";
 cin>>n2->name;
 n2->next=temp;
 head=n2;
 cnt++;
 return head;
}

void panclub::insert_member(node *head)
{
 node *temp,*n2;
 int pn;
 temp=head;
 n2=new node;
 cout<<"\nEnter the PRN number of Member: ";
 cin>>n2->prn;
 cout<<"\nEnter the name of Member: ";
 cin>>n2->name;
 n2->next=NULL;
 cout<<"\nEnter the PRN number after which u want to add this member: ";
 cin>>pn;
 while(temp!=NULL)
 {
  if(temp->prn==pn)
  {
   n2->next=temp->next;
   temp->next=n2;
   break;
  }
  temp=temp->next;
 }
 cout<<"\n\nMember added sucessfully....!!";
 cnt++;
}

void panclub::insert_sec(node *head)
{
 node *temp,*n2;
 temp=head;
 n2=new node;
 cout<<"\nEnter the PRN number of Secretary: ";
 cin>>n2->prn;
 cout<<"\nEnter the Name of Secretary: ";
 cin>>n2->name;
 n2->next=NULL;
 while(temp->next!=NULL)
 {
  temp=temp->next;
 }
 temp->next=n2;
 cnt++;
}
//delete the president node from list
node* panclub::del_president(node *head)
{
 node *temp;
 temp=head;
 head=temp->next;
 free(temp);
 cnt--;
 return head;
}
//Delete the secretary node from the list.
node* panclub::del_secretary(node *head)
{
 node *temp,*t1;
 temp=head;
 while(temp->next!=NULL)
 {
  t1=temp;
  temp=temp->next;
 }
 t1->next=NULL;
 free(temp);
 cnt--;
 return head;
}

//Delete the memeber from the list.

node* panclub::del_member(node *head)
{
 node *temp,*t1;
 int pn;
 temp=head;
 cout<<"\nEnter the PRN number after which u want to delete member: ";
 cin>>pn;
 while(temp!=NULL)
 {
  if(temp->prn==pn)
  {
   t1=temp->next;
   temp->next=t1->next;
   free(t1);
   break;
  }
  temp=temp->next;
 }
 cout<<"\n\nMember removed sucessfully....!!";
 cnt--;
 return head;
}


int panclub::numb(node * head)
{
return cnt;
}


node * panclub::sort(node * head)
{

		node *i,*j,*last=NULL;
		int tprn;
		string tname;

	if(head==NULL)
	{
		cout<<"\nList is empty.";
		return NULL;
	}
	for(i=head;i->next!=NULL;i=i->next)
	{
		for(j=head;j->next!=last;j=j->next)
		{
			if((j->prn)>(j->next->prn))
			{
				tprn=j->prn;
				tname=j->name;
				j->prn=j->next->prn;
				j->name=j->next->name;

				j->next->prn=tprn;
				j->next->name=tname;

			}
		}
	}



return head;

}



node * panclub:: merge(node* a,node* b)
{

	node* result = NULL;



	    /* Base cases */
	    if (a == NULL)
	        return(b);
	    else if (b == NULL)
	        return(a);

	    /* Pick either a or b, and recur */
	    if (a->prn <= b->prn)
	    {
	        result = a;
	        result->next = merge(a->next, b);
	    }
	    else
	    {
	        result = b;
	        result->next = merge(a, b->next);
	    }

	cout<< " ["<< result->prn<<"| "<<result->name<< "]->";
return result;

}








int main()
{
    panclub p1,p2,p3,p4;
    node *h1,*h2,*h3,*h4;
    h1=h2=h3=NULL;
    int ch,choose1,c1,c2,c3;
    cout << "\n\t!WELCOME TO THE PINNACLE CLUB " << endl;
   do
   {

    cout<<"\nEnter your choice: ";
    cout<<"\n\n1.Enter data of Class 1	";
       cout<<"\n2.Enter data of Class 2";
       cout<<"\n3.Concatenation or merging of Both Classes ..";
    cin>>ch;
    switch(ch)
    {
     case 1:

            cout<<"\n\nEnter the  Data of Class 1:\n";
            h1=p1.create();
        cout<<"\nClass 1  List are as follows..\n\n";
        p1.display(h1);
       do{ cout<<"\n\n Choose the next operation: 1 to add President, 2 to add Secretary, 3 to add a member , 4 to display the list in reverse order, 5 to delete a member ,6 to delete President, 7 to delete secretary,8 to find total number of members ,9 to sort the list:";
        cin>>choose1;
        switch(choose1)
        {
        case 1:h1=p1.insert_president(h1);
        cout<<"\nAfter insertion of President: \n";
        p1.display(h1);break;
        case 2:p1.insert_sec(h1);
        cout<<"\nAfter insertion of Secretary: \n";
        p1.display(h1);
        break;
        case 3:p1.insert_member(h1);
        cout<<"\n After insertion of member...\n";
        p1.display(h1);break;
        case 4:cout<<"\n\nReverse List of SE Div A:\n\n";
        p1.reverse(h1);break;

        case 5: h1=p1.del_member(h1);
        cout<<"\n\nAfter deletion of member...\n";
        p1.display(h1);break;
        case 6: h1=p1.del_president(h1);
        cout<<"\n\nAfter deletion of president...\n";
        p1.display(h1);break;
        case 7: h1=p1.del_secretary(h1);
        cout<<"\n\nAfter deletion of secretary...\n";
        p1.display(h1);break;
        case 8:
        	cout<<"\n\n The number of members in class 1 are "<<p1.numb(h1);break;
        case 9:
        	cout<<"\n\n hi";
        	h1=p1.sort(h1);
        	        cout<<"\n\nAfter sorting of list..\n";
        	        p1.display(h1);
        	       break;

        default:cout<<"\n\n INvalid op ";break;


        }
       cout<<"\n\n Do you wish to perform more operations on Class 1";
       cin>>c2;
       }while(c2==1);
        break;
    case 2:
        cout<<"\n\nEnter the  Data of Class 2:\n";
        h2=p2.create();
        cout<<"\nClass 2 List are as follows..\n\n";
        p2.display(h2);
        do{cout<<"\n\n Choose the next operation: 1 to add President, 2 to add Secretary, 3 to add a member at a position, 4 to display the list in reverse order, 5 to delete a member at a position,6 to delete President, 7 to delete secretary,8 to find total number of members ,9 to sort the list :";
            cin>>choose1;
            switch(choose1)
            {
            case 1:h2=p2.insert_president(h2);
            cout<<"\nAfter insertion of President: \n";
            p2.display(h2);break;
            case 2:p2.insert_sec(h2);
            cout<<"\nAfter insertion of Secretary: \n";
            p2.display(h2);break;
            break;
            case 3:p2.insert_member(h2);
            cout<<"\n After insertion of member...\n";
            p2.display(h2);break;
            case 4:cout<<"\n\nReverse List of SE Div A:\n\n";
            p2.reverse(h2);break;

            case 5: h2=p2.del_member(h2);
            cout<<"\n\nAfter deletion of member...\n";
            p2.display(h2);break;
            case 6: h2=p2.del_president(h2);
            cout<<"\n\nAfter deletion of president...\n";
            p2.display(h2);break;
            case 7: h2=p2.del_secretary(h2);
            cout<<"\n\nAfter deletion of secretary...\n";
            p2.display(h2);break;
            case 8:
                    	cout<<"\n\n The number of members in class 2 are "<<p2.numb(h2);break;
            case 9:
            	h2=p2.sort(h2);
            	        	        cout<<"\n\nAfter sorting of list..\n";
            	        	        p2.display(h2);
            	        	      break;
            default:cout<<"\n\n INvalid op ";break;


            }
        cout<<"\n\n Do you wish to perform another operation on Class 2? 1 or 0";
        cin>>c1;
        }while(c1==1);
            break;
    case 3:


        h3=p3.concat(h1,h2);
        cout<<"\n\nThe concatenation of  Class 1 and 2 are as follows.\n\n";
        p3.display(h3);
        cout<<"\n\nThe concatenation of  Class 1 and 2 are as follows.\n\n";

    	h4=p4.merge(h1,h2);

            break;
  }
 }while(ch!=4);
        return 0;

}
