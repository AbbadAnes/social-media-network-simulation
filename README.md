<h1>social-media-network-simulation</h1>
<p align="center">
  <img src="https://storage.googleapis.com/kainofreelancerpictures/anes/sma.png" width="50%" title="logo">
</p>
The goal of this project is to model a small social network using the techniques of game theory.
This social network which consists mainly of individuals wanting to establish links between them for personal or professional interests. The connection produced after the establishment of a link between two individuals makes change the structure of the social network and makes it evolve thanks to some actions allowed in the network like:
<br>
- Add a friend
<br>
- accept / reject a friend
<br>
- delete friendshipe link
<br>
Network users using these actions to improve their earnings according to different policies for each population in the network.



<h2> Tools : </h2>
Walforme Mathematica

<h2>Network definition</h2>
Let "Population" be a graph representing a population consisting of: Standard, Rich and Famous Users.
As: each vertex of the population graph represents a user. And the links between the vertices represent the connection between two users.
A user is identified by:
<br>
Id, Type, Gain, Request List, List of Invitations Received
<br>
-> the ID is a unique identifier that represents the individual in the network.
<br>
-> Type is the population of the user it can be:
<br>
C: A little "Famous" often they were standard people and by dint of connecting with a lot of people they became famous, this type of population is known in some social networks like Facebook and Instagram under the name "influencers".
<br>
Cb: Famous type B often super stars who frequent social networks to be closer to their fans.
<br>
A: "Rich" people use social networks to find financial opportunities such as finding a famous one to promote their products.
<br>
S: Standard people who frequent social networks for different reasons: finding friends, becoming famous or just a hobby.
<br>
-> the list of requests contains all the invitations sent by the summit in question.
<br>
-> the reception list contains all the invitations received by the summit in question.
<br><br>

```Mathematica

(*liste ds individues ( sommets) *)
individues = {1,2,3,4,5,6,7,8,9,10};\[IndentingNewLine](*presentation d'une population par un graphe*)\[IndentingNewLine]population=Graph[ individues,{}, VertexLabels-> "Name", GraphLayout -> "CircularEmbedding", ImageSize -> Medium,EdgeStyle -> {Gray, Thick}] ;
population = SetProperty[{population, 1}, 
  VertexWeight -> 
   Info[id["C", 1], 10, demande[], reception[]]];
   
population = SetProperty[{population, 2},
  VertexWeight -> Info[id["Cb", 2], 20, demande[], reception[]]];
  
population = SetProperty[{population, 3},
  VertexWeight -> Info[id["R", 3], 7, demande[], reception[]]];
  
population = SetProperty[{population, 4}, 
  VertexWeight -> Info[id["S", 4], 20, demande[], reception[]]];
  
  population = SetProperty[{population, 5}, 
  VertexWeight -> Info[id["C", 5], 60, demande[], reception[]]];
  
  population = SetProperty[{population, 6}, 
  VertexWeight -> Info[id["Cb", 6], 21, demande[], reception[]]];
  
  population = SetProperty[{population, 7}, 
  VertexWeight -> Info[id["Cb", 7], 30, demande[], reception[]]];
  
  population = SetProperty[{population, 8}, 
  VertexWeight -> Info[id["R", 8], 13, demande[], reception[]]];
  
  
    population = SetProperty[{population, 9}, 
  VertexWeight -> Info[id["S", 9], 12, demande[], reception[]]];
  
  population = SetProperty[{population, 10}, 
  VertexWeight -> Info[id["S", 10], 20, demande[], reception[]]];

l = {PropertyValue[ {population, 1}, VertexWeight],  PropertyValue[ {population, 2}, VertexWeight], PropertyValue[ {population, 3}, VertexWeight], PropertyValue[ {population, 4}, VertexWeight],PropertyValue[ {population, 5}, VertexWeight],
  PropertyValue[ {population, 6}, VertexWeight],PropertyValue[ {population, 7}, VertexWeight],PropertyValue[ {population, 8}, VertexWeight],PropertyValue[ {population, 9}, VertexWeight], PropertyValue[ {population, 10}, VertexWeight]};
 For[i=1,i<=10,i++,m = Part[l, i];
population = SetProperty[population, VertexLabels -> {i -> Part[m, 1]}];];

```

<br>
So we get our initial network:

<p align="center">
  <img src="https://storage.googleapis.com/kainofreelancerpictures/anes/network.png" width="50%" title="logo">
</p>

<h2> Policy </h2>
Each type of population follows a strategy to improve the gain. especially to accept a login invitation.
Each user must make sure that the new link established with an individual is useful to improve his gain.
We have defined the "Accept" function in order to implement our winning policy.
<br>
Indeed it takes in parameter two individuals x and y such that: x has received an invitation from y, and it returns if x should accept the invitation to respect the following rules:
<br>
- An individual of the "C" population (Famous) can accept individuals who are of population "R" (Rich) and "Cb" (Famous of type beta).
<br>
- An individual of the population "Cb" (Famous of type beta) can accept the individuals who are of population "R" (Rich), "C" (Famous) and those of type "S" (Standard)
<br>
- An individual of the "R" (Rich) population can accept individuals who are of "Cb" (Famous Beta), "C" (Famous) and "S" (Standard) type whose gain is greater than 10
<br>
- And finally the population "S" can accept all individuals.



<br><br>

```Mathematica
Accept[x_,y_]:=Module[{u},With[{r=0},u=r; 
Which[Part[x,1]=="C"&& Part[y,1]=="Cb",u:= r+ 1,Part[x,1]=="C"&& Part[y,1]=="C",u:= r+1,Part[x,1]=="C"&& Part[y,1]=="R",u:= r+1, Part[x,1]=="R"&& Part[y,1]=="R",u:=r+ 1,Part[x,1]=="R"&& Part[y,1]=="C",u:=r+ 1, Part[x,1]=="R"&& Part[y,1]=="Cb",u:=r+ 1,Part[x,1]=="R"&& Part[y,1]=="S"&&Part[y,2]>=10,u:= r+1,Part[x,1]=="S",u:=r+1];r];u]
```

<br>
After each link established it is imperative to update the gain of the two individuals.
<br><br>


```Mathematica
Type[x_]/; x>15&& x<20 := "R"\[IndentingNewLine]Type[x_]/;x> 20 := "C"\[IndentingNewLine]Type[x_]/; x<15 := "S"
(*deffinition de la fonction NewGain : elle attribue a chaque indivdu son nouveau gain *)
NewGain[x_,n_]:= Module[{r,u},\[IndentingNewLine]Which[x=="R",u:=Part[n,2]+2,x=="S",u:=Part[n,2]+1,x=="C",u:=Part[n,2]+3,x=="Cb"&&Part[n,1]=="R",u=Part[n,2]+3+2,x=="Cb"&&Part[n,1]=="S",u=Part[n,2]+3+1,x=="Cb"&&Part[n,1]=="C",u=Part[n,2]+3+3];u];

```
<br>
<h2> Sending invitations </h2>
As we mentioned earlier each individual in the network is looking to improve his gain. regardless of the type of population and its winning policy. Establishing new links with users can only improve the gain.
For this, each user sends login invitations to each user who has a gain greater than or equal to his gain.
thus each user ensures a good quality of link with these future friends.
<br><br>

``` Mathematica
For[k=1,k<= Length[l],k++,
myProp = Part[l,k];
For[j=1,j<= Length[l],j++,
individue = Part[l,j];
If[Part[individue,2] > Part[myProp,2],
population= SetProperty[{population, k}, VertexWeight -> Info[Part[myProp,1], Part[myProp,2],Insert[Part[myProp,3],j,-1],Part[myProp,4]]];
population = SetProperty[{population, j}, VertexWeight -> Info[Part[individue,1], Part[individue,2], Part[individue,3],Insert[Part[individue,4],k,-1]]];
l = {PropertyValue[ {population, 1}, VertexWeight], 
  PropertyValue[ {population, 2}, VertexWeight], 
  PropertyValue[ {population, 3}, VertexWeight], 
  PropertyValue[ {population, 4}, VertexWeight],
  PropertyValue[ {population, 5}, VertexWeight],
  PropertyValue[ {population, 6}, VertexWeight],
  PropertyValue[ {population, 7}, VertexWeight],
  PropertyValue[ {population, 8}, VertexWeight],
  PropertyValue[ {population, 9}, VertexWeight],
  PropertyValue[ {population, 10}, VertexWeight]};
]
]
]

```


<br>
<h2> Accept invitations: </h2>
After the send-out stage, it's time to accept the invitations received.
This time each type of population follows a logic different from the others as we mentioned in the political section of gain in order to improve their gain or satisfy their personal or professional interests.
<br><br>

``` Mathematica
EdgeToAdd = {};
For[j = 1, j <= Length [l],j++,
myProp = Part[l,j];
receptionList = Part[myProp, 4];
monGain = Part[myProp, 2];
For[i = 1, i <= Length [receptionList], i++, 
 m = Part[l, Part[receptionList, i]]; 
 If[Accept[Part[myProp,1],Part[m,1]]==1,
 myId = Part[myProp, 1];ID = Part[myId, 2];
 SecondID = Part[m, 1]; SID = Part[SecondID, 2]; 
 AppendTo[EdgeToAdd,ID <-> SID];
 (* mettre à jour le gain et la population *)
 population = SetProperty[{population,ID},VertexWeight -> info[id[Type[NewGain[Part[myId,1],myProp]],ID],NewGain[Part[myId,1],myProp],Part[myProp,3],Part[myProp,4] ] ];
 population = SetProperty[{population,SID},VertexWeight ->  info[id[Type[NewGain[Part[SecondID,1],m]],SID],NewGain[Part[SecondID,1],m],Part[m,3],Part[m,4] ]];
  ]
  ]
  ];
  population=EdgeAdd[population, EdgeToAdd];

```



<br>
<h2> Delete a link </h2>
it's sometimes necessary to delete some connection links if for example the Threshold of maximum number of friends is reached and we want to connect with other people with whom we have more interests.
<br><br>

``` Mathematica
EdgeToDelete = {};
l2 = {PropertyValue[ {population, 1}, VertexWeight], 
  PropertyValue[ {population, 2}, VertexWeight], 
  PropertyValue[ {population, 3}, VertexWeight], 
  PropertyValue[ {population, 4}, VertexWeight],
  PropertyValue[ {population, 5}, VertexWeight],
  PropertyValue[ {population, 6}, VertexWeight],
  PropertyValue[ {population, 7}, VertexWeight],
  PropertyValue[ {population, 8}, VertexWeight],
  PropertyValue[ {population, 9}, VertexWeight],
  PropertyValue[ {population, 10}, VertexWeight]};
For[j = 1, j <= Length [l2],j++,myProp = Part[l,j];
If[Part[myProp,2]>10,arcd=VertexList[NeighborhoodGraph[population, j]];
arcd=Delete[arcd, 1];
monGain = Part[myProp,2];
For[i = 0, i <= Length [arcd],i++, m= Part[l2,Part[arcd,i]];If[Part[m,2]<monGain,myId=Part[myProp,1];myId=Part[myId,2];SecondID=Part[m,1];SecondID=Part[SecondID,2];AppendTo[EdgeToDelete,myId <-> SecondID]]];
 ]
]
```

<br>

<h2>Evolution</h2>
Finally it is time to see the evolution of the network after the various actions performed by users.
<br>

``` Mathematica
population = EdgeDelete[population,_];
film1 = FoldList[ Function[{g, arc}, EdgeAdd[g, arc]], population, EdgeToAdd];
population = EdgeAdd[population, EdgeToAdd];
film2 = FoldList[ Function[{g, arc}, EdgeDelete[g, arc]], population, EdgeToDelete];
BigFilm = Join[film1,film2];
```

<h2>Simulation</h2>
<p align="center">
  <img src="https://storage.googleapis.com/kainofreelancerpictures/anes/Nouvelle%20simulation%20%C3%A0%20deux%20cycles.gif" width="50%" title="logo">
</p>


<h2> How to use the code? </h2>
create a notbook mathematica copy paste the parts of code in the orde and execute as and then follow the evolution of the network.
<h2> Contribution and Acknowledgment </h2>
<h3> Project realized by: </h3>
- Anes Abdelfatah ABBAD.
<br>
- Amira KETFI.



