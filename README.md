<h1>social-media-network-simulation</h1>
<p align="center">
  <img src="https://storage.googleapis.com/kainofreelancerpictures/anes/sma.png" width="50%" title="logo">
</p>
Le but de ce projet est de modéliser un petit réseau social en utilisant les techniques de la théorie de jeux.
Ce réseau social qui est constitué principalement d'individus voulant établir des liens entre eux pour des intérêts  personnels ou professionnels. La connexion produite après l'établissement  d'un lien entre deux individus fait changer la structure du réseau social et le fait évoluer grâce  à quelques actions permises dans le réseau comme :
<br>
- Ajouter une personne
<br>
- Accepter / refuser une personne
<br>
- Supprimer un lien d'amitier
<br>
Les utilisateurs du réseau utilisent ces actions pour améliorer leurs gains selon différentes politiques pour chaque population dans le réseau.



<h2> Outils : </h2>
Walforme Mathematica

<h2>Définition du Réseau</h2>
Soit  « Population » un graphe qui représente une population constituée des : Utilisateurs standards, Riches et Célèbres.
Tel que : chaque sommet du graphe "population" représente un utilisateur. Et les liens entre les sommets représentent la connexion entre deux utilisateurs. 
Un utilisateur est identifié par:
<br>
Id, Type, Gain, Liste des demande,s Liste des invitations reçus
<br>
-> le ID est un identifiant unique qui représente l'individu  dans le réseau.
<br>
-> Type est la population de l'utilisateur ça peut être  :
<br>
C : Un peu "Célèbre" souvent ils étaient des personnes standards et à force de se connecter avec beaucoup de gens ils sont devenu célèbre, ce type de population est connu dans quelques réseaux sociaux comme Facebook et Instagram sous le nom "influenceurs".
<br>
Cb : Célèbre de type B souvent des super stars qui fréquentent les réseaux sociaux pour être plus proches de leurs fans .
<br>
R : Des personnes "Riche" ils utilisent les réseaux sociaux pour trouver des opportunités financières comme par exemple trouver un célèbre pour faire la promotion de leurs produits.
<br>
S : Des personne standard qui fréquentent les réseaux sociaux pour différentes raisons :  trouver des amis, devenir célèbre ou juste un passe-temps.
<br>
-> la liste des demandes contient toutes les invitations envoyées par le sommet en question.
<br>
-> la liste de réception contient toutes les invitations reçues par le sommet en question.

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
Ainsi nous obtenons notre réseau initial  :

<p align="center">
  <img src="https://storage.googleapis.com/kainofreelancerpictures/anes/network.png" width="50%" title="logo">
</p>

<h2>Politique</h2>
Chaque type de population suit une stratégie pour améliorer le gain. surtout pour accepter une invitation de connexion.
Chaque utilisateur doit  s'assurer que le nouvel lien établi avec un individu est utile pour améliorer son gain.
Nous avons définis la fonction "Accepte" afin d'implémenter notre politique de gain.
<br>
En effet  elle prend en paramètre deux individus x et y tel que : x a reçu une invitation de y , et elle retourne si x devrait accepter l'invitation de y en respectant les règles suivantes : 
<br>
-  Un individu de la population "C "(Célèbre)  peut accepter les individus qui sont de population "R" (Riche) et "Cb" (Célèbre de type béta).
<br>
- Un individu de la population "Cb"(Célèbre de type béta)  peux accepter les individus qui sont de population "R" (Riche) , "C" (Célèbre) et ceux de type "S" ( Standard)
<br>
- Un individu de la population "R"(Riche)  peux accepter les individus qui sont de population "Cb" (Célèbre de type béta), "C" (Célèbre) et ceux de type "S" (Standard) dont leurs gain est supérieur à 10
<br>
- Et enfin la population "S" peut accepter tous les individus.



<br><br>

```Mathematica
Accept[x_,y_]:=Module[{u},With[{r=0},u=r; 
Which[Part[x,1]=="C"&& Part[y,1]=="Cb",u:= r+ 1,Part[x,1]=="C"&& Part[y,1]=="C",u:= r+1,Part[x,1]=="C"&& Part[y,1]=="R",u:= r+1, Part[x,1]=="R"&& Part[y,1]=="R",u:=r+ 1,Part[x,1]=="R"&& Part[y,1]=="C",u:=r+ 1, Part[x,1]=="R"&& Part[y,1]=="Cb",u:=r+ 1,Part[x,1]=="R"&& Part[y,1]=="S"&&Part[y,2]>=10,u:= r+1,Part[x,1]=="S",u:=r+1];r];u]
```

<br>
Après chaque lien établi on doit impérativement mettre à jour le gain des deux individus
<br><br>


```Mathematica
Type[x_]/; x>15&& x<20 := "R"\[IndentingNewLine]Type[x_]/;x> 20 := "C"\[IndentingNewLine]Type[x_]/; x<15 := "S"
(*deffinition de la fonction NewGain : elle attribue a chaque indivdu son nouveau gain *)
NewGain[x_,n_]:= Module[{r,u},\[IndentingNewLine]Which[x=="R",u:=Part[n,2]+2,x=="S",u:=Part[n,2]+1,x=="C",u:=Part[n,2]+3,x=="Cb"&&Part[n,1]=="R",u=Part[n,2]+3+2,x=="Cb"&&Part[n,1]=="S",u=Part[n,2]+3+1,x=="Cb"&&Part[n,1]=="C",u=Part[n,2]+3+3];u];

```
<br>
<h2>Envoi des invitations</h2>
Comme nous avons mentionné plus haut chaque individu dans le réseau cherche à améliorer son gain. peu importe le type de la population et sa politique de gain. Établir  de nouveaux lien avec des utilisateur peut que améliorer le gain. 
Pour cela chaque utilisateur envoit des invitations de connexion à chaque utilisateur qui possède un gain supérieur ou égale à son gain.
ainsi chaque utilisateur assure une bonne qualité de lien avec ces futurs amis.
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
<h2>Accepter les invitations:</h2>
Après l'étape d'envois des invitations , il est temps d'accepter les invitations reçus.
Cette fois chaque type de population suit une logique différente  des autres comme nous avons mentionnés dans la section politique de gain afin d'améliorer son gain ou satisfaire ses intérêts  personnels ou professionnel.
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
<h2>Suppression : </h2>
il est des fois nécessaire de supprimer quelques lien de connexions si par exemple le Seuil de nombre maximal d'amis est atteint et nous voulons se connecter avec d'autres personnes avec lesquels nous avons plus d'intérêts.
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

<h2>Évolution</h2>
Enfin il est temps de voir l'évolution du réseau après les différentes actions réalisés par les utilisateurs.
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


<h2>Comment utiliser le code ?</h2>
créez un notbook mathematica copier coller les parties de code dans l'orde et executer au fur et a mesure puis suivre l'évolution du réseau. 
<h2>Contribution et Remerciement</h2>
<h3>Projet réalisé par :</h3>
- Anes Abdelfatah ABBAD.
<br>
- Amira KETFI.



