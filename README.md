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
-> Type est la population de l'utilisateur ça peut être  :
C : Un peu "Célèbre" souvent ils étaient des personnes standards et à force de se connecter avec beaucoup de gens ils sont devenu célèbre, ce type de population est connu dans quelques réseaux sociaux comme Facebook et Instagram sous le nom "influenceurs".
Cb : Célèbre de type B souvent des super stars qui fréquentent les réseaux sociaux pour être plus proches de leurs fans .
R : Des personnes "Riche" ils utilisent les réseaux sociaux pour trouver des opportunités financières comme par exemple trouver un célèbre pour faire la promotion de leurs produits.
S : Des personne standard qui fréquentent les réseaux sociaux pour différentes raisons :  trouver des amis, devenir célèbre ou juste un passe-temps.
-> la liste des demandes contient toutes les invitations envoyées par le sommet en question.
-> la liste de réception contient toutes les invitations reçues par le sommet en question.

<br>

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

<br>

```Mathematica
Accept[x_,y_]:=Module[{u},With[{r=0},u=r; 
Which[Part[x,1]=="C"&& Part[y,1]=="Cb",u:= r+ 1,Part[x,1]=="C"&& Part[y,1]=="C",u:= r+1,Part[x,1]=="C"&& Part[y,1]=="R",u:= r+1, Part[x,1]=="R"&& Part[y,1]=="R",u:=r+ 1,Part[x,1]=="R"&& Part[y,1]=="C",u:=r+ 1, Part[x,1]=="R"&& Part[y,1]=="Cb",u:=r+ 1,Part[x,1]=="R"&& Part[y,1]=="S"&&Part[y,2]>=10,u:= r+1,Part[x,1]=="S",u:=r+1];r];u]
```

<br>

```Mathematica
Accept[x_,y_]:=Module[{u},With[{r=0},u=r; 
Which[Part[x,1]=="C"&& Part[y,1]=="Cb",u:= r+ 1,Part[x,1]=="C"&& Part[y,1]=="C",u:= r+1,Part[x,1]=="C"&& Part[y,1]=="R",u:= r+1, Part[x,1]=="R"&& Part[y,1]=="R",u:=r+ 1,Part[x,1]=="R"&& Part[y,1]=="C",u:=r+ 1, Part[x,1]=="R"&& Part[y,1]=="Cb",u:=r+ 1,Part[x,1]=="R"&& Part[y,1]=="S"&&Part[y,2]>=10,u:= r+1,Part[x,1]=="S",u:=r+1];r];u]
```

<br>
Après chaque lien établi on doit impérativement mettre à jour le gain des deux individus
<br>


```Mathematica
Type[x_]/; x>15&& x<20 := "R"\[IndentingNewLine]Type[x_]/;x> 20 := "C"\[IndentingNewLine]Type[x_]/; x<15 := "S"
(*deffinition de la fonction NewGain : elle attribue a chaque indivdu son nouveau gain *)
NewGain[x_,n_]:= Module[{r,u},\[IndentingNewLine]Which[x=="R",u:=Part[n,2]+2,x=="S",u:=Part[n,2]+1,x=="C",u:=Part[n,2]+3,x=="Cb"&&Part[n,1]=="R",u=Part[n,2]+3+2,x=="Cb"&&Part[n,1]=="S",u=Part[n,2]+3+1,x=="Cb"&&Part[n,1]=="C",u=Part[n,2]+3+3];u];

```


<h2>Comment utiliser le code ?</h2>
1. Clonez le projet et decompressez code.rar
<br>
2. Dans le dossier Dataset vous trouverez les dataset du projet
<br>
3. vous trouvez un fichier code.py pour visualiser les données et toutes les étapes de la réalisation du projet.
<h2>Contribution et Remerciement</h2>
<h3>Projet réalisé par :</h3>
- Anes Abdelfatah ABBAD.
<br>
- Amira KETFI.
