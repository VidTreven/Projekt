# Program za učenje besed 

Program vsebuje implementacijo končnih avtomatov, enega najpreprostejših računskih modelov.
Končni avtomat začne v enem izmed možnih stanj, nato pa glede na trenutno stanje in trenutni simbol preide v neko novo stanje.

## O programu

Program je narejen z namenom, da nam pomaga pri učenju besed (npr. tujih). Vanj vnesemo besede in njihove prevode (gesla), on pa nas nato, podobno kot učitelj, sprašuje po besedah. Za vsako besedo (oz. črko) nam da nekaj poizkusov, če pa vidi da nam ne gre, nam pomaga in nam sam razkrije kako črko. Tako stori za vsako besedo, ki bi se jo radi naučili. Seveda je priporočljivo, da vajo večkrat ponovimo, da znanje utrdimo, in se naučimo napisati vse besede brez pomoči programa. 

Program za vsako besedo posebaj naredi stanja na sledeč način: Za vsako (razen zadnjo) črko naredi toliko stanj, kot je na voljo poizkusov. Za zadnjo črko pa naredi samo eno stanje, saj pri zadnji črki ne ugibamo več. Ko vnesemo besedo, za katero mislimo da je prava, program za vsako črko posebaj preveri, če je prava, in glede na to nas avtomat postavi v določeno stanje.
Program seveda tudi preverja, če upoštevamo njegovo pomoč.

## Navodila za prevajanje

Nameščen morate imeti `dune` in knjižnico `vdom`, ki jo namestite z

    opam install vdom

Nato lahko projekt prevedete z

    dune build

kar ustvari Javascript datoteko `ucbenik.bc.js`, ki jo naložimo v `ucbenik.html`.

## Navodila za uporabo

Vsa potrebna navodila se nahajajo v spletnem vmesniku, ki ga odpremo z ukazom 
'open ucbenik.html'.
