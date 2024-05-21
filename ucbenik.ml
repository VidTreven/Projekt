open Vdom


let obrni sez =
  let rec pomozna acc sez =
    match sez with
    | x :: xs -> pomozna (x :: acc) xs
    | [] -> acc
  in
  pomozna [] sez


(* DOLOCIMO MAX STEVILO POSKUSOV ZA CRKO *)
(*let max_stevilo_poizkusov = 2*)

type stanje = {
  crka : char;
  zaporedna_stevilka : int;
  poizkus : int;
  prikaz : string
}


(* NAREDIMO STANJA IZ BESEDE *)

let naredi_zacetna_stanja max =
  let rec pomozna acc max =
    let stanje =
      {
        crka = 's';
        zaporedna_stevilka = -1;
        poizkus = max;
        prikaz = ""
      } in
    if max > 0 then pomozna (stanje :: acc) (max-1)
    else acc
  in
  pomozna [] max

let naredi_stanje (p:int) (l:int) (beseda:string) =
    {
      crka = beseda.[l];
      zaporedna_stevilka = l;
      poizkus = p;
      prikaz = String.sub beseda 0 (l+1)
    }

let naredi_zadnje_stanje l beseda =
  {
    crka = beseda.[l];
    zaporedna_stevilka = l;
    poizkus = 1;
    prikaz = (String.sub beseda 0 (l+1))^" - Cestitke!"
  }

let dobi_stanja beseda st_poi =
  let max_stevilo_poizkusov = st_poi in
  let dolzina = String.length beseda in
  let rec pomozna poizkus lokacija acc beseda =
    match (poizkus, lokacija) with
    | (p,l) when (l + 1 = dolzina) && (p < 13 (*TODO*)) -> (naredi_zadnje_stanje l beseda) :: acc
    | (p,l) when (l = dolzina) && (p < 13 (*TODO*)) -> acc
    | (p,l) when (l < dolzina) && (p < max_stevilo_poizkusov) -> pomozna (p+1) l ((naredi_stanje p l beseda) :: acc) beseda
    | (p,l) when (l < dolzina) && (p = max_stevilo_poizkusov) -> pomozna 1 (l+1) ((naredi_stanje p l beseda) :: acc) beseda
    | _ -> failwith "negativna dolzina"
  in
  (naredi_zacetna_stanja max_stevilo_poizkusov) @ (obrni (pomozna 1 0 [] beseda))


(* OD TU NAPREJ PREHODNA FUNKCIJA *)

let sledece_pravilno_stanje (s : stanje) (seznam : stanje list) = 
  let ali_je_naslednji s0 s1 =
    (s1.zaporedna_stevilka = (s0.zaporedna_stevilka + 1)) && (s1.poizkus = 1)
  in
  List.find (ali_je_naslednji s) seznam

let ponovno_naslednje_stanje (s : stanje) (seznam : stanje list)  =
  let ali_je_naslednji s0 s1 =
    (s1.zaporedna_stevilka = s0.zaporedna_stevilka) && (s1.poizkus = (s0.poizkus + 1))
  in
  List.find (ali_je_naslednji s) seznam

(*
let prehodna_funkcija (seznam : stanje list) (s : stanje) (c : char) =
  let pravilna_crka = (sledece_pravilno_stanje s seznam).crka in
  if s.poizkus = max_stevilo_poizkusov then (sledece_pravilno_stanje s seznam)
  else (match c with
        | c when c = pravilna_crka -> (sledece_pravilno_stanje s seznam)
        | _ -> (ponovno_naslednje_stanje s seznam)
       )    
       
*)


(* IZBOLJSANA PREHODNA FUNKCIJA *)

type napaka = Napacno | Pravilno

type stanje_z_napako = 
{
  stanje : stanje;
  napaka : napaka
}

let mini_prehodna_funkcija max_st_poi (seznam : stanje list) (s : stanje) (c : char) =
  let pravilna_crka = (sledece_pravilno_stanje s seznam).crka in
  match c with
  | x when x = pravilna_crka -> {stanje = (sledece_pravilno_stanje s seznam); napaka = Pravilno}
  | x when x != pravilna_crka -> (
    if s.poizkus = max_st_poi then {stanje = (sledece_pravilno_stanje s seznam); napaka = Napacno}
    else {stanje = (ponovno_naslednje_stanje s seznam); napaka = Napacno}
  )
  | _ -> failwith "mini ne dela"

let ostanek (s : stanje) (niz : string) =
  let zacetek = s.zaporedna_stevilka + 1 in
  if (String.length niz) < zacetek then None else(
  if ((s.prikaz = (String.sub niz 0 zacetek)) = false) then None else(
    let dolzina = (String.length niz) - zacetek in
    if dolzina < 1 then None else
    Some (String.sub niz zacetek dolzina)))
  
let super_prehodna_funkcija max_st_poi (seznam : stanje list) (s : stanje) (niz : string) =
  match ostanek s niz with
  | None -> List.hd seznam
  | Some ostanek ->(
  let rec pomozna k (seznam : stanje list) (s : stanje) (niz : string) =
    if (k >= String.length niz) || (k >= (((List.length seznam - max_st_poi-1) / max_st_poi)+1)) then s else
    match (mini_prehodna_funkcija max_st_poi seznam s niz.[k]).napaka with
    | Pravilno -> pomozna (k+1) seznam ((mini_prehodna_funkcija max_st_poi seznam s niz.[k]).stanje) niz
    | Napacno -> (mini_prehodna_funkcija max_st_poi seznam s niz.[k]).stanje
  in
  pomozna 0 seznam s ostanek)



(* VZPOSTAVIMO CELOTEN SISTEM *)

type fsm = 
{
geslo : string;
stanja : stanje list;
zacetno_stanje : stanje;
prehodna_funkcija : stanje list -> stanje -> string -> stanje
}

type zagnanifsm = {sistem : fsm; trenutno_stanje : stanje}

let s0 =
  {
    crka = 's';
    zaporedna_stevilka = -1;
    poizkus = 1;
    prikaz = ""
  }

(* DODAJAM PRVO IN ZADNJO STRAN *)  

type uceca_beseda = 
{
  beseda : string;
  geslo : string
}

type trenutno_uceca_beseda =
{
  avtomat : zagnanifsm;
  indeks : int
}

type ucenje =
{
  gradivo_celotno : uceca_beseda list;
  trenutno_gradivo : trenutno_uceca_beseda;
  konec : bool;
  st_poizkusov : int
}

type rezultati = 
{
  uspesnost : string;
  ucenje : ucenje;
  st_poizkusov : int
}

let zmesaj_seznam sez =
  let tabela = Array.of_list sez in
  let rec pomozna stara novi_sez =
    let len = Array.length stara in
    match len with
    | 0 -> novi_sez
    | k when k > 0 -> (
                        let ran = Random.int k in
                        if ran = k-1 then
                          (let nova_stara = Array.sub stara 0 (ran) in
                          pomozna nova_stara (stara.(ran) :: novi_sez) )
                        else (
                          let sprednji_del = Array.sub stara 0 ran in
                          let zadnji_del = Array.sub stara (ran+1) (k-ran-1) in
                          let nova_stara = Array.append sprednji_del zadnji_del in
                          pomozna nova_stara (stara.(ran) :: novi_sez) )
                       )
    | _ -> failwith "nedela"
  in
  pomozna tabela []
  

type prikaz = Nastavitve of uceca_beseda list | Ugibanje of ucenje | Rezultati of rezultati

(* TUKAJ JE MSG *)

type msg_nastavitve = Nova_beseda of string * string | Zacni | Nepravilni_vzorec

type msg_ugib = Dodaj_niz of string | Naprej

type spreminjaj = Povecaj | Pomansaj | Ostani

type konec = {se_enkrat : bool; st_poizkusov : spreminjaj}

type msg = Prva_stran of msg_nastavitve | Druga_stran of msg_ugib | Zadnja_stran of konec


(* TUKAJ JE UPDATE *)

let zacni_z_ucenjem seznam st_poizkusov =
  let novi_seznam = zmesaj_seznam seznam in
  let zagnanifsm = {sistem = {geslo = (List.hd novi_seznam).geslo;
                              stanja = dobi_stanja (List.hd novi_seznam).beseda st_poizkusov ;
                              zacetno_stanje = s0;
                              prehodna_funkcija = super_prehodna_funkcija st_poizkusov};
                    trenutno_stanje = s0} in
                  let trenutno_uceca_beseda = {avtomat = zagnanifsm; indeks = 0} in
                  Ugibanje({gradivo_celotno = novi_seznam; trenutno_gradivo = trenutno_uceca_beseda;
                            konec = false; st_poizkusov = st_poizkusov})

let naslednje_gradivo snov =
  let sledeci_indeks = snov.trenutno_gradivo.indeks + 1 in
  if sledeci_indeks = List.length (snov.gradivo_celotno) then 
    ({
       gradivo_celotno = snov.gradivo_celotno;
       trenutno_gradivo = snov.trenutno_gradivo;
       konec = true;
       st_poizkusov = snov.st_poizkusov
     }) else(
    let sledece_gradivo = List.nth snov.gradivo_celotno sledeci_indeks in
    let zag_fsm = {sistem = {geslo = sledece_gradivo.geslo;
                            stanja = dobi_stanja sledece_gradivo.beseda snov.st_poizkusov;
                            zacetno_stanje = s0;
                            prehodna_funkcija = super_prehodna_funkcija snov.st_poizkusov};
                  trenutno_stanje = s0} in
    let trenutna_beseda = {avtomat = zag_fsm;
                          indeks = sledeci_indeks} in
      {
        gradivo_celotno = snov.gradivo_celotno;
        trenutno_gradivo = trenutna_beseda;
        konec = false;
        st_poizkusov = snov.st_poizkusov
      })

let update stari_model msg =
  match stari_model with
  | Ugibanje ucenje -> (
      match msg with
      | Druga_stran ugib_msg ->(
          match ugib_msg with
          | Dodaj_niz niz ->(
            let stanja = ucenje.trenutno_gradivo.avtomat.sistem.stanja in
            let trenutno_stanje = ucenje.trenutno_gradivo.avtomat.trenutno_stanje in
            let novo_stanje = super_prehodna_funkcija ucenje.st_poizkusov stanja trenutno_stanje niz in
            let avtomat = {sistem = ucenje.trenutno_gradivo.avtomat.sistem;
                           trenutno_stanje = novo_stanje} in
            let trenutno_uceca_beseda = {avtomat = avtomat;
                                         indeks = ucenje.trenutno_gradivo.indeks} in
            Ugibanje( {gradivo_celotno = ucenje.gradivo_celotno;
                                trenutno_gradivo = trenutno_uceca_beseda;
                                konec = false;
                                st_poizkusov = ucenje.st_poizkusov}))
          | Naprej -> (if (naslednje_gradivo ucenje).konec = false then Ugibanje(naslednje_gradivo ucenje) else
                          Rezultati {uspesnost = "Odlično"; ucenje = ucenje;
                                     st_poizkusov = ucenje.st_poizkusov})
          (*| Zacni_znova -> Ugibanje({sistem = ugib.sistem;
                            trenutno_stanje = s0}))*)
      )
      | Prva_stran _ -> failwith "napacni msg"
      | Zadnja_stran _ -> failwith "napacni msg"
   )
  | Nastavitve seznam -> (
    match msg with
    | Druga_stran _ -> failwith "napacni msg"
    | Prva_stran stran -> (
      match stran with
      | Nova_beseda (beseda,geslo) -> (
        let novi_sez = { beseda = beseda; geslo = geslo} :: seznam in
        Nastavitve novi_sez
        )
      | Zacni -> (zacni_z_ucenjem seznam 2)
      | Nepravilni_vzorec -> Nastavitve seznam
      )
      | Zadnja_stran _ -> failwith "napacni msg"
    )
    | Rezultati rezultati -> (
      match msg with
      | Zadnja_stran konec -> (if konec.se_enkrat = true then  (zacni_z_ucenjem rezultati.ucenje.gradivo_celotno rezultati.st_poizkusov) 
                                  else  (match konec.st_poizkusov with
                                        | Pomansaj -> (Rezultati{uspesnost = rezultati.uspesnost;
                                                                ucenje = rezultati.ucenje;
                                                                st_poizkusov = rezultati.st_poizkusov -1})
                                        | Povecaj -> (Rezultati{uspesnost = rezultati.uspesnost;
                                                                ucenje = rezultati.ucenje;
                                                                st_poizkusov = rezultati.st_poizkusov +1})
                                        | Ostani -> (Rezultati{uspesnost = rezultati.uspesnost;
                                                                ucenje = rezultati.ucenje;
                                                                st_poizkusov = rezultati.st_poizkusov})
      )
      )
      | _ -> failwith "se mi je zdelo"
    )

  
    (*| Nova_beseda (geslo, beseda) -> {sistem = {geslo = geslo;
                                    stanja = dobi_stanja beseda;
                                    zacetno_stanje = s0;
                                    prehodna_funkcija = super_prehodna_funkcija};
                          trenutno_stanje = s0} *)
          


(* TUKAJ JE VIEW *)

let razbij (beseda : string) =
  let sez = String.split_on_char '-' beseda in
  if (List.length sez > 2) then Prva_stran(Nepravilni_vzorec) else
  Prva_stran(Nova_beseda ((List.hd sez), (List.nth sez 1)))

let dodaj_niz (niz : string) = 
  Druga_stran (Dodaj_niz niz)

let prikazi_seznam (seznam: uceca_beseda list) =
  let rec pomozna niz seznam =
    match seznam with 
    | x :: [] -> x.beseda
    | [] -> niz
    | x :: xs -> (pomozna (niz) xs)^", "^x.beseda
  in
  match seznam with
  | [] -> "Vnesli še niste ustreznega gesla in besede."
  | _ -> pomozna "" seznam


let view zagnanifsm =
  match zagnanifsm with
  | Ugibanje ucenje ->(
      div
        [
          elt "h1" [ text ("Geslo: "^ucenje.trenutno_gradivo.avtomat.sistem.geslo) ];
          (*input[ ]
                ~a:[ onchange (fun niz -> razbij niz); ];*)
          elt "h2" [ text ("Beseda: "^ucenje.trenutno_gradivo.avtomat.trenutno_stanje.prikaz)];
          input[ ]
                ~a:[ onchange (fun niz -> dodaj_niz niz); disabled( String.contains ucenje.trenutno_gradivo.avtomat.trenutno_stanje.prikaz '!' )];
          elt "button" ~a:[ onclick (fun _ -> Druga_stran Naprej); disabled ( (String.contains ucenje.trenutno_gradivo.avtomat.trenutno_stanje.prikaz '!' = false)) ] [ text "Naslednja" ]
          (*elt "button" ~a:[ onclick (fun _ -> Zacni_znova) ] [ text "zacni znova" ]*)
        ]
  )
  | Nastavitve seznam -> (
    div
        [
          elt "h1" [ text ("Dobrodošli v programu za učenje besed!") ];
          elt "p" [ text "V polje vpišite besedo, ki se jo hočete naučiti, pomišljaj in nato geslo, ki vas bo spomnilo na dano besedo. Pritisnite Enter. ";
                    elt "br" [];
                    text "Da je program sprejel vašo besedo (in pripadajoče geslo), boste vedeli tako, da se bo vpisana beseda pojavila v seznamu 'Vaše besede'";
                    elt "br" [];
                    text "Če želite dodati še kakšno besedo, stvar preprosto ponovite. Ko boste vpisali vse željene dvojice (besedo in geslo), pritisnite gumb 'Začni z učenjem' " ];
          elt "p" [ text "PRIMER: Želite se naučiti angleške besede za barve mavrice. Pogledate v slovar in v polje vpišete 'red-rdeča'. Pritisnete enter.";
                    elt "br" [];
                    text "V seznamu vaših besed se pojavi beseda 'red'. Spet pogledate v slovar, vpišete 'orange-oranžna' in pritisnete enter. V seznamu vaših besed se poleg besede 'red' pojavi še beseda 'orange'. ";
                    elt "br" [];
                    text "Nadaljujete z vnosi in vnesete še preostale barve; vse se morajo pojaviti na seznamu vaših besed. Ko končate z vnosi, pritisnite na gumb 'Začni z učenjem'.";
                    elt "br" []];
          elt "h3" [ text "POZOR: Program bo sprejel vse kar je levo od pomišljaja (tudi presledke, razne črtice ipd.) za besedo, in vse kar je desno od pomišljaja za geslo.";
                     elt "br" [];
                     text "V primeru, da pomišljaja ne vpišete ali jih vpišete več kot enega, program vnosa ne bo sprejel."];
          elt "p" [ text "POTEK UČENJA: Ko pritisnete tipko 'Začni z učenjem', vam bo program prikazal geslo od ene izmed vpisanih besed. V polje vpišite, kar mislite, da je iskana beseda.";
                     elt "br" [];
                     text "Če boste vpisali pravilno besedo, vam bo program čestital in lahko boste šli na naslednjo besedo. V kolikor pa besede ne vpišete pravilno, vam program pomaga na sledeči način:";
                     elt "br" [];
                     text "Če ste pravilno napisali prvih nekaj znakov, se le ti pojavijo pod iskano besedo. Ti znaki naj vam bojo v pomoč, pri nadaljnem ugibanju. Vedno seveda vnašate celo besedo in ne zgolj ostanka.";
                     elt "br" [];
                     text "Za vsako črko (znak) besede imate na voljo 2 poizkusa. Če v 2. poizkusu ne vpišete pravilne črke (znaka), vam jo program izpiše sam (da ne izgubljate časa z ugibanjem).";
                     elt "br" [];
                     text "Število poizkusov za posamezno črko (znak) lahko kasneje nastavite sami (od 1 do 5).";
                     elt "br" [];
                     text "Če ze ugotovite prvih nekaj črk (znakov) besede in jih v naslednjem poizkusu ne upoštevate, torej vpišete nekaj drugega, kot veste da je prav,";
                     elt "br" [];
                     text "program to razume kot neracionalno dejanje in vas v želji po zbistritvi uma vrne na začetek iskanja te besede."];
          input[ ]
                ~a:[ onchange (fun niz -> razbij niz); ];
          elt "button" ~a:[ onclick (fun _ -> Prva_stran Zacni);
                          disabled (List.length seznam = 0) ] [ text "Začni z učenjem" ];
          elt "h2" [ text ("VAŠE BESEDE: "^prikazi_seznam seznam) ];]
  )
  | Rezultati rezultati -> (
    div
        [
          elt "h1" [ text (rezultati.uspesnost) ];
          elt "h1" [ text ("Število poizkusov za posamezno črko: "^string_of_int rezultati.st_poizkusov) ];
          elt "button" ~a:[ onclick (fun _ -> (Zadnja_stran {st_poizkusov = Povecaj; se_enkrat = false}));
           disabled (rezultati.st_poizkusov = 5) ] [ text "+" ];
          elt "button" ~a:[ onclick (fun _ -> (Zadnja_stran {st_poizkusov = Pomansaj; se_enkrat = false}));
           disabled (rezultati.st_poizkusov = 1) ][ text "-" ];
          elt "button" ~a:[ onclick (fun _ -> Zadnja_stran {st_poizkusov = Ostani; se_enkrat = true}) ] [ text "Poskusi ponovno" ]]
  )

(*let zacetni_sistem =
  {
    geslo = "crna";
    stanja = dobi_stanja "black";
    zacetno_stanje = s0;
    prehodna_funkcija = super_prehodna_funkcija
  }*)

(*let init = Ugibanje({ sistem = zacetni_sistem;
             trenutno_stanje = s0 })*)

let init = Nastavitve []

let app = simple_app ~init ~view ~update ()

let () =
  let open Js_browser in
  let run () =
    Vdom_blit.run app |> Vdom_blit.dom
    |> Js_browser.Element.append_child
         (match
            Js_browser.Document.get_element_by_id Js_browser.document
              "container"
          with
         | Some element -> element
         | None -> Js_browser.Document.document_element Js_browser.document)
  in
  Window.set_onload window run
