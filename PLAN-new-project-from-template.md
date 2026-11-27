# Piano: "New Project From Template"

## Context

Oggi `File > New Project...` (`browser#dialog_project_new`, `src/browser.ml:326`) crea un progetto vuoto
(`Project.create`) e apre la finestra Project Properties (`Project_properties.create`), dove l'utente deve
costruire a mano i target (aggiunge un target vuoto se non ce ne sono). Si vuole partire da modelli pronti:

1. **Basic Executable**: un eseguibile, un sorgente.
2. **Basic Library**: una libreria, un sorgente.
3. **Executable and library**: eseguibile + libreria nella stessa cartella `src/`.
4. **Advanced Executable**: eseguibile in `src/` con la libreria in una cartella a parte (`src/<lib>/`),
   come fa OCamlEditor stesso (`common/`, `gmisclib/`, ... vedi `ocamleditor.project.json`).

**Costo stimato: basso-medio.** Circa 300 righe OCaml in 2 file nuovi, ritocchi di poche righe a
`browser.ml`, `menu_file.ml`. Nessuna modifica al modello (`Prj`, `Target`, `Rconf`) né al formato del progetto.
Il lavoro vero è generare target/dipendenze/run config giusti, non l'interfaccia.

Scelte confermate dall'utente: dialogo template seguito dalle Project Properties già compilate; nessun
file dune generato (resta `Project > Generate dune Files`).

## Cosa esiste già e si riusa

- `Project.create ~filename ()` (`src/project.ml:87`): progetto vuoto con le impostazioni di default.
- `Target.create ~id ~name` (`src/target.ml`), `Rconf.create ~id ~name ~target_id` (`src/rconf.ml:35`).
- `Project_properties.create ~editor ~new_project ~callback` (`src/project_properties.ml:213`): al reset
  legge `project.targets` e `project.executables`; aggiunge un target vuoto **solo se** `targets = []`, quindi
  un progetto pre-popolato con i target del template non viene toccato. `self#save` riscrive targets/executables
  dalle viste e chiama `callback`, poi `Project.save`.
- `Project.save` (`src/project.ml:353`): crea `root/`, `src/`, `bak/`, `.tmp/`, scrive il JSON. Non crea sottocartelle di `src/`.
- Modello di riferimento per target libreria + eseguibile: `ocamleditor.project.json`: libreria con
  `files "lib/lib.ml"`, `includes "lib"`, `outname "lib/lib"`, `target_type Library`; eseguibile con
  `libs "lib"`, `includes "lib"`, `dependencies [id della libreria]`.
- Pattern di dialogo modale con pulsanti: `Menu_file` (finestra "Open Remote File...") e
  `GWindow`/`GPack.button_box` usati altrove. Comando di menu: `Command.register` + `Menu_builder`.

## Piano

### 1. `src/project_template.ml` (nuovo, logica senza GUI)

- Tipo `t = { id; name; description; apply : Prj.t -> unit; files : name:string -> (string * string) list }`:
  `apply` pre-popola `targets` ed `executables` del progetto; `files` restituisce (percorso relativo a `src/`, contenuto).
- `all : t list` con i 4 template. Per ciascuno, targets e run config:
  - **Basic Executable**: 1 target `Executable` (`files "main.ml"`, `byt` e `opt` come gli altri progetti, `default`), 1 `Rconf` di default. File: `main.ml` con `let () = print_endline "Hello, world!"`.
  - **Basic Library**: 1 target `Library` (`files "<name>.ml"`, `outname "<name>"`). File: `<name>.ml` e `<name>.mli`.
  - **Executable and library**: target Library + target Executable con `libs "<name>"` e `dependencies [id lib]`, entrambi in `src/`. File: `<name>.ml`, `main.ml` che la usa.
  - **Advanced Executable**: come sopra ma con la libreria in `src/<name>_lib/` (`files "<name>_lib/<name>_lib.ml"`, `includes "<name>_lib"`, `outname "<name>_lib/<name>_lib"`), l'eseguibile con `includes "<name>_lib"`.
- `write_files project template`: crea le sottocartelle necessarie sotto `root/src/` e scrive i file solo se non esistono.
  Il nome dei target deriva dal nome del progetto, quindi i target si costruiscono quando il nome è noto (vedi sotto).

### 2. `src/dialog_project_template.ml` (nuovo, GUI)

- Dialogo modale: lista dei template (radio o `GTree` a una colonna) con descrizione a lato, campo **Name**,
  scelta della **cartella** (`GWindow.file_chooser_dialog` come in `Project_properties`), OK/Cancel.
- Restituisce `(template, name, parent_dir)` o `None`. Controlla che la cartella `parent_dir/name` non esista già.

### 3. `browser.ml`: nuovo metodo `dialog_project_new_from_template`

Stessa struttura di `dialog_project_new` (`src/browser.ml:326`), con questa differenza:
1. mostra il dialogo template;
2. `Project.create ~filename:(dir/name/name.project.json)`, imposta `name`, chiama `template.apply`;
3. apre `Project_properties.create ~new_project` con la stessa `callback` di oggi;
4. nella `callback`, **dopo** `Project.save` (che crea `src/`), chiama `Project_template.write_files`, poi `project_open`.

Il codice duplicato con `dialog_project_new` si riduce estraendo la parte comune in un metodo privato.

### 4. Comando di menu

In `src/menu_file.ml`: nuovo comando `file.new_project_from_template`, etichetta "New Project from Template...",
subito dopo "New Project...". Richiede solo un `Command.register` e una riga nella lista del menu.

## Rischi e punti da verificare

- **Nome del target/libreria**: `Target.name` è libero, ma `outname` e `files` devono essere coerenti col nome del progetto.
  Va fissato come "nome del progetto, senza spazi, minuscolo" (sanitizzare per i moduli OCaml: lettera iniziale minuscola, solo `[a-z0-9_]`).
- **Ordine di compilazione**: l'eseguibile deve avere la libreria in `dependencies`, altrimenti `oebuild` non la costruisce prima.
- **Conflitto con il nome del modulo**: il template non deve generare un modulo che si chiami come un modulo della stdlib (es. `list.ml`);
  rifiutare o prefissare in tal caso.
- **Edit del nome nelle Properties**: se l'utente cambia il nome dopo aver scelto il template, target e file restano coerenti con il nome iniziale
  (accettabile: sono modificabili nella stessa finestra).
- **Progetto già aperto**: `dialog_project_new` chiude il progetto corrente nella `callback`; la nuova funzione deve fare lo stesso.

## Verifica

1. Compilare con `ocaml build.ml ocamleditor` (dalla radice, come indicato dall'utente).
2. Per ciascuno dei 4 template: File > New Project from Template..., scegliere nome e cartella, confermare le Properties, poi
   verificare su disco la struttura (`src/main.ml`, `src/<lib>/...`) e il contenuto di `<name>.project.json` (target, `dependencies`, `libs`, `includes`).
3. Nel progetto creato: Project > Build (Ctrl+Maiusc+B) e Project > Run (F5). Verificare che l'eseguibile stampi il messaggio e che le librerie vengano costruite prima.
4. Riaprire il progetto da File > Recent o Open Project per controllare che il JSON si ricarichi senza avvisi.
5. Provare nome già esistente, nome con spazi e nome che coincide con un modulo della stdlib.
