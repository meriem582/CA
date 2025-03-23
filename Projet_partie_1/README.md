# README - Machine Virtuelle Lua 5.1 en OCaml

## 📌 Description du projet
Ce projet implémente une machine virtuelle (VM) en OCaml pour exécuter du bytecode Lua 5.1. Il prend en entrée des fichiers `.luac`, effectue un `undump` pour récupérer la représentation du bytecode, puis exécute ce bytecode avec la VM.

Après l'exécution de `make all`, tous les fichiers `.luac` présents dans le répertoire `test/` sont traités automatiquement. Le résultat est généré sous la forme suivante :
- **Dumps des fichiers `.luac`** sont sauvegardés dans `resultat_dump/`
- **Résultats de l'undump** sont écrits dans `resultat_undump/`
- **Exécution des fichiers `.luac`** est effectuée en utilisant la machine virtuelle implémentée et affichée sur la console

## 📂 Structure du projet
```
Projet_partie_1/
│── src/                # Code source de la machine virtuelle
│── test/               # Fichiers .luac à tester
│── resultat_dump/      # Fichiers dumpés après traitement
│── resultat_undump/    # Résultats des undumps
│── README.md           # Documentation du projet
│── Makefile            # Compilation et exécution automatique
```

## 🚀 Compilation et exécution

### ℹ️ Prérequis
Avant d’exécuter les programmes, assurez-vous d’avoir installé :
* **OCaml Compiler** : Vous avez besoin du compilateur OCaml installé sur votre système.
* **Gestionnaire de paquets** : Pour gérer les dépendances de vos projets OCaml, vous pouvez utiliser `opam`.
* **IDE recommandé** : Installez un IDE comme **VS Code** avec l'extension **OCaml Platform** pour une meilleure expérience de développement.

### 🛠️ Compilation et Nettoyage
1. Extraire l'archive ZIP du projet.
2. Se placer dans le dossier du projet :
   ```sh
   cd Projet_partie_1
   ```
3. Pour traiter tous les fichiers .luac dans test/ 
   ```sh
   make run
   ```
4. Pour traiter un seul fichier spécifique :
   ```sh
   make run-file FILE=./test/mon_fichier.luac
   ```

5. Nettoyer les fichiers générés :
   ```sh
   make clean
   ```

### ▶️ Exécution
Après `make all`, tous les fichiers `.luac` situés dans `test/` seront traités.
Le format de sortie affiché est par exemple:
```sh
======================Traitement du fichier test7.luac=======================
Undump result est écrit dans : ./resultat_undump/undump_test7.luac.txt
Bytecode écrit dans : ./resultat_dump/processed_test7.luac
Résultat :*********************************
Alice
25.
Paris
*******************************************
Execution terminée
```

## 🔥 Fonctionnalités principales
- **Undump** : Lecture et conversion du bytecode Lua 5.1 en une structure exploitable en OCaml
- **Dump** : Génération du bytecode à partir de la structure interne
- **Exécution avec la VM** : Exécution du bytecode grâce à une machine virtuelle implémentée en OCaml

## 👨‍💻 Auteur
Projet réalisé dans le cadre du cours de **Compilation Avancée**.

## 📜 Licence
Ce projet est sous licence MIT.

