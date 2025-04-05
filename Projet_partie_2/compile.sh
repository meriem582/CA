#!/bin/sh
set -e  # Arrêter le script en cas d'erreur

echo "🔄 Génération de l'analyseur lexical..."
ocamllex lexer.mll

echo "🔄 Compilation du fichier AST..."
ocamlc -c ast.ml

echo "🔄 Génération et compilation de l'analyseur syntaxique..."
menhir --infer --explain parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml

echo "🔄 Compilation du lexer..."
ocamlc -c lexer.ml

echo "🔄 Compilation du transformateur (compilateur Mini-ML -> CAM)..."
ocamlc -c transformateur.ml

echo "🔄 Compilation du main"
ocamlc -I +unix -c main.ml

echo "🔄 Liaison finale de l'exécutable..."
ocamlc -I +unix -o mini_ml unix.cma ast.cmo lexer.cmo parser.cmo transformateur.cmo main.cmo

echo "✅ Compilation terminée avec succès !"
