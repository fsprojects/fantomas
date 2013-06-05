module internal Fantomas.SourceFilter

// This module filters comments and compiler directives based on their locations
// and try to reattach those while pretty-printing.
//
// Comments will be preserved as follows:
//   1. Any number of commented lines before a let binding (skipping attributes) will be attached to that binding.
//   2. The same heuristic is done for type declarations and member bindings.
//   3. We would like to attach comments to patterns and expressions, but it's difficult to find out their boundaries
//   4. Any commented lines in the end of files will be copied to the results
//
// Tentative solution:
//  1. Lex the source file and obtain a token stream
//  2. At some keyword tokens such as 'let', 'type', 'member', etc try to go backwards to find comment tokens.
//  3. If we find some attributes, skip them
//  4. Find first comment token, go backwards until find a token of another kind (except whitespace tokens)
//  5. If found no comment token, no entry will be created
//  6. Add blocks of comments into a map keyed by locations of keyword tokens
// 
// Compiler directives need more thorough treatments.
// Some hindrances are (1) They may or may not have an else branch (2) They can be nested
// Compiler directives can be looked up by line numbers.
// The problem is to associate line numbers with the AST.

 

