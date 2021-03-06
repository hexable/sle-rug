module Syntax

import ParseTree;

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id formId "{" Question* questions "}"; 

// A question is either a string question with a form, a if statement or a block of questions
syntax Question
  = Str questionString Id assignmentId ":" Type assignmentType ("=" Expr assignmentExpression)?
  | "if" "(" Expr ifCondition ")" Question ifTrueQuestion ('else' Question ifElseQuestion)?
  | "{" Question* "}" questionBlock
  ;

syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | Int
  | Str
  | Bool
  | Money
  > "(" Expr ")"
  > "!" Expr notExpression
  > left Expr leftExpression [*/%] 		expressionOperator Expr rightExpression // * / %
  > left Expr leftExpression [+\-] 		expressionOperator Expr rightExpression // + -
  > left Expr leftExpression [\<\>][=]? expressionOperator Expr rightExpression // < <= > >=
  > left Expr leftExpression [!=][=] 	expressionOperator Expr rightExpression // != ==
  > left Expr leftExpression "&&" 		expressionOperator Expr rightExpression
  > left Expr leftExpression "||" 		expressionOperator Expr rightExpression
  ;

syntax Type
  = "boolean"
  | "integer"
  | "money"
  | "string";  

// String definition with the ability to escape " with a \, and the ability to escape a \ with another \
lexical Str = "\"" ((![\"\\])|([\\]![]))* "\"";

// A interger should not start with a zero unless its the only number
lexical Int 
  = [0] | "-"?[1-9][0-9]*;
  
// Money is an extension of Int that can have two decimals
lexical Money
  = "-"?[0]"."[0-9][0-9]? | "-"?[1-9][0-9]*"."[0-9][0-9]?;
  
lexical Bool 
  = "true"
  | "false";
