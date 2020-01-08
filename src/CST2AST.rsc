module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  if((Form) `form <Id id> { <Question* questions> }` := f) {
  	return form("<id>", [cst2ast(q) | q <- questions], src=f@\loc);
  }
}

AQuestion cst2ast(Question q) {
  switch(q) {
    case (Question) `<Str questionString> <Id questionID> : <Type answerType> = <Expr answerExpr>`:
  	  return question("<questionString>", cst2ast(questionID), cst2ast(answerType), [cst2ast(answerExpr)], src=questionString@\loc);
  	
  	case (Question) `<Str questionString> <Id questionID> : <Type answerType>`:
  	  return question("<questionString>", cst2ast(questionID), cst2ast(answerType), [], src=questionString@\loc);
  	    	  
  	case (Question) `if ( <Expr ifExpr> ) <Question ifQuestion>`:
  	  return question(cst2ast(ifExpr), cst2ast(ifQuestion), [], src=ifExpr@\loc);
  	  
  	case (Question) `if ( <Expr ifExpr> ) <Question ifQuestion> else <Question elseQuestion>`:
  	  return question(cst2ast(ifExpr), cst2ast(ifQuestion), [cst2ast(elseQuestion)], src=ifExpr@\loc);
  	    	  
  	case (Question) `{ <Question* blockQuestions> }`:
  	  return question([cst2ast(qq) | qq <- blockQuestions]);
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(questionID("<x>", src=x@\loc), src = x@\loc);
    case (Expr)`<Int x>`: return \int(toInt("<x>"), src=x@\loc);
    case (Expr)`<Str s>`: return \str("<s>", src=s@\loc);
    case (Expr)`<Bool b>`: return \bool("<b>" == "true", src=b@\loc);
    case (Expr)`( <Expr parenthExp> )`: return expr(cst2ast(parenthExp, src=parenthExp@\loc), src=parenthExp@\loc);
    case (Expr)`! <Expr notExp>`: return notExpr(cst2ast(notExp), src=notExp@\loc);
    case (Expr)`<Expr leftExpr> * <Expr rightExpr>`: return mult(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> / <Expr rightExpr>`: return div(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> % <Expr rightExpr>`: return \mod(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> + <Expr rightExpr>`: return add(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> - <Expr rightExpr>`: return sub(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> \< <Expr rightExpr>`: return lessThan(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> \<= <Expr rightExpr>`: return leq(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> \> <Expr rightExpr>`: return greaterThan(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> \>= <Expr rightExpr>`: return greq(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> != <Expr rightExpr>`: return notEqual(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> == <Expr rightExpr>`: return equals(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> && <Expr rightExpr>`: return land(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    case (Expr)`<Expr leftExpr> || <Expr rightExpr>`: return lor(cst2ast(leftExpr, src=leftExpr@\loc), cst2ast(rightExpr, src=rightExpr@\loc), src=leftExpr@\loc);
    
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch(t) {
  	case (Type)`boolean`: return booleanType();
  	case (Type)`integer`: return integerType();
  	case (Type)`string`: return stringType();
  }
}

AId cst2ast(Id x) {
  return questionID("<x>", src=x@\loc);
}
