module Check

import AST;
import Resolve;
import Message; // see standard library
import Set;
import List;
import IO;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

Type interpretType(AType t) {
	switch(t) {
		case integerType():
			return tint();
		case booleanType():
			return tbool();
		case stringType():
			return tstr();
		default:
			return tunknown();
	}
}

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  return {<questionID.src, questionID.name, name, interpretType(answerType)> | /question(str name, AId questionID, AType answerType, _) := f}; 
}

set[Message] check(AForm f) {
  graph = resolve(f);
 
 println(collect(f));
  
  ud = graph.useDef;
  return check(f, collect(f), ud);
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  visit(f) {
	case AQuestion q:
		msgs += check(q, tenv, useDef);
	case AExpr exp:
		msgs += check(exp, tenv, useDef);
  }

  return msgs;
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(question(str questionString, AId questionID, AType answerType, list[AExpr] answerExpressions), TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  Type questionType = interpretType(answerType);
  str questionName = questionID.name;

  // duplicate identifiers and different types declared
  TEnv duplicates = {};
  for(<loc def, questionName, str label, Type \type> <- tenv) {
    duplicates += <def, questionName, label, \type>;
  }
  
  if(size(duplicates) > 1) {
 	  for(d <- duplicates) {
        msgs += { warning("Duplicate identifier declared", d.def)};
        
        if(d.\type != questionType) {
          msgs += { error("Question declared again with different type", d.def)};
        }
      }
    }
  
  expectedType = interpretType(answerType);
  if(!isEmpty(answerExpressions) && !(expectedType == typeOf(answerExpressions[0], tenv, useDef))) {
    msgs += { error("Expected answer type is not equal to the type of the given answer expression", answerExpressions[0].src)};
  }
  
  return msgs;
}

set[Message] check(question(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] elseQuestions), TEnv tenv, UseDef useDef) {
  return check(ifCondition, tenv, useDef) + check(ifTrueQuestion, tenv, useDef) + check(question(elseQuestions), tenv, useDef);
}

set[Message] check(question(list[AQuestion] blockQuestions), TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};

  for(q <- blockQuestions) {
    msgs += check(q, tenv, useDef);
  }
  
  return msgs;
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };

	case mult(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };
  
    case div(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      
    
    case \mod(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };
      
    case add(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      
    
    case sub(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      
    
    case lessThan(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      
    
    case leq(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      
    
    case greaterThan(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      
    
    case greq(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      
    
    case notEqual(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      
    
    case equals(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };      

    case land(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };
	        
    case lor(AExpr leftExpr, AExpr rightExpr):
	  msgs += check(leftExpr, tenv, useDef) + check(rightExpr, tenv, useDef) + { error("Left expression is of different type than right expression", leftExpr.src) | typeOf(leftExpr, tenv, useDef) != typeOf(rightExpr, tenv, useDef) };  }
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(str x, src = loc u):
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }

	case \int(int exprInt):
	  return tint();
	  
	case \str(str exprStr):
	  return tstr();
	  
	case \bool(bool exprBool):
  	  return tbool();
  	  
    case notExpr(AExpr expr):
      if(typeOf(expr, tenv, useDef) == tbool()) {
        return tbool();
      } else {
      	return tunknown();
      }
      
    case mult(AExpr leftExpr, AExpr rightExpr): {
      println("Here");
      return (((typeOf(leftExpr) == tint()) && (typeOf(rightExpr()) == tint())) ? tint() : tunknown());
      }
    case div(AExpr leftExpr, AExpr rightExpr):
      return tint();
      
    case \mod(AExpr leftExpr, AExpr rightExpr):
      return tint();
      
    case add(AExpr leftExpr, AExpr rightExpr):
      return tint();
      
    case sub(AExpr leftExpr, AExpr rightExpr):
      return tint();
      
    case lessThan(AExpr leftExpr, AExpr rightExpr):
      return tbool();
      
    case leq(AExpr leftExpr, AExpr rightExpr):
      return tbool();
      
    case greaterThan(AExpr leftExpr, AExpr rightExpr):
      return tbool();
      
    case greq(AExpr leftExpr, AExpr rightExpr):
      return tbool();
      
    case notEqual(AExpr leftExpr, AExpr rightExpr):
      return tbool();
      
    case equals(AExpr leftExpr, AExpr rightExpr):
      return tbool();
      
    case land(AExpr leftExpr, AExpr rightExpr):
      return tbool();
      
    case lor(AExpr leftExpr, AExpr rightExpr):
      return tbool();
  }

  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(str x, src = loc u), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

