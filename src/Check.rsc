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
          msgs += { error("Qestion declared again with different type", d.def)};
        }
      }
    }
  
  expectedType = interpretType(answerType);
  if(!isEmpty(answerExpressions) && !(expectedType == typeOf(answerExpressions[0], tenv, useDef))) {
    msgs += { error("Question answer type is not equal to the answer expression type")};
  }
  
  return msgs;
}

set[Message] check(question(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] elseQuestions), TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};

  return msgs;
}

set[Message] check(question(list[AQuestion] blockQuestions), TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};

  return msgs;
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  /*
  switch (e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };

    // etc.
  }
  */
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(str x, src = loc u):
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    // etc.
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
 
 

