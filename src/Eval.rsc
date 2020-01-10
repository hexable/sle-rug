module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

Value toValue(booleanType()) = vbool(false);
Value toValue(integerType()) = vint(0);
Value toValue(stringType()) = vstr("");

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
alias TestInput = list[Input];
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) = ( name : toValue(answerType) | /question(_, questionID(str name), AType answerType, _) <- f);

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  
  return venv;
}

  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
VEnv eval(question(_, questionID(str name), _, _), Input inp, VEnv venv) {
	
	return venv;
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(str x): return venv[x];
    
    // etc.
    
    default: throw "Unsupported expression <e>";
  }
}