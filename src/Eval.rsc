module Eval

import AST;
import Resolve;
import List;
import IO;

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
  return (venv | eval(q, inp, it) | q <- f.questions);
}

  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
VEnv eval(question(_, questionID(str name), _, list[AExpr] answerExpressions), Input inp, VEnv venv) {
	if(inp.question == name) {
	  venv[name] = inp.\value;
	}
	
	if(!isEmpty(answerExpressions)) {
	  venv[name] = eval(answerExpressions[0], venv);
	}

	return venv;
}

VEnv eval(\if(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] elseQuestions), Input inp, VEnv venv) {
	if(eval(ifCondition, venv) == vbool(true)) {
	  return eval(ifTrueQuestion, inp, venv);
	} else if(!isEmpty(elseQuestions)) {
	  return eval(elseQuestions[0], inp, venv);
	}
	
	return venv;
}

VEnv eval(block(list[AQuestion] blockQuestions), Input inp, VEnv venv) {
	return (venv | eval(q, inp, it) | q <- blockQuestions);
}

Value eval(ref(questionID(str name)), venv) = venv[name];	 	 	
Value eval(\int(int e), venv) = vint(e);
Value eval(\str(str e), venv) = vstr(e);
Value eval(\bool(bool e), venv) = vbool(e);
Value eval(expr(AExpr expr), venv) = eval(expr, venv);	 	 	
Value eval(notExpr(AExpr expr)) = vbool(eval(expr) == vbool(true));

Value eval(greaterThan(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool((lexpInt > rexpInt) == true)
	when vint(int lexpInt) := eval(leftExpr, venv),
	 	 vint(int rexpInt) := eval(rightExpr, venv);

Value eval(lessThan(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool((lexpInt < rexpInt) == true)
	when vint(int lexpInt) := eval(leftExpr, venv),
	 	 vint(int rexpInt) := eval(rightExpr, venv);

Value eval(leq(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool((lexpInt <= rexpInt) == true)
	when vint(int lexpInt) := eval(leftExpr, venv),
	 	 vint(int rexpInt) := eval(rightExpr, venv);

Value eval(geq(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool((lexpInt >= rexpInt) == true)
	when vint(int lexpInt) := eval(leftExpr, venv),
	 	 vint(int rexpInt) := eval(rightExpr, venv);

Value eval(notEqual(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool((lexpInt != rexpInt) == true)
	when vint(int lexpInt) := eval(leftExpr, venv),
	 	 vint(int rexpInt) := eval(rightExpr, venv);
	 	 
Value eval(notEqual(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool((lexpStr != rexpStr) == true)
	when vstr(str lexpStr) := eval(leftExpr, venv),
	 	 vstr(str rexpStr) := eval(rightExpr, venv);
	 	 
Value eval(notEqual(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool((lexpBool != rexpBool) == true)
	when vbool(bool lexpBool) := eval(leftExpr, venv),
	 	 vbool(bool rexpBool) := eval(rightExpr, venv);

Value eval(equals(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool(lexp == rexp)
	when vint(int lexpInt) := eval(leftExpr, venv),
	 	 vint(int rexpInt) := eval(rightExpr, venv);
	 	 
Value eval(equals(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool(lexp == rexp)
	when vstr(str lexp) := eval(leftExpr, venv),
	 	 vstr(str rexp) := eval(rightExpr, venv);	 	 

Value eval(equals(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool(lexp == rexp)
	when vbool(bool lexp) := eval(leftExpr, venv),
	 	 vbool(bool rexp) := eval(rightExpr, venv);	 
	 	 
Value eval(mult(AExpr leftExpr, AExpr rightExpr), venv) = 
	vint(lexp * rexp)
	when vint(int lexp) := eval(leftExpr, venv),
	 	 vint(int rexp) := eval(rightExpr, venv);

Value eval(div(AExpr leftExpr, AExpr rightExpr), venv) = 
	vint(lexp / rexp)
	when vint(int lexp) := eval(leftExpr, venv),
	 	 vint(int rexp) := eval(rightExpr, venv);

Value eval(\mod(AExpr leftExpr, AExpr rightExpr), venv) = 
	vint(lexp % rexp)
	when vint(int lexp) := eval(leftExpr, venv),
	 	 vint(int rexp) := eval(rightExpr, venv);
	 	 
Value eval(add(AExpr leftExpr, AExpr rightExpr), venv) = 
	vint(lexp + rexp)
	when vint(int lexp) := eval(leftExpr, venv),
	 	 vint(int rexp) := eval(rightExpr, venv);

Value eval(sub(AExpr leftExpr, AExpr rightExpr), venv) = 
	vint(lexp - rexp)
	when vint(int lexp) := eval(leftExpr, venv),
	 	 vint(int rexp) := eval(rightExpr, venv);

Value eval(land(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool(lexp && rexp)
	when vbool(bool lexp) := eval(leftExpr, venv),
	 	 vbool(bool rexp) := eval(rightExpr, venv);

Value eval(lor(AExpr leftExpr, AExpr rightExpr), venv) = 
	vbool(lexp || rexp)
	when vbool(bool lexp) := eval(leftExpr, venv),
	 	 vbool(bool rexp) := eval(rightExpr, venv);