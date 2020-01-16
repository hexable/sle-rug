module Eval

import AST;
import Exception;
import List;
import util::Math;

/*
 * Implement big-step semantics for QL
 */

// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vmoney(real m)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input = input(str question, Value \value);

// The default values per type
Value initialValueForType(booleanType()) = vbool(false);
Value initialValueForType(integerType()) = vint(0);
Value initialValueForType(moneyType()) = vmoney(0.0);
Value initialValueForType(stringType()) = vstr("");
default Value initialValueForType(_) {
	assert false : "Type not known";
	throw AssertionFailed();
}

// produce an environment which for each question has a default value
// Also do eval with those values so any calculated value work
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) 
	= eval(f, input("", vbool(false)), (
		questionId.name : initialValueForType(assignmentType)
		| /question(_, AId questionId, AType assignmentType, _) <- f
	));


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
	return solve (venv) {
		venv = evalOnce(f, inp, venv);
  	}
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) 
	= (venv | eval(q, inp, it) | q <- f.questions); 

VEnv eval(question(_, AId questionId, _, list[AExpr] expresion), Input inp, VEnv venv) {
	if (inp.question == questionId.name) {
		venv[questionId.name] = inp.\value;
	}
	if (!isEmpty(expresion)) {
		venv[questionId.name] = eval(expresion[0], venv);
	}
	return venv;
}
VEnv eval(\if(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] ifElseQuestion), Input inp, VEnv venv) {
	if (vbool(bool boolval) := eval(ifCondition, venv)) {
		if (boolval) {
			return eval(ifTrueQuestion, inp, venv);
		} else if (!isEmpty(ifElseQuestion)) {
			return eval(ifElseQuestion[0], inp, venv);
		}
		return venv;
	}
	assert false : "Could not parse if statement";
	throw AssertionFailed();
}
VEnv eval(block(list[AQuestion] questionBlock), Input inp, VEnv venv) 
	= (venv | eval(q, inp, it) | AQuestion q <- questionBlock);

default VEnv eval(_, Input inp, VEnv venv) {
	assert false : "Could not parse question";
	throw AssertionFailed();
}

Value eval(ref(AId x), VEnv venv) = venv[x.name];
Value eval(\int(int intValue), VEnv venv) = vint(intValue);
Value eval(\str(str stringValue), VEnv venv) = vstr(stringValue);
Value eval(\bool(bool boolean), VEnv venv) = vbool(boolean);
Value eval(money(real money), VEnv venv) = vmoney(money);
Value eval(expr(AExpr expression), VEnv venv) = eval(expression, venv);
Value eval(not(AExpr expression), VEnv venv) 
	= vbool(!evalbool) when
	vbool(evalbool) := eval(expression, venv);
Value eval(mul(AExpr leftExpression, AExpr rightExpression), VEnv venv) 
	= vint(vall * valr) when 
	vint(vall) := eval(leftExpression, venv), 
	vint(valr) := eval(rightExpression, venv);
Value eval(mul(AExpr leftExpression, AExpr rightExpression), VEnv venv) 
	= vmoney(vall * valr) when 
	real vall := evalNumber(leftExpression, venv), 
	real valr := evalNumber(rightExpression, venv);
Value eval(div(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vint(vall / valr) when 
	vint(vall) := eval(leftExpression, venv), 
	vint(valr) := eval(rightExpression, venv);
Value eval(div(AExpr leftExpression, AExpr rightExpression), VEnv venv) 
	= vmoney(vall / valr) when 
	real vall := evalNumber(leftExpression, venv), 
	real valr := evalNumber(rightExpression, venv);
Value eval(\mod(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vint(vall % valr) when 
	vint(vall) := eval(leftExpression, venv), 
	vint(valr) := eval(rightExpression, venv);
Value eval(add(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vint(vall + valr) when 
	vint(vall) := eval(leftExpression, venv), 
	vint(valr) := eval(rightExpression, venv);
Value eval(add(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vmoney(vall + valr) when 
	real vall := evalNumber(leftExpression, venv), 
	real valr := evalNumber(rightExpression, venv);
Value eval(sub(AExpr leftExpression, AExpr rightExpression), VEnv venv) 
	= vint(vall - valr) when 
	vint(vall) := eval(leftExpression, venv), 
	vint(valr) := eval(rightExpression, venv);
Value eval(sub(AExpr leftExpression, AExpr rightExpression), VEnv venv) 
	= vmoney(vall - valr) when 
	real vall := evalNumber(leftExpression, venv), 
	real valr := evalNumber(rightExpression, venv);
Value eval(gt(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall > valr) when 
	vall := evalNumber(leftExpression, venv), 
	valr := evalNumber(rightExpression, venv);
Value eval(gte(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall >= valr) when 
	vall := evalNumber(leftExpression, venv), 
	valr := evalNumber(rightExpression, venv);
Value eval(lt(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall < valr) when 
	vall := evalNumber(leftExpression, venv), 
	valr := evalNumber(rightExpression, venv);
Value eval(lte(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall >= valr) when 
	vall := evalNumber(leftExpression, venv), 
	valr := evalNumber(rightExpression, venv);
Value eval(equal(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall == valr) when 
	vbool(vall) := eval(leftExpression, venv), 
	vbool(valr) := eval(rightExpression, venv);
Value eval(equal(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall == valr) when 
	real vall := evalNumber(leftExpression, venv), 
	real valr := evalNumber(rightExpression, venv);
Value eval(equal(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall == valr) when 
	vstr(vall) := eval(leftExpression, venv), 
	vstr(valr) := eval(rightExpression, venv);
Value eval(notequal(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall != valr) when 
	vbool(vall) := eval(leftExpression, venv), 
	vbool(valr) := eval(rightExpression, venv);
Value eval(notequal(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall != valr) when 
	real vall := evalNumber(leftExpression, venv), 
	real valr := evalNumber(rightExpression, venv);
Value eval(notequal(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall != valr) when 
	vstr(vall) := eval(leftExpression, venv), 
	vstr(valr) := eval(rightExpression, venv);
Value eval(and(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall && valr) when 
	vbool(vall) := eval(leftExpression, venv), 
	vbool(valr) := eval(rightExpression, venv);
Value eval(or(AExpr leftExpression, AExpr rightExpression), VEnv venv)
	= vbool(vall || valr) when 
	vbool(vall) := eval(leftExpression, venv), 
	vbool(valr) := eval(rightExpression, venv);

default Value eval(AExpr e, VEnv venv) {
	assert false : "Invalid expression";
	throw AssertionFailed();
}

// Eval number since money can be either a int or a money value
real evalNumber(AExpr expression, VEnv venv) = val
	when vmoney(val) :=  eval(expression, venv);
real evalNumber(AExpr expression, VEnv venv) = toReal(val)
	when vint(val) :=  eval(expression, venv);
default real evalNumber(AExpr expression, VEnv venv) {
	assert false : "Invalid expression";
	throw AssertionFailed();
}