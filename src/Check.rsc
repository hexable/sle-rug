module Check

import AST;
import Resolve;
import List;
import Message; // see standard library

data Type
  = tint()
  | tmoney()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` )
TEnv collect(AForm f) 
	= {
		<questionId.src, questionId.name, questionString, typeOfAType(assignmentType)> 
		| /question(str questionString, AId questionId, AType assignmentType, _) <- f
	};

set[Message] check(AForm f, TEnv tenv, UseDef useDef)
	= {
		*check(q, tenv, useDef)
		| /AQuestion q <- f
	} + {
		*check(e, tenv, useDef)
		| /AExpr e <- f
	};

set[Message] check(question(str label, AId qid, AType assignmentType, list[AExpr] expresion), TEnv tenv, UseDef useDef) {
	set[Message] msgs = {};
	str name = qid.name;
	loc qidsrc = qid.src;
	Type questionType = typeOfAType(assignmentType);
	// Check in the env on same named questions
	for (<loc def, name, _, Type \type> <- tenv, \type != questionType){
		msgs += error("Question \"<name>\" is also defined with a different type, at: <def>",qid.src);
	}
	// Check in the env on same named labels
	for (<loc def, str tname, label, _> <- tenv, tname != name){
		msgs += warning("There already exists a question with the same label for a different id, at: <def>", qid.src);
	}
	// Check in the env on same named labels
	for (<loc def, name, str qlabel, _> <- tenv, qlabel != label){
		msgs += warning("There already exists a question with a different label for the same id, at: <def>", qid.src);
	}
	if (isEmpty(expresion)) {
		return msgs;
	}
	Type expressionType = typeOf(expresion[0], tenv, useDef);
	if (questionType == tmoney()) {
		if (expressionType notin [tint(), tmoney()]) {
			msgs += error("Expression does not match with type", expresion[0].src);
		}
	} else if (expressionType notin [questionType, tunknown()]) {
		msgs += error("Expression does not match with type", expresion[0].src);
	}
	
	return msgs;
}

set[Message] check(\if(AExpr ifCondition, _, _,_), TEnv tenv, UseDef useDef)
 	= {error("Condition is not of a boolean type", ifCondition.src)}
	when typeOf(ifCondition, tenv, useDef) notin [tbool(), tunknown()];
	
// Block not needed since all questions are suplied through a visit
default set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) = {};

set[Message] check(ref(AId x), _, UseDef useDef) = 
	{error("Undeclared question", x.src) | useDef[x.src] == {} };
set[Message] check(not(AExpr e), TEnv tenv, UseDef useDef) = 
	{warning("Boolean type expected", e.src) | typeOf(e, tenv, useDef) notin [tunknown(), tbool()] };
set[Message] check(mul(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer or money type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint(), tmoney()] };
set[Message] check(div(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer or money type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint(), tmoney()] };
set[Message] check(\mod(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint()] };
set[Message] check(add(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer or money type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint(), tmoney()] };
set[Message] check(sub(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer or money type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint(), tmoney()] };
set[Message] check(gt(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer or money type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint(), tmoney()] };
set[Message] check(gte(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer or money type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint(), tmoney()] };
set[Message] check(lt(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer or money type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint(), tmoney()] };
set[Message] check(lte(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Integer or money type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tint(), tmoney()] };
set[Message] check(eqs: equal(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) {
	Type let = typeOf(leftExpression, tenv, useDef);
	Type ret = typeOf(rightExpression, tenv, useDef);
	if (let == tunknown() || ret == tunknown()) {
		return {};
	}
	if (let in [tint(), tmoney()]) {
		if (ret notin [tint(), tmoney()]) {
			return {warning("Equal types expected", eqs.src)};
		}
	} else if (let != ret) {
		return {warning("Equal types expected", eqs.src)};
	}
	return {};
}
set[Message] check(eqs: notequal(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) {
	Type let = typeOf(leftExpression, tenv, useDef);
	Type ret = typeOf(rightExpression, tenv, useDef);
	if (let == tunknown() || ret == tunknown()) {
		return {};
	}
	if (let in [tint(), tmoney()]) {
		if (ret notin [tint(), tmoney()]) {
			return {warning("Equal types expected", eqs.src)};
		}
	} else if (let != ret) {
		return {warning("Equal types expected", eqs.src)};
	}
	return {};
}
set[Message] check(and(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Boolean type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tbool()] };
set[Message] check(or(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = 
	{warning("Boolean type expected", e.src) | e <- [leftExpression, rightExpression], typeOf(e, tenv, useDef) notin [tunknown(), tbool()] };
	
	
default set[Message] check(AExpr e, TEnv tenv, UseDef useDef) =
	{}; 

Type typeOfAType(booleanType()) = tbool();
Type typeOfAType(integerType()) = tint();
Type typeOfAType(moneyType()) = tmoney();
Type typeOfAType(stringType()) = tstr();
default Type typeOfAType(AExpr _) = tunknown();

Type typeOf(\int(_), _, _) = tint();
Type typeOf(money(_), _, _) = tmoney();
Type typeOf(\str(_), _, _) = tstr();
Type typeOf(\bool(_), _, _) = tbool();
Type typeOf(expr(AExpr e), TEnv tenv, UseDef useDef) = typeOf(e, tenv, useDef);

Type typeOf(ref(AId x), TEnv tenv, UseDef useDef) = t
	when <u, loc d> <- useDef, u := x.src, name := x.name, <d, name, _, Type t> <- tenv;
	
Type typeOf(not(AExpr e), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(e, tenv, useDef) == tbool();

Type typeOf(mul(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tint()
	when typeOf(leftExpression, tenv, useDef) == tint() 
	&& typeOf(rightExpression, tenv, useDef) == tint();
	
Type typeOf(mul(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tmoney()
	when typeOf(leftExpression, tenv, useDef) in [tint(), tmoney()]
	&& typeOf(rightExpression, tenv, useDef) in [tint(), tmoney()];
	
Type typeOf(div(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tint()
	when typeOf(leftExpression, tenv, useDef) == tint() 
	&& typeOf(rightExpression, tenv, useDef) == tint();
	
Type typeOf(div(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tmoney()
	when typeOf(leftExpression, tenv, useDef) in [tint(), tmoney()] 
	&& typeOf(rightExpression, tenv, useDef) in [tint(), tmoney()];
	
Type typeOf(\mod(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tint()
	when typeOf(leftExpression, tenv, useDef) == tint() 
	&& typeOf(rightExpression, tenv, useDef) == tint();
	
Type typeOf(add(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tint()
	when typeOf(leftExpression, tenv, useDef) == tint() 
	&& typeOf(rightExpression, tenv, useDef) == tint();
	
Type typeOf(add(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tmoney()
	when typeOf(leftExpression, tenv, useDef) in [tint(), tmoney()] 
	&& typeOf(rightExpression, tenv, useDef) in [tint(), tmoney()];
	
Type typeOf(sub(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tint()
	when typeOf(leftExpression, tenv, useDef) == tint() 
	&& typeOf(rightExpression, tenv, useDef) == tint();
	
Type typeOf(sub(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tmoney()
	when typeOf(leftExpression, tenv, useDef) in [tint(), tmoney()] 
	&& typeOf(rightExpression, tenv, useDef) in [tint(), tmoney()];
	
Type typeOf(gt(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(leftExpression, tenv, useDef) in [tmoney(), tint()] 
	&& typeOf(rightExpression, tenv, useDef) in [tmoney(), tint()];
	
Type typeOf(gte(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(leftExpression, tenv, useDef) in [tmoney(), tint()] 
	&& typeOf(rightExpression, tenv, useDef) in [tmoney(), tint()];
	
Type typeOf(lt(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(leftExpression, tenv, useDef) in [tmoney(), tint()] 
	&& typeOf(rightExpression, tenv, useDef) in [tmoney(), tint()];
	
Type typeOf(lte(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(leftExpression, tenv, useDef) in [tmoney(), tint()] 
	&& typeOf(rightExpression, tenv, useDef) in [tmoney(), tint()];
	
Type typeOf(equal(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(leftExpression, tenv, useDef) == typeOf(rightExpression, tenv, useDef)
	|| (typeOf(leftExpression, tenv, useDef) in [tmoney(), tint()] 
		&& typeOf(rightExpression, tenv, useDef) in [tmoney(), tint()] );
	
Type typeOf(notequal(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(leftExpression, tenv, useDef) == typeOf(rightExpression, tenv, useDef)
	|| (typeOf(leftExpression, tenv, useDef) in [tmoney(), tint()] 
		&& typeOf(rightExpression, tenv, useDef) in [tmoney(), tint()] );
	
Type typeOf(and(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(leftExpression, tenv, useDef) == tbool() 
	&& typeOf(rightExpression, tenv, useDef) == tbool();
	
Type typeOf(or(AExpr leftExpression, AExpr rightExpression), TEnv tenv, UseDef useDef) = tbool()
	when typeOf(leftExpression, tenv, useDef) == tbool() 
	&& typeOf(rightExpression, tenv, useDef) == tbool();

default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
