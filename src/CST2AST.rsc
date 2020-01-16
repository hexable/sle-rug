module CST2AST

import Syntax;
import AST;
import ParseTree;
import String;
import Boolean;

AForm cst2ast(start[Form] sf) 
	= form("<formId>", [cst2ast(q) | q <- questions], src=sf.top@\loc)
	when (Form) `form <Id formId> { <Question* questions> }` := sf.top;
default AForm cst2ast(start[Form] sf) {
	throw "Not yet implemented form: <sf.top>";
}

AQuestion cst2ast(q: (Question) `<Str questionString> <Id assignmentId> : <Type assignmentType> = <Expr assignmentExpression>`)
	= question("<questionString>"[1..-1],cst2ast(assignmentId),cst2ast(assignmentType), [cst2ast(assignmentExpression)], src=q@\loc);
AQuestion cst2ast(q: (Question) `<Str questionString> <Id assignmentId> : <Type assignmentType>`)
	= question("<questionString>"[1..-1],cst2ast(assignmentId),cst2ast(assignmentType), [], src=q@\loc);
AQuestion cst2ast(q: (Question) `if ( <Expr ifCondition> ) <Question ifTrueQuestion> else <Question ifElseQuestion>`)
	= \if(cst2ast(ifCondition), cst2ast(ifTrueQuestion), [cst2ast(ifElseQuestion)], src=q@\loc);
AQuestion cst2ast(q: (Question) `if ( <Expr ifCondition> ) <Question ifTrueQuestion>`)
	= \if(cst2ast(ifCondition), cst2ast(ifTrueQuestion), [], src=q@\loc);
AQuestion cst2ast(q: (Question) `{ <Question* qqs> }`)
	= block([cst2ast(qq) | qq <- qqs], src=q@\loc);
default AQuestion cst2ast(Question q) {
	throw "Not yet implemented question: <q>";
}

AExpr cst2ast((Expr)`<Id x>`) = ref(cst2ast(x), src=x@\loc);
AExpr cst2ast((Expr)`<Int i>`) = \int(toInt("<i>"), src=i@\loc);
AExpr cst2ast((Expr)`<Money m>`) = money(toReal("<m>"), src=m@\loc);
AExpr cst2ast((Expr)`<Str s>`) = \str("<s>"[1..-1], src=s@\loc);
AExpr cst2ast((Expr)`<Bool b>`) = \bool(fromString("<b>"), src=b@\loc);
AExpr cst2ast(e: (Expr)`( <Expr exp> )`) = expr(cst2ast(exp), src=e@\loc);
AExpr cst2ast(e: (Expr)`! <Expr exp>`) = not(cst2ast(exp), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> * <Expr rightExpression>`)
	= mul(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> / <Expr rightExpression>`)
	= div(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> % <Expr rightExpression>`)
	= \mod(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> + <Expr rightExpression>`)
	= add(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> - <Expr rightExpression>`)
	= sub(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> \< <Expr rightExpression>`)
	= lt(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> \<= <Expr rightExpression>`)
	= lte(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> \> <Expr rightExpression>`)
	= gt(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> \>= <Expr rightExpression>`)
	= gte(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> == <Expr rightExpression>`)
	= equal(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> != <Expr rightExpression>`)
	= notequal(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> && <Expr rightExpression>`)
	= and(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
AExpr cst2ast(e: (Expr)`<Expr leftExpression> || <Expr rightExpression>`)
	= or(cst2ast(leftExpression), cst2ast(rightExpression), src=e@\loc);
default AExpr cst2ast(Expr e) {
	throw "Not yet implemented expression <e>";
}

AType cst2ast(t: (Type) `boolean`) = booleanType(src=t@\loc);
AType cst2ast(t: (Type) `integer`) = integerType(src=t@\loc);
AType cst2ast(t: (Type) `money`) = moneyType(src=t@\loc);
AType cst2ast(t: (Type) `string`) = stringType(src=t@\loc);
default AType cst2ast(Type t) {
	throw "Not yet implemented type: <t>";
}

AId cst2ast(Id x) = id("<x>", src=x@\loc);