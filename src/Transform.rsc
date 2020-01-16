module Transform

import Syntax;
import Resolve;
import AST;
import List;
import IO;
import ParseTree;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
alias QuestionPerIf = lrel[list[AExpr] ifConditions, AQuestion question]; 

AForm flatten(AForm f) {
	f.questions = [
		\if((\bool(true) | and(it, condition) | condition <- ifConditions), question, [], src=question.src)
		| <ifConditions, question> <- getIfCond([], f.questions)
	];
  	return f; 
}

QuestionPerIf getIfCond(list[AExpr] currentIfConditions, list[AQuestion] questions) = ([] | it + getIfCond(currentIfConditions, q) | q <- questions);
QuestionPerIf getIfCond(list[AExpr] currentIfConditions, block(list[AQuestion] questions)) = getIfCond(currentIfConditions, questions);
QuestionPerIf getIfCond(list[AExpr] currentIfConditions, q: question(_, _, _, _)) = [<currentIfConditions, q>];
QuestionPerIf getIfCond(list[AExpr] currentIfConditions, \if(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] ifElseQuestion))
	= getIfCond(currentIfConditions + ifCondition, ifTrueQuestion) 
	+ [
		*getIfCond(currentIfConditions + not(ifCondition), q)
		| q <- ifElseQuestion
	];

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
loc getFullLoc(start[Form] f, loc selection) {
	if (treeFound(Id idx) := treeAt(#Id, selection, f)) {
		return idx@\loc;
	}
	return |tmp:///|;
}

start[Form] rename(start[Form] f, loc useOrDef, str newName, RefGraph refGraph) {
	Id newId = [Id]newName;
	loc def = useOrDef;
	if (<useOrDef, loc defx> <- refGraph.useDef) {
		def = defx;
	}
	if (<str oldName, def> <- refGraph.defs ) {
	  	return visit(f) {
	  		case (Expr)`<Id idx>` => (Expr)`<Id newId>`
	  			when 
	  				<loc use, def> <- refGraph.useDef,
	  				use == idx@\loc
	  		
	  		case (Question)`<Str qstr> <Id idx> : <Type qtype>` 
	  			=> (Question)`<Str qstr>
	  				'	<Id newId>: <Type qtype>
	  				'
	  				'	`
	  				when
	  					curLoc := idx@\loc,
	  					<str name, curLoc> <- refGraph.defs,
	  					name == oldName
	  		case (Question)`<Str qstr> <Id idx> : <Type qtype> = <Expr qexpr>` 
	  			=> (Question)`<Str qstr> 
	  				'	<Id newId>: <Type qtype> = <Expr qexpr>
	  				'
	  				'	`
	  				when
	  					curLoc := idx@\loc,
	  					<str name, curLoc> <- refGraph.defs,
	  					name == oldName
	  	}
  	}
  	
  	return f;
}