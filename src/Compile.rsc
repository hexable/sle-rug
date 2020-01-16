module Compile

import AST;
import Exception;
import String;
import util::Resources;
import IO;
import Resolve;
import lang::html5::DOM; // see standard library

void compile(AForm f) {
	writeFile(f.src[extension="js"].top, form2js(f));
	writeFile(f.src[extension="html"].top, "\<!doctype html\>" + toString(form2html(f)));
}

HTML5Node form2html(AForm f) 
	= html(
	  	lang("en"),
	  	head(
	  		// Some mark up stuff including the title
	  		meta(charset("utf-8")),
	  		meta(name("viewport"), content("width=device-width, initial-scale=1, shrink-to-fit=no")),
	  		link(\rel("stylesheet"), \type("text/css"), href("https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css")),
	  		title(f.name)
	  	),
	  	body(
	  		// The ql form
	  		div([
	  			id("input_form_" + f.name),
	  			class("container"),
	  			*questions2html(f.questions)
	  		]),
	  		// A shadow form that contains the actual values of the form
	  		form([
	  			id("form_" + f.name),
	  			action("/"),
	  			method("post"),
	  			class("container"),
	  			*initializeQuestionValues(f)
	  		]),
	  		// Additional scripts to be loaded at the end of the file
	  		script(src("https://code.jquery.com/jquery-3.4.1.js")),
	  		script(src("https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.js")),
	  		script(src("https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.js")),
	  		script(src("https://cdn.jsdelivr.net/npm/inputmask@5.0.1/dist/jquery.inputmask.js")),
	  		script(src(location(f.src[extension="js"]).uri))
  		)
  	);


// Gets the inputs for the shadow form 
list[HTML5Node] initializeQuestionValues(AForm f) = [
	input(
		id(aidName),
		name(aidName),
		\type("hidden"),
		\value(initialValueForType(atype))
	)
	| /question(_, id(aidName), AType atype, _) := f , aidName <- resolve(f).defs.name];

// Transforms AQuestions to a list of html elements
list[HTML5Node] questions2html(list[AQuestion] questions) 
	= [q | question <- questions, HTML5Node q <- question2html(question)];
list[HTML5Node] question2html(block(list[AQuestion] questions)) 
	= questions2html(questions);
list[HTML5Node] question2html(question: question(str questionString, AId aid, _, _))
	= [
		label(
			\for("question_<createId(aid)>"),
			questionString
		),
		question2htmlInput(question),
		br()
	];
list[HTML5Node] question2html(ifq: \if(_, AQuestion ifTrueQuestion, list[AQuestion] ifElseQuestion)) 
	= [
		div([
			class("form-group"),
			id("if_<ifq.src.begin.line>_<ifq.src.begin.column>"),
			html5attr("style", "display:none;"),
			*question2html(ifTrueQuestion)
		]),
		*[
			// Add and else option if the else question has been set
			div([
				class("form-group"),
				id("else_<ifq.src.begin.line>_<ifq.src.begin.column>"),
				html5attr("style", "display:none;"),
				*question2html(q)
			])
			| AQuestion q <- ifElseQuestion
		]
	];
default list[HTML5Node] question2html(_) {
	assert false:"Could not compile form";
	throw AssertionFailed();
}

/* The function below transforms the diverent types into html inputs
 * This includes some formating and additional divs with classes
 * The questions witout expresion can change and have a onchange (or similar) function to update
 * the shadow form
 * The ones with expression are readonly or disabled and can only be changed by javascript
 */
HTML5Node question2htmlInput(question(_,AId aid, booleanType(), [])) 
	= div(
		class("form-group"),
		div(
			class("form-check form-check-inline"),
			label(
				class("input-group-prepend form-check-label mb-0"),
				\for("question_<createId(aid)>_yes"),
				div(
					class("input-group-text"),
					input(
						class("form-check-input"),
						name("question_<createId(aid)>"),
						\value("yes"),
						\type("radio"),
						onchange("changeVal(\'<createId(aid)>\', \'<aid.name>\', true);"),
						id("question_<createId(aid)>_yes")
					),
					"Yes"
				)
			),
			label(
				class("input-group-append form-check-label mb-0"),
				\for("question_<createId(aid)>_no"),
				div(
					class("input-group-text"),
					input(
						class("form-check-input"),
						name("question_<createId(aid)>"),
						\value("no"),
						\type("radio"),
						checked(true),
						onchange("changeVal(\'<createId(aid)>\', \'<aid.name>\', false);"),
						id("question_<createId(aid)>_no")
					),
					"No"
				)
			)
		)
	);
HTML5Node question2htmlInput(question(_,AId aid, booleanType(), list[AExpr] expresion)) 
	= div(
		class("form-group"),
		div(
			class("form-check form-check-inline"),
			label(
				class("input-group-prepend form-check-label mb-0"),
				\for("question_<createId(aid)>_yes"),
				div(
					class("input-group-text"),
					input(
						class("form-check-input"),
						name("question_<createId(aid)>"),
						\value("yes"),
						\type("radio"),
						disabled(true),
						onchange("changeVal(\'<createId(aid)>\', \'<aid.name>\', true);"),
						id("question_<createId(aid)>_yes")
					),
					"Yes"
				)
			),
			label(
				class("input-group-append form-check-label mb-0"),
				\for("question_<createId(aid)>_no"),
				div(
					class("input-group-text"),
					input(
						class("form-check-input"),
						name("question_<createId(aid)>"),
						\value("no"),
						\type("radio"),
						disabled(true),
						checked(true),
						id("question_<createId(aid)>_no")
					),
					"No"
				)
			)
		)
	);
HTML5Node question2htmlInput(question(_,AId aid, stringType(), []))
	= input(
		\type("text"),
		class("form-control"),
		name("question_<createId(aid)>"),
		id("question_<createId(aid)>"),
		oninput("changeVal(\'<createId(aid)>\', \'<aid.name>\', \'\\\'\' + $(\'#question_<createId(aid)>\').val() + \'\\\'\')")
	);
HTML5Node question2htmlInput(question(_,AId aid, stringType(), list[AExpr] expresion))
	= input(
		\type("text"),
		class("form-control"),
		name("question_<createId(aid)>"),
		id("question_<createId(aid)>"),
		readonly("true")
	);
HTML5Node question2htmlInput(question(_, AId aid, integerType(), [])) 
	= input(
		\type("text"),
		class("form-control integer"),
		name("question_<createId(aid)>"),
		id("question_<createId(aid)>"),
		oninput("changeVal(\'<createId(aid)>\', \'<aid.name>\', parseInt($(\'#question_<createId(aid)>\').val()))")
	);
HTML5Node question2htmlInput(question(_, AId aid, integerType(), list[AExpr] expresion)) 
	= input(
		\type("text"),
		class("form-control integer"),
		name("question_<createId(aid)>"),
		id("question_<createId(aid)>"),
		readonly("true")
	);
HTML5Node question2htmlInput(question(_, AId aid, moneyType(), [])) 
	= div(
		class("input-group"),
		div(
			class("input-group-prepend"),
			div(
				class("input-group-text"),
				"€"
			)
		),
		input(
			\type("text"),
			class("form-control currency"),
			name("question_<createId(aid)>"),
			id("question_<createId(aid)>"),
			oninput("changeVal(\'<createId(aid)>\', \'<aid.name>\', parseFloat($(\'#question_<createId(aid)>\').val().replace(/,/g,\'\')));")
		)
		
	);
HTML5Node question2htmlInput(question(_, AId aid, moneyType(), list[AExpr] expresion)) 
	= div(
		class("input-group"),
		div(
			class("input-group-prepend"),
			div(
				class("input-group-text"),
				"€"
			)
		),
		input(
			\type("text"),
			class("form-control currency"),
			name("question_<createId(aid)>"),
			id("question_<createId(aid)>"),
			readonly("true")
		)
		
	);
default HTML5Node question2htmlInput(_) {
	assert false: "Could not compile form";
	throw AssertionFailed();
}

// This create a unique identifier for a given AId as long as one file is used
str createId(AId aid) = "<aid.name>_<aid.src.begin.line>_<aid.src.begin.column>";

/*
 * ------------------------------------------------------------------------------------------------- 
 * Every thing below here is for javascript
 * ------------------------------------------------------------------------------------------------- 
 */
/* This is the main javascript function. It retrives javascript from other functions
 * as well as defining some helper javascript function and setting masks on inputs
 * The helper function "inputToValue(name)" retrives the value of the shadow form and
 * transforms it to a value of its types as values from the shadow form are always a string
 * The "changeVal(origin, name, val)" function changes the value for the given name in the
 * shadow form and updates the entire form
 */
str form2js(AForm f) 
	= "
	'<jsQuestionMapping(f)>
	'function updateInputForm() {
	'	<question2js(f.questions)>
	'}
	'function inputToValue(name) {
	'	var value = $(\"#\" + name).val();
	'	if (value === \"true\") {
	'		return true;
	'	} else if (value === \"false\") {
	'		return false;
	'	} else if (value[0] === \"\'\") {
	'		return value;
	'	} else if (value.indexOf(\".\") !== -1) {
	'		return parseFloat(value);
	'	} else {
	'		if (Number.isNaN(value)) {
	'			return 0;
	'		}
	'		return parseInt(value);
	'	}
	'}
	'function changeVal(origin, name, val){
	'	if (typeof val === \"number\") {
	'		if (!Number.isFinite(val)) {
	'			val = 0;
	'		} else if (!Number.isNaN(val) && !Number.isInteger(val)) {
	'			val = +val.toFixed(2);
	'		}
	'	}
	'	if (inputToValue(name) !== val) {
	'		$(\"#\" + name).val(val);
	'		questionMapping[name].forEach(function(item){
	'			if (item === origin) {
	'				return;
	'			}
	'			if (val === true) {
	'				$(\"#question_\" + item + \"_yes\").prop(\"checked\", true);
	'			} else if (val === false) {
	'				$(\"#question_\" + item + \"_no\").prop(\"checked\", true);
	'			} else if (val[0] === \"\'\") {
	'				$(\"#question_\" + item).val(val.substring(1, val.length - 1));
	'			} else {
	'				$(\"#question_\" + item).val(val);
	'			}
	'		});
	'		updateInputForm();
	'	}
	'};
	'$(document).ready(function(){
	'	$(\".currency\").inputmask(\"currency\",{digitsOptional:false});
	'	$(\".integer\").inputmask(\"integer\");
	'	updateInputForm();
	'});";


// This is a lookup map to change all occurances of a name in the QL form
str jsQuestionMapping(AForm f) 
	= "questionMapping = {<for (aidName<-{aid.name | /question(_, AId aid, _, _)<-f}) {>
	'	\"<aidName>\": [<for (/question(_,qaid: id(aidName), _, list[AExpr] expresion)<-f) {>
	'		\"<createId(qaid)>\",<}>
	'	],<}>
	'}";


/* Transform AQuestion to javascript
 * This includes if questions that show and hide if/else parts of the form
 */
str question2js(list[AQuestion] qs) {
	retval = "";
	for (q <- qs) {
		js = question2js(q);
		// We might get empty javascript back, to keep nice formating we skip these lines 
		if (js == "") {
			continue;
		}
		// Add the javascript with a new line to keep formating
		retval += js + "\n";
	}
	return retval;
}
str question2js(block(list[AQuestion] qs)) = question2js(qs);
str question2js(question(_, _, _, [])) = "";
str question2js(question(_, AId aid, _, list[AExpr] expresion))
 	= "changeVal(null, \"<aid.name>\", <expression2js(expresion[0])>);";
str question2js(ifq: \if(AExpr ifCondition, AQuestion ifTrueQuestion, [])) 
	= "if (<expression2js(ifCondition)>) {
	'	$(\"#if_<ifq.src.begin.line>_<ifq.src.begin.column>\").show();
	'	<question2js(ifTrueQuestion)>
	'} else {
	'	$(\"#if_<ifq.src.begin.line>_<ifq.src.begin.column>\").hide();
	'}";
str question2js(ifq: \if(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] ifElseQuestion))
 	= "if (<expression2js(ifCondition)>) {
	'	$(\"#if_<ifq.src.begin.line>_<ifq.src.begin.column>\").show();
	'	$(\"#else_<ifq.src.begin.line>_<ifq.src.begin.column>\").hide();
	'	<question2js(ifTrueQuestion)>
	'} else {
	'	$(\"#if_<ifq.src.begin.line>_<ifq.src.begin.column>\").hide();
	'	$(\"#else_<ifq.src.begin.line>_<ifq.src.begin.column>\").show();
	'	<question2js(ifElseQuestion[0])>
	'}";
default str question2js(_) {
	assert false : "question not known";
	throw AssertionFailed();
}

/* This function takes in a AExpr and returns an expression that can be parsed by javascript
 * Its almost a 1 to 1 transformation except for the reverence, this first need to be retrived and
 * converted to a scalar type of what we want
 */
str expression2js(ref(AId x)) = "inputToValue(\"<x.name>\")";
str expression2js(\int(int intValue)) = "<intValue>";
str expression2js(\str(str stringValue)) = "\"\'<stringValue>\'\"";
str expression2js(\bool(bool boolean)) = "<boolean>";
str expression2js(money(real money)) = "<money>";
str expression2js(expr(AExpr expression)) = "(<expression2js(expression)>)";
str expression2js(not(AExpr expression)) = "!<expression2js(expression)>";
str expression2js(mul(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> * <expression2js(rightExpression)>";
str expression2js(div(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> / <expression2js(rightExpression)>";
str expression2js(\mod(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> % <expression2js(rightExpression)>";
str expression2js(add(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> + <expression2js(rightExpression)>";
str expression2js(sub(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> - <expression2js(rightExpression)>";
str expression2js(gt(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> \> <expression2js(rightExpression)>";
str expression2js(gte(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> \>= <expression2js(rightExpression)>";
str expression2js(lt(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> \< <expression2js(rightExpression)>";
str expression2js(lte(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> \<= <expression2js(rightExpression)>";
str expression2js(equal(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> == <expression2js(rightExpression)>";
str expression2js(notequal(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> != <expression2js(rightExpression)>";
str expression2js(and(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> && <expression2js(rightExpression)>";
str expression2js(or(AExpr leftExpression, AExpr rightExpression)) 
	= "<expression2js(leftExpression)> || <expression2js(rightExpression)>";
default str expression2js(_) {
	assert false : "Type not known";
	throw AssertionFailed();
}

// Sets the initial values like we do in eval
str initialValueForType(booleanType()) = "false";
str initialValueForType(integerType()) = "0";
str initialValueForType(moneyType()) = "0.00";
str initialValueForType(stringType()) = "\'\'";
default str initialValueForType(_) {
	assert false : "Type not known";
	throw AssertionFailed();
}