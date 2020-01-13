module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library
import List;
import Eval;

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node questions2html(AForm f) {
  list[value] questions = [];
  
  visit(f) {
    case question(str questionString, AId questionID, AType answerType, list[AExpr] answerExpressions): {    
      if(!isEmpty(answerExpressions)) {
        questions += div(
        	questionString[1..-1],
        	br(),
        	input(
        		id("question_<questionID.name>_<questionID.src.begin.line>_<questionID.src.begin.column>"),
        		oninput("console.log($(\'#question_<questionID.name>_<questionID.src.begin.line>_<questionID.src.begin.column>\').val())"),
        		\type("text")
        	)
        	
        );
      } else {
        questions += div(
        	questionString[1..-1],
        	br(),
        	output(
        	)
        	
        );
      }
    }
      
    case \if(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] elseQuestions):
      ;
  }
  
  return form(questions);
}

// This function is called every time a change is made to the myql file.
// Therefore after every change we have a new AForm. We then compile a new HTML file for this 
// new form. 
HTML5Node form2html(AForm f) {
  // Build html page
  HTML5Node htmlHead = head(title("Form title"));
  HTML5Node htmlBody = body(questions2html(f), script(src("https://code.jquery.com/jquery-3.4.1.js")));
  
  return html(htmlHead, htmlBody);
}

str form2js(AForm f) {
  return "";
}
