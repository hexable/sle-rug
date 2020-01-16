module IDE

import Syntax;
import AST;
import CST2AST;
import Resolve;
import IO;
import Check;
import Compile;
import Transform;

import util::IDE;
import util::Prompt;
import Message;
import ParseTree;


private str MyQL ="MyQL";

anno rel[loc, loc] Tree@hyperlinks;

void main() {
  registerLanguage(MyQL, "myql", Tree(str src, loc l) {
    return parse(#start[Form], src, l);
  });
  
  contribs = {
    annotator(Tree(Tree t) {
      if (start[Form] pt := t) {
        AForm ast = cst2ast(pt);
        UseDef useDef = resolve(ast).useDef;
        set[Message] msgs = check(ast, collect(ast), useDef);
        return t[@messages=msgs][@hyperlinks=useDef];
      }
      return t[@messages={error("Not a form", t@\loc)}];
    }),
    
    builder(set[Message] (Tree t) {
      if (start[Form] pt := t) {
        AForm ast = cst2ast(pt);
        UseDef useDef = resolve(ast).useDef;
        set[Message] msgs = check(ast, collect(ast), useDef);
        if (msgs == {}) {
          compile(ast);
        }
        return msgs;
      }
      return {error("Not a form", t@\loc)};
    }),
    popup(
    	edit("Rename...", str (Tree t, loc selection) {
    		if (start[Form] pt := t) {
    			loc fullLoc = getFullLoc(pt, selection);
    			if (fullLoc == |tmp:///|) {
    				alert("You have not selected a valid id.");
    				return;
    			}
	    		newName = prompt("Enter new name: ");
	    		t = rename(pt, fullLoc, newName, resolve(cst2ast(pt)));
    		}
    		return "<t>";
    	})
    )
  };

  registerContributions(MyQL, contribs);
}
